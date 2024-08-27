#' Wrapper for the TabNet ParamSet
#'
#' Used to de-duplicate learner setup.
#' @return Object of classes `ParamSet" "R6"`, suitable for use with
#' @keywords internal
#'
params_tabnet = function() {
  param_set = ps(
    num_threads = p_int(default = 1L, lower = 1L, upper = Inf, tags = c("train", "threads")),
    batch_size = p_int(default = 256L, lower = 1L, upper = Inf, tags = "train"),
    penalty = p_dbl(default = 0.001, tags = "train"),
    
    # FIXME: NULL here is used for bool FALSE, not sure what to do there.
    clip_value = p_uty(default = NULL, tags = "train"),
    loss = p_fct(default = "auto", levels = c("auto", "mse", "cross_entropy"), tags = "train"),
    epochs = p_int(default = 5L, lower = 1L, upper = Inf, tags = "train"),
    drop_last = p_lgl(default = FALSE, tags = "train"),
    decision_width = p_int(default = 8L, lower = 1L, upper = Inf, tags = "train"),
    attention_width = p_int(default = 8L, lower = 1L, upper = Inf, tags = "train"),
    num_steps = p_int(default = 3L, lower = 1L, upper = Inf, tags = "train"),
    feature_reusage = p_dbl(default = 1.3, lower = 0, upper = Inf, tags = "train"),
    mask_type = p_fct(default = "sparsemax", levels = c("sparsemax", "entmax"), tags = "train"),
    virtual_batch_size = p_int(default = 128L, lower = 1L, upper = Inf, tags = "train"),
    valid_split = p_dbl(default = 0, lower = 0, upper = 1, tags = "train"),
    learn_rate = p_dbl(default = 0.02, lower = 0, upper = 1, tags = "train"),
    
    # FIXME: Currently either 'adam' or arbitrary optimizer function according to docs
    optimizer = p_uty(default = "adam", tags = "train"),
    
    # FIXME: This is either NULL or a function or explicit "steps", needs custom_check fun
    lr_scheduler = p_uty(default = NULL, tags = "train"),
    
    lr_decay = p_dbl(default = 0.1, lower = 0, upper = 1, tags = "train"),
    step_size = p_int(default = 30L, lower = 1L, upper = Inf, tags = "train"),
    checkpoint_epochs = p_int(default = 10L, lower = 1L, upper = Inf, tags = "train"),
    cat_emb_dim = p_int(default = 1L, lower = 0L, upper = Inf, tags = "train"),
    num_independent = p_int(default = 2L, lower = 0, upper = Inf, tags = "train"),
    num_shared = p_int(default = 2L, lower = 0, upper = Inf, tags = "train"),
    momentum = p_dbl(default = 0.02, lower = 0, upper = 1, tags = "train"),
    pretraining_ratio = p_dbl(default = 0.5, lower = 0, upper = 1, tags = "train"),
    verbose = p_lgl(default = FALSE, tags = "train"),
    device = p_fct(default = "auto", levels = c("auto", "cpu", "cuda"), tags = "train"),
    importance_sample_size = p_int(lower = 0, upper = 1e5, special_vals = list(NULL), tags = "train")
  )
  
  # Set param values that differ from default in tabnet_fit
  param_set$values = list(
    num_threads = 1L,
    # clip_value = NULL,
    decision_width = 8L,
    attention_width = 8L
  )
  return(param_set)
}

#' @title Regression TabNet Learner
#' @author Lukas Burk
#' @name mlr_learners_regr.tabnet
#'
#' @template class_learner
#' @templateVar id regr.tabnet
#' @templateVar caller tabnet
#' @references
#' `r format_bib("arik2021tabnet")`
#'
#' @template seealso_learner
#' @export
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3torch)
#'
#' task = tsk("boston_housing")
#'
#' # Creating a learner & training on example task
#' lrn = lrn("regr.tabnet")
#'
#' lrn$param_set$values$epochs = 10
#' lrn$train(task)
#'
#' # Predict on training data, get RMSE
#' predictions = lrn$predict(task)
#' predictions$score(msr("regr.rmse"))
#' }
LearnerRegrTabNet = R6::R6Class(
  "LearnerRegrTabnet",
  inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = params_tabnet()
      
      super$initialize(
        id = "regr.tabnet",
        packages = "tabnet",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        param_set = ps,
        properties = c("importance"),
        man = "mlr3torch::mlr_learners_regr.tabnet",
        label = "Attentive Interpretable Tabular Network"
      )
    },
    
    #' @description
    #' The importance scores are extracted from the slot `.$model$fit$importances`.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      imp = self$model$fit$importances
      sort(stats::setNames(imp$importance, imp$variables),
           decreasing = TRUE)
    }
  ),
  private = list(
    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")
      pars_threads = pars$num_threads
      pars$num_threads = NULL
      
      # Set number of threads
      torch::torch_set_num_threads(pars_threads)
      
      # set column names to ensure consistency in fit and predict
      self$state$feature_names = task$feature_names
      
      # use the mlr3misc::invoke function (it's similar to do.call())
      mlr3misc::invoke(
        tabnet::tabnet_fit,
        x = task$data(cols = task$feature_names),
        y = task$data(cols = task$target_names),
        .args = pars
      )
    },
    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")
      
      # get newdata and ensure same ordering in train and predict
      newdata = task$data(cols = self$state$feature_names)
      
      pred = mlr3misc::invoke(predict, self$model,
                              new_data = newdata,
                              .args = pars)
      
      list(response = pred[[".pred"]])
    }
  )
)
