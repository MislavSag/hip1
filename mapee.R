#' @title Mean Absolute Directional Loss (MADL) https://arxiv.org/pdf/2309.10546
#'
#' @description
#' This measure implements the Mean Absolute Directional Loss (MADL) for evaluating forecasting models
#' in algorithmic trading strategies. For each observation, it computes:
#'
#' \deqn{\text{loss}_i = -\operatorname{sign}(R_i \times \hat{R}_i) \times |R_i|,}
#'
#' and returns the mean loss over all observations.
#'
#' Lower values are better since the loss function is designed to be minimized.
#'
#' @format R6 class inheriting from mlr3::MeasureRegr.
#' @export
MapeWithEpsilon = R6::R6Class(
  "MeanAbsoluteDirectionalLoss",
  inherit = mlr3::MeasureRegr,
  public = list(
    
    #' @description
    #' Initialize the MADL measure.
    initialize = function() {
      super$initialize(
        id = "mape_with_epsilon",
        predict_type = "response",
        minimize = TRUE,
        range = c(-Inf, Inf)
      )
    }
  ),
  
  private = list(
    
    # The main scoring function
    .score = function(prediction, ...) {
      
      mape_with_epsilon <- function(truth, 
                                    response, 
                                    sample_weights = NULL, 
                                    epsilon = 1e-8, 
                                    TOL = 1e-8, ...) {
        # Adjust the denominator: if close to zero, use epsilon.
        denom <- ifelse(abs(truth) < TOL, epsilon, truth)
        
        # Compute with the adjusted denominator.
        ape <- abs((truth - response) / denom)
        
        # Compute and return.
        return(mlr3measures:::wmean(ape, sample_weights))
      }
      mape_with_epsilon(prediction$truth, prediction$response)
    }
  )
)
