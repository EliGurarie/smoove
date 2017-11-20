#' A function to strip model fits of all but coefficient estimates
#'
#' This function was taken from muschellij2/ms_seg, it strips fit objects from R of all but coefficient estimates
#' @param model A fit object to be stripped
#' @keywords fit
#' @export
strip.model = function(model){
  model$y = c()
  model$model = c()
  model$residuals = c()
  model$fitted.values = c()
  model$effects = c()
  model$qr$qr = c()
  model$linear.predictors = c()
  model$weights = c()
  model$prior.weights = c()
  model$data = c()
  attr(model$terms,".Environment") = c()
  attr(model$formula,".Environment") = c()
  model
}