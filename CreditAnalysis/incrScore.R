#' to Calculate the incremental scores adding a variable can bring
#' 
#' @param fullFormul full formula
#' @param var the variable we inspect
#' @param data data
#' @parma quantil quantiles of score incremental the variable brings

incrScore = function(fullFormul, var, data, quantil = c(0.05, 0.25, 0.5, 0.75, 0.95))
{
  fullF = fullFormul
  reducedF = reformulate(
    attr(terms(fullF), "term.labels")[attr(terms(fullF), "term.labels") != var], fullF[[2]])
  
  redM = coxph(reducedF, data = data, model = TRUE)
  fulM = coxph(fullF, data = data, model = TRUE)
  incrS = exp(predict(fulM, data)) - exp(predict(redM, data))
  quantile(incrS, probs = quantil)
}