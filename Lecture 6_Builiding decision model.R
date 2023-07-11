# Lecture 6: Building decision model

library(decisionSupport)

example_decision_inputs <- read.csv("example_decision_inputs.csv")

example_decision_model <- function(x, varnames){
  profit <- benefits-costs
  
  final_profits <- profit + 500
  
  return(final_profits)
  
}

mcSimulation(estimate = as.estimate(example_decision_inputs),
             model_function = example_decision_model,
             numberOfModelRuns = 100,
             functionSyntax = "plainNames")
