
# Calculate final result (?)

library(tidyverse)
library(decisionSupport)

read.csv("input_variable_estimates.csv")


model_function <- function(){

  # Estimate the income of rice in a normal season
  rice_income <- Paddy_yield * Market_price_rice
  
  # Estimate the income of soybean in a normal season
  soybean_income <- Soybean_yield * Market_price_soybean
  
  # Estimate the income of chili in a normal season
  chili_income <- Chili_yield * Market_price_chili
  
  #Estimate the cost of rice farm in a normal season
  rice_cost <- sum(Cost_of_labor, Cost_of_rice_seeds, cost_of_pesticides,
                   cost_of_fertilizer, cost_of_machinery, cost_of_harvesting)
  
  #Estimate the cost of soybean farm in a normal season
  soybean_cost <- sum(Cost_of_labor, cost_of_soybean_seeds, cost_of_pesticides,
                      cost_of_fertilizer, cost_of_machinery, cost_of_harvesting)
  
  #Estimate the cost of chili farm in a normal season
  chili_cost <- sum(Cost_of_labor, cost_of_chili_seeds, cost_of_pesticides,
                      cost_of_fertilizer, cost_of_machinery, cost_of_harvesting)
  
  
  # Estimate the final results from the model
  final_result_rice <- rice_income - rice_cost
  
  final_result_soybean <- soybean_income - soybean_cost
  
  final_result_chili <- chili_income - chili_cost
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result_rice + final_result_soybean + final_result_chili))
}
  

# Run the Monte Carlo simulation using the model function

input_estimates<-read.csv("input_variable_estimates.csv")

example_mc_simulation <- mcSimulation(estimate = as.estimate(input_variable_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 1000,
                                      functionSyntax = "plainNames")

example_mc_simulation


