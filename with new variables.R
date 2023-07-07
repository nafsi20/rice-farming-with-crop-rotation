library(tidyverse)
library(decisionSupport)

read.csv("new_variable_estimates.csv", sep=";")


crop_rotation_decision <- function(){
  
  # Estimate the income of rice in a normal season
  rice_income <- rice_yield * rice_price
  
  # Estimate the income of soybean in a normal season
  soybean_income <- soybean_yield * soybean_price
  
  # Estimate the income of chili in a normal season
  chili_income <- chili_yield * chili_price
  
  #Estimate the cost of rice farm in a normal season
  rice_cost <- sum(rice_land_rental_cost, rice_seeds_cost, rice_fertilizer_cost,
                   rice_pesticide_cost, rice_machinery_cost, rice_harvesting_cost)
    
    
  #Estimate the cost of soybean farm in a normal season
  soybean_cost <- sum(soybean_land_rental_cost, soybean_seeds_cost, soybean_fertilizer_cost,
                      soybean_pesticide_cost, soybean_machinery_cost, soybean_harvesting_cost)
  
  
  #Estimate the cost of chili farm in a normal season
  chili_cost <- sum(chili_land_rental_cost, chili_seeds_cost, chili_fertilizer_cost,
                    chili_pesticide_cost, chili_machinery_cost, chili_harvesting_cost)
  
  
  
  # Estimate the final results from the model
  final_result_rice <- rice_income - rice_cost
  
  final_result_soybean <- soybean_income - soybean_cost
  
  final_result_chili <- chili_income - chili_cost
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result_rice + final_result_soybean + final_result_chili))
}



# Run the Monte Carlo simulation using the model function

input_estimates <-read.csv("new_variable_estimates.csv", sep=";")

crop_rotation_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = crop_rotation_decision,
                                      numberOfModelRuns = 500,
                                      functionSyntax = "plainNames")




mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("new_variable_estimates.csv", sep=";"),
  model_function = crop_rotation_decision,
  numberOfModelRuns = 500,
  functionSyntax = "plainNames"
)




library(ggplot2)

plot_distributions(mcSimulation_object = crop_rotation_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")


make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}


make_variables(as.estimate(input_estimates))




