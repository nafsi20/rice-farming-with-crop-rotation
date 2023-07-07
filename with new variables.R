library(tidyverse)
library(decisionSupport)
library(ggplot2)

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
  
  # Estimate the profit
  rice_profit <- vv(rice_income - rice_cost,n=n_year,var_CV=100)
  soybean_profit <- vv(soybean_income - soybean_cost,n=n_year,var_CV=100)
  chili_profit <- vv(chili_income - chili_cost,n=n_year,var_CV=100)
  
  # Final
  crop_rotation_result = rice_profit + soybean_profit + chili_profit
  rice_cultivation_result = rice_profit*3
  
  # NPV
  NPV_total <- discount(crop_rotation_result, discount_rate, calculate_NPV = TRUE)
  NPV_rice <- discount(rice_cultivation_result, discount_rate, calculate_NPV = TRUE)
  NPV_total_income <- discount(rice_income + soybean_income + chili_income, discount_rate, calculate_NPV = TRUE)
  NPV_rice_income <- discount(rice_income, discount_rate, calculate_NPV = TRUE)
  NPV_total_cost <- discount(rice_cost + soybean_cost + chili_cost, discount_rate, calculate_NPV = TRUE)
  NPV_rice_cost <- discount(rice_cost, discount_rate, calculate_NPV = TRUE)
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(NPV_total=NPV_total, NPV_rice=NPV_rice,
              NPV_total_income=NPV_total_income,NPV_rice_income=NPV_rice_income,
              NPV_total_cost=NPV_total_cost,NPV_rice_cost=NPV_rice_cost))
}

# Run the Monte Carlo simulation using the model function

input_estimates <-read.csv("new_variable_estimates.csv", sep=";")

str(input_estimates)

crop_rotation_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                            model_function = crop_rotation_decision,
                                            numberOfModelRuns = 500,
                                            functionSyntax = "plainNames")

crop_rotation_mc_simulation


# plot NPV distribution analysis

decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_total","NPV_rice"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_total_income","NPV_rice_income"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_total_cost","NPV_rice_cost"),
                                    method = 'smooth_simple_overlay')

# cashflow analysis

plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "crop_rotation_result")
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "rice_cultivation_result")


# Projection to Latent Structures (PLS) analysis


pls_result <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                resultName = "NPV_total", ncomp = 1)
plot_pls(pls_result, threshold = 0)

pls_result_rice <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                     resultName = "NPV_rice", ncomp = 1)
plot_pls(pls_result_rice, threshold = 0)




make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}


make_variables(as.estimate(input_estimates))
