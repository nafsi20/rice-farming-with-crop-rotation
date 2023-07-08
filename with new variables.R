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
  
  # Final result
  crop_rotation_result = rice_profit + soybean_profit + chili_profit
  rice_cultivation_result = rice_profit*3
  
  # NPV
  NPV_crop_rotation <- discount(crop_rotation_result, discount_rate, calculate_NPV = TRUE)
  NPV_rice <- discount(rice_cultivation_result, discount_rate, calculate_NPV = TRUE)
  NPV_crop_rotation_income <- discount(rice_income + soybean_income + chili_income, discount_rate, calculate_NPV = TRUE)
  NPV_rice_income <- discount(rice_income, discount_rate, calculate_NPV = TRUE)
  NPV_crop_rotation_cost <- discount(rice_cost + soybean_cost + chili_cost, discount_rate, calculate_NPV = TRUE)
  NPV_rice_cost <- discount(rice_cost, discount_rate, calculate_NPV = TRUE)
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(NPV_crop_rotation=NPV_crop_rotation, NPV_rice=NPV_rice,
              NPV_crop_rotation_income=NPV_crop_rotation_income, NPV_rice_income=NPV_rice_income,
              NPV_crop_rotation_cost=NPV_crop_rotation_cost, NPV_rice_cost=NPV_rice_cost,
              crop_rotation_result=crop_rotation_result,
              rice_cultivation_result=rice_cultivation_result))
}

# Run the Monte Carlo simulation using the model function
input_estimates <-read.csv("new_variable_estimates.csv", sep=";")

str(input_estimates)

crop_rotation_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                            model_function = crop_rotation_decision,
                                            numberOfModelRuns = 500,
                                            functionSyntax = "plainNames")


# plot NPV distribution analysis
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_crop_rotation","NPV_rice"),
                                    method = 'smooth_simple_overlay')


#just try, but we aren't using these
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_crop_rotation_income","NPV_rice_income"),
                                    method = 'smooth_simple_overlay')

decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_crop_rotation_cost","NPV_rice_cost"),
                                    method = 'smooth_simple_overlay')


# cashflow analysis

#with crop rotation
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "crop_rotation_result")

#without crop rotation
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "rice_cultivation_result")



# Vol analysis
mcSimulation_table <- data.frame(crop_rotation_mc_simulation$x, crop_rotation_mc_simulation$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_crop_rotation")


plot_evpi(evpi, decision_vars = "NPV_crop_rotation")
plot_evpi(evpi, decision_vars = "NPV_rice")
plot_evpi(evpi, decision_vars = "NPV_crop_rotation_income")



# Projection to Latent Structures (PLS) analysis

#with crop rotation
pls_result_crop_rotation <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                resultName = "NPV_crop_rotation", ncomp = 1)
plot_pls(pls_result, threshold = 0)


#without crop rotation
pls_result_rice <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                     resultName = "NPV_rice", ncomp = 1)
plot_pls(pls_result_rice, threshold = 0)



# the plots

#with crop rotation
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_crop_rotation, 
                EVPIresults = evpi, decision_var_name = "NPV_crop_rotation", 
                cashflow_var_name = "crop_rotation_result", 
                base_size = 7)

#without crop rotation
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_rice, 
                EVPIresults = evpi, decision_var_name = "NPV_rice", 
                cashflow_var_name = "rice_cultivation_result", 
                base_size = 7)


