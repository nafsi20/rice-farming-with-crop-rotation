library(tidyverse)
library(decisionSupport)
library(ggplot2)

read.csv("new_variable_estimates.csv", sep=";")


crop_rotation_decision <- function(){
  
  # Estimate the income of rice in a normal season
  rice_income <- vv(rice_yield * rice_price, n=n_year, var_CV=100)
  
  # Estimate the income of soybean in a normal season
  soybean_income <- vv(soybean_yield * soybean_price, n=n_year, var_CV=100)
  
  # Estimate the income of chili in a normal season
  chili_income <- vv(chili_yield * chili_price, n=n_year, var_CV=100)
  
  #Estimate the cost of rice farm in a normal season
  rice_cost_precal <- sum(rice_land_rental_cost, rice_seeds_cost, rice_fertilizer_cost,
                          rice_pesticide_cost, rice_machinery_cost, rice_harvesting_cost)
  rice_cost <- vv(rice_cost_precal, n=n_year, var_CV=100)
  
  
  #Estimate the cost of soybean farm in a normal season
  soybean_cost_precal <- sum(soybean_land_rental_cost, soybean_seeds_cost, soybean_fertilizer_cost,
                             soybean_pesticide_cost, soybean_machinery_cost, soybean_harvesting_cost)
  soybean_cost <- vv(soybean_cost_precal, n=n_year, var_CV=100)
  
  
  #Estimate the cost in a normal season
  chili_cost_precal <- sum(chili_land_rental_cost, chili_seeds_cost, chili_fertilizer_cost,
                           chili_pesticide_cost, chili_machinery_cost, chili_harvesting_cost)
  chili_cost <- vv(chili_cost_precal, n=n_year, var_CV=100)
  
  
  # Estimate the profit
  rice_profit <- vv(rice_income - rice_cost, n=n_year, var_CV=100)
  soybean_profit <- vv(soybean_income - soybean_cost, n=n_year, var_CV=100)
  chili_profit <- vv(chili_income - chili_cost, n=n_year, var_CV=100)
  
  
  # Final result
  #assuming rice cultivation is 3 times per year  
  rice_cultivation_result = vv(rice_profit*3, n=n_year, var_CV=100)
  
  #crop rotation decision scenario
  #if crop rotation of 3 crops is done in one year
  crop_rotation_result = vv(rice_profit + soybean_profit + chili_profit, n=n_year, var_CV=100)
  
  
  # NPV
  NPV_rice <- discount(rice_cultivation_result, discount_rate, calculate_NPV = TRUE)
  NPV_crop_rotation <- discount(crop_rotation_result, discount_rate, calculate_NPV = TRUE)
  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(Rice_NPV = NPV_rice,
              crop_rotation_NPV = NPV_crop_rotation,
              NPV_decision_crop_rotation = NPV_crop_rotation - NPV_rice,
              cashflow_crop_rotation = crop_rotation_result - rice_cultivation_result
  ))
}


make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("new_variable_estimates.csv"))



# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("new_variable_estimates.csv", sep=";")

crop_rotation_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                            model_function = crop_rotation_decision,
                                            numberOfModelRuns = 500,
                                            functionSyntax = "plainNames")


# plot NPV distribution analysis

decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("crop_rotation_NPV", "Rice_NPV"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = "NPV_decision_crop_rotation",
                                    method = 'boxplot')


# cashflow analysis

#with crop rotation of 3 crops
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "cashflow_crop_rotation")



# Vol analysis
mcSimulation_table <- data.frame(crop_rotation_mc_simulation$x, crop_rotation_mc_simulation$y[1:3])

evpi_crop_rotation <- multi_EVPI(mc = mcSimulation_table, first_out_var = "crop_rotation_NPV")
plot_evpi(evpi_crop_rotation, decision_vars = "NPV_decision_crop_rotation")


# Projection to Latent Structures (PLS) analysis

#with crop rotation of rice, soybean, and chili
pls_result_crop_rotation <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                              resultName = names(crop_rotation_mc_simulation$y)[5], ncomp = 1)
plot_pls(pls_result_crop_rotation, threshold = 0)



# the plots

#with crop rotation of 3 crops
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_crop_rotation, 
                EVPIresults = evpi_crop_rotation, decision_var_name = "NPV_decision_crop_rotation", 
                cashflow_var_name = "cashflow_crop_rotation", 
                base_size = 7)

