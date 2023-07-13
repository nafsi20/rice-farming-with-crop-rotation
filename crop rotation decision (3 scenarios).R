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
  
  #if crop rotation of rice and soybean is done in one year (rice-soybean-rice)
  rice_soybean_result = vv((rice_profit*2) + soybean_profit, n=n_year, var_CV=100)
  
  #if crop rotation of rice and chili is done in one year (rice-chili)
  rice_chili_result = vv(rice_profit + chili_profit, n=n_year, var_CV=100)
  
  
  # NPV
  NPV_rice <- discount(rice_cultivation_result, discount_rate, calculate_NPV = TRUE)
  NPV_crop_rotation <- discount(crop_rotation_result, discount_rate, calculate_NPV = TRUE)
  NPV_rice_soybean <- discount(rice_soybean_result, discount_rate, calculate_NPV = TRUE)
  NPV_rice_chili <- discount(rice_chili_result, discount_rate, calculate_NPV = TRUE)
  
  
  # Cashflow
  cashflow_crop_rotation <- crop_rotation_result - rice_cultivation_result
  cashflow_rice_soybean <- rice_soybean_result - rice_cultivation_result
  cashflow_rice_chili <- rice_chili_result - rice_cultivation_result
  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(Rice_NPV = NPV_rice,
              crop_rotation_NPV = NPV_crop_rotation,
              rice_soybean_NPV = NPV_rice_soybean,
              rice_chili_NPV= NPV_rice_chili,
              NPV_decision_crop_rotation = NPV_crop_rotation - NPV_rice,
              NPV_decision_rice_soybean = NPV_rice_soybean - NPV_rice,
              NPV_decision_rice_chili = NPV_rice_chili - NPV_rice,
              cashflow_crop_rotation = cashflow_crop_rotation,
              cashflow_rice_soybean = cashflow_rice_soybean,
              cashflow_rice_chili = cashflow_rice_chili
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
                                            numberOfModelRuns = 1000,
                                            functionSyntax = "plainNames")


# plot NPV distribution analysis

#if rice with soybean and chili (rice-soybean-chili)
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_decision_crop_rotation", "Rice_NPV"),
                                    method = 'smooth_simple_overlay')


#if rice with soybean (rice-soybean-rice)
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_decision_rice_soybean","Rice_NPV"),
                                    method = 'smooth_simple_overlay')


#if rice with chili (rice-chili)
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_decision_rice_chili","Rice_NPV"),
                                    method = 'smooth_simple_overlay')


decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = "NPV_decision_crop_rotation",
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = "NPV_decision_rice_soybean",
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = "NPV_decision_rice_chili",
                                    method = 'boxplot')

# cashflow analysis

#with crop rotation of 3 crops
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "cashflow_crop_rotation")

#with crop rotation of rice and soybean (rice-soybean-rice)
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "cashflow_rice_soybean")

#with crop rotation of rice and chili (rice-chili)
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "cashflow_rice_chili")



# VoI analysis
mcSimulation_table <- data.frame(crop_rotation_mc_simulation$x, crop_rotation_mc_simulation$y[1:7])

evpi_crop_rotation <- multi_EVPI(mc = mcSimulation_table, first_out_var = "crop_rotation_NPV")
plot_evpi(evpi_crop_rotation, decision_vars = "NPV_decision_crop_rotation")


evpi_rice_soybean <- multi_EVPI(mc = mcSimulation_table, first_out_var = "rice_soybean_NPV")
plot_evpi(evpi_rice_soybean, decision_vars = "NPV_decision_rice_soybean")


evpi_rice_chili <- multi_EVPI(mc = mcSimulation_table, first_out_var = "rice_chili_NPV")
plot_evpi(evpi_rice_chili, decision_vars = "NPV_decision_rice_chili")



# Projection to Latent Structures (PLS) analysis

#with crop rotation of rice, soybean, and chili
pls_result_crop_rotation <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                              resultName = names(crop_rotation_mc_simulation$y)[5], ncomp = 1)
plot_pls(pls_result_crop_rotation, threshold = 0)


#with crop rotation of rice and soybean (rice-soybean-rice)
pls_result_rice_soybean <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                             resultName = names(crop_rotation_mc_simulation$y)[6], ncomp = 1)
plot_pls(pls_result_rice_soybean, threshold = 0)


#with crop rotation of rice and chili (rice-chili)
pls_result_rice_chili <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                           resultName = names(crop_rotation_mc_simulation$y)[7], ncomp = 1)
plot_pls(pls_result_rice_chili, threshold = 0)



# the plots

#with crop rotation of 3 crops
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_crop_rotation, 
                EVPIresults = evpi_crop_rotation, decision_var_name = "NPV_decision_crop_rotation", 
                cashflow_var_name = "cashflow_crop_rotation", 
                base_size = 7)


#with crop rotation of rice and soybean (rice-soybean-rice)
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_rice_soybean, 
                EVPIresults = evpi_rice_soybean, decision_var_name = "NPV_decision_rice_soybean", 
                cashflow_var_name = "cashflow_rice_soybean", 
                base_size = 7)

#with crop rotation of rice and chili (rice-chili)
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_rice_chili, 
                EVPIresults = evpi_rice_chili, decision_var_name = "NPV_decision_rice_chili", 
                cashflow_var_name = "cashflow_rice_chili", 
                base_size = 7)

