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
  
  #Estimate the cost in a normal season
  chili_cost <- sum(chili_land_rental_cost, chili_seeds_cost, chili_fertilizer_cost,
                    chili_pesticide_cost, chili_machinery_cost, chili_harvesting_cost)
  
  # Estimate the profit
  rice_profit <- vv(rice_income - rice_cost, n=n_year, var_CV=100)
  soybean_profit <- vv(soybean_income - soybean_cost, n=n_year, var_CV=100)
  chili_profit <- vv(chili_income - chili_cost, n=n_year, var_CV=100)
  
  # Final result
    #assuming rice cultivation is 3 times per year  
    rice_cultivation_result = rice_profit*3
  
    #crop rotation scenario
      #if crop rotation of 3 crops is done in one year
      crop_rotation_result = rice_profit + soybean_profit + chili_profit
  
      #if crop rotation of rice and soybean is done in one year (rice-soybean-rice)
      rice_soybean_result = (rice_profit*2) + soybean_profit
    
      #if crop rotation of rice and chili is done in one year (rice-chili)
      rice_chili_result = rice_profit + chili_profit
      
      
  
  # NPV
  NPV_rice <- discount(rice_cultivation_result, discount_rate, calculate_NPV = TRUE)
  NPV_crop_rotation_full <- discount(crop_rotation_result, discount_rate, calculate_NPV = TRUE)
  NPV_rice_soybean <- discount(rice_soybean_result, discount_rate, calculate_NPV = TRUE)
  NPV_rice_chili <- discount(rice_soybean_result, discount_rate, calculate_NPV = TRUE)
  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(NPV_rice = NPV_rice, NPV_crop_rotation_full = NPV_crop_rotation_full, 
              NPV_rice_soybean = NPV_rice_soybean, NPV_rice_chili = NPV_rice_chili,
              
              rice_cultivation_result = rice_cultivation_result,
              crop_rotation_result = crop_rotation_result,
              rice_soybean_result = rice_soybean_result,
              rice_chili_result = rice_chili_result
              ))
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
                                    vars = c("NPV_crop_rotation_full","NPV_rice"),
                                    method = 'smooth_simple_overlay')


#if rice with soybean (rice-soybean-rice)
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_rice_soybean","NPV_rice"),
                                    method = 'smooth_simple_overlay')

#if rice with chili (rice-chili)
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_rice_chili","NPV_rice"),
                                    method = 'smooth_simple_overlay')


# cashflow analysis

  #without crop rotation
  plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "rice_cultivation_result")

  #with crop rotation of 3 crops
  plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "crop_rotation_result")

  #with crop rotation of rice and soybean (rice-soybean-rice)
  plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "rice_soybean_result")
  
  #with crop rotation of rice and chili (rice-chili)
  plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "rice_chili_result")
  

# Vol analysis
mcSimulation_table <- data.frame(crop_rotation_mc_simulation$x, crop_rotation_mc_simulation$y[1:4])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_rice")


plot_evpi(evpi, decision_vars = "NPV_rice")
plot_evpi(evpi, decision_vars = "NPV_crop_rotation_full")
plot_evpi(evpi, decision_vars = "NPV_rice_soybean")
plot_evpi(evpi, decision_vars = "NPV_rice_chili")



# Projection to Latent Structures (PLS) analysis

  #without crop rotation
  pls_result_rice <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                     resultName = "NPV_rice", ncomp = 1)
  plot_pls(pls_result_rice, threshold = 0)


  #with crop rotation of 3 crops 
  pls_result_crop_rotation <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                              resultName = "NPV_crop_rotation_full", ncomp = 1)
  plot_pls(pls_result, threshold = 0)

  #with crop rotation of rice and soybean (rice-soybean-rice)
  pls_result_rice_soybean <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                                resultName = "NPV_rice_soybean", ncomp = 1)
  plot_pls(pls_result, threshold = 0)
  
  #with crop rotation of rice and chili (rice-chili)
  pls_result_rice_chili <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                                resultName = "NPV_rice_chili", ncomp = 1)
  plot_pls(pls_result, threshold = 0)
  

# the plots

  #without crop rotation
  compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                  input_table = input_estimates, plsrResults = pls_result_rice, 
                  EVPIresults = evpi, decision_var_name = "NPV_rice", 
                  cashflow_var_name = "rice_cultivation_result", 
                  base_size = 7)
  
  
  #with crop rotation of 3 crops
  compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_crop_rotation, 
                EVPIresults = evpi, decision_var_name = "NPV_crop_rotation_full", 
                cashflow_var_name = "crop_rotation_result", 
                base_size = 7)


  #with crop rotation of rice and soybean (rice-soybean-rice)
  compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                  input_table = input_estimates, plsrResults = pls_result_rice_soybean, 
                  EVPIresults = evpi, decision_var_name = "NPV_rice_soybean", 
                  cashflow_var_name = "rice_soybean_result", 
                  base_size = 7)

  #with crop rotation of rice and chili (rice-chili)
  compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                  input_table = input_estimates, plsrResults = pls_result_rice_chili, 
                  EVPIresults = evpi, decision_var_name = "NPV_rice_chili", 
                  cashflow_var_name = "rice_chili_result", 
                  base_size = 7)
  