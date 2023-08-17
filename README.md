# Rice Farming with Crop Rotation for Smallholder Farmers in Indonesia
---
title: "Rice Farming with Crop Rotation for Smallholder Farmers in Indonesia"
author: "Noviria Syifaun Nafsi, Sineenad Kongtonkun, Inkyin May, Vani Lian"
date: "2023-07-12"
output:
  html_document: default
  pdf_document: default
bibliography:
- references.bib
- export.bib
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction
### Overview

  Indonesia is the largest country in Southeast Asia.Rice is the primary staple food crop with a steady increase in annual production, making Indonesia the third largest rice producer in the world. 93% of Indonesia’s total number of farmers are small family farms. Rice is the main crop grown and staple food in Southeast Asia. 
  Crop rotation is the practice of planting different crops sequentially on the same plot of land to improve soil health, optimize nutrients in the soil, and combat pest and weed pressure.
    - Soybean (Glycine max) is a species of legume native to East Asia, widely grown for its edible bean which has numerous uses.
    - Chili (Capsicum annum L.)  is a plant of tropical and subtropical regions  for their fleshy fruits

### Motivation

1. Rice is the primary staple food crop with a steady increase in annual production, making Indonesia the third largest rice producer in the world. 
2. Crop rotation can increase crop yields and income than monoculture of rice and it can help disrupt the lifecycle of crop pests and reducing chemical use.
3. Soybean can increase soil fertility and give extra income to farmers.
4. Chili cultivation can improve farmers' income  because of good market price.


### Overview of the project

<center>
```{r echo=FALSE}
knitr::include_graphics('Photo rice farm with crop rotation/overview of the project.png')
```
</center>


### Conceptual model 

Rice farm with crop rotation. Crop rotation for this project is chilli ana soybean. Total cost per crop are consists of labor, seeds, pesticides, fertilizer, machinery and rent land. Moreover, Revenues is yield of rice, soybean and chilli. Finally, total cost, revenues and discount rate use put to calculate to Net Present Value(NVP).

```{r echo=FALSE}
knitr::include_graphics('Photo rice farm with crop rotation/conceptual model.png')
```


## R code
### Variable used in conceptual and mathemetical formulation

```{r}
read.csv("new_variable_estimates.csv",sep=";")
```

<center>
```{r echo=FALSE}
knitr::include_graphics('Photo rice farm with crop rotation/Variable estimate.png')
```

</center>

### Variable used in conceptual model

```{r eval=FALSE, include=FALSE}
read.csv("new_variable_estimates.csv",sep=";")
```

<center>
```{r echo=FALSE}
knitr::include_graphics('Photo rice farm with crop rotation/Variable estimate.png')
```
</center>
Variable for rice farm and crop rotation for small holder farmers in Indonesia have 8 mains variable and this consists of production, rice cultivation cost, soybean production, soybean cultivation cost, chilli production, chilli cultivation cost, discount rate and year of system. This project use 37 variable estimate to calculate


## Estimate Calculation
```{r echo=FALSE}
knitr::include_graphics('Photo rice farm with crop rotation/Estimate calculation.png')
```

## R code
### Decision analysis


```{r include=FALSE}
library(tidyverse)
library(decisionSupport)
library(ggplot2)
```

```{r echo=TRUE}

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

make_variables(read.csv("new_variable_estimates.csv"))

# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("new_variable_estimates.csv", sep=";")

crop_rotation_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                            model_function = crop_rotation_decision,
                                            numberOfModelRuns = 1000,
                                            functionSyntax = "plainNames")

# Run the Monte Carlo simulation using the model function
input_estimates <- read.csv("new_variable_estimates.csv", sep=";")

crop_rotation_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                            model_function = crop_rotation_decision,
                                            numberOfModelRuns = 1000,
                                            functionSyntax = "plainNames")

```



### Plot NPV distribution analysis

#### NPV for crop-rotation
<center>
```{r}

#if rice with soybean and chili (rice-soybean-chili)
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_decision_crop_rotation", "Rice_NPV"),
                                    method = 'smooth_simple_overlay')
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = "NPV_decision_crop_rotation",
                                    method = 'boxplot')
```
</center>


#### NPV for rice-soybean-rice
<center>
```{r}
#if rice with soybean (rice-soybean-rice)
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_decision_rice_soybean","Rice_NPV"),
                                    method = 'smooth_simple_overlay')
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = "NPV_decision_rice_chili",
                                    method = 'boxplot')
```
</center>


#### NPV for rice-chilli
<center>
```{r}
#if rice with chili (rice-chili)
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = c("NPV_decision_rice_chili","Rice_NPV"),
                                    method = 'smooth_simple_overlay')
decisionSupport::plot_distributions(mcSimulation_object = crop_rotation_mc_simulation, 
                                    vars = "NPV_decision_rice_chili",
                                    method = 'boxplot')

```
</center>


### Cashflow analysis

#### With crop rotation of 3 crops
<center>
```{r}
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "cashflow_crop_rotation")
```
</center>

#### with crop rotation of rice and soybean (rice-soybean-rice)
<center>
```{r}
plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "cashflow_rice_soybean")
```
</center>

#### with crop rotation of rice and chili (rice-chili)
<center>
```{r}

plot_cashflow(mcSimulation_object = crop_rotation_mc_simulation, cashflow_var_name = "cashflow_rice_chili")
```
</center>


### Value of Information (VoI) analysis

<center>
```{r}
mcSimulation_table <- data.frame(crop_rotation_mc_simulation$x, crop_rotation_mc_simulation$y[1:7])
```
</center>

#### EVPI crop rotation
<center>
```{r}
evpi_crop_rotation <- multi_EVPI(mc = mcSimulation_table, first_out_var = "crop_rotation_NPV")
plot_evpi(evpi_crop_rotation, decision_vars = "NPV_decision_crop_rotation")
```
</center>

#### EVPI rice and soybean
<center>
```{r}
evpi_rice_soybean <- multi_EVPI(mc = mcSimulation_table, first_out_var = "rice_soybean_NPV")
plot_evpi(evpi_rice_soybean, decision_vars = "NPV_decision_rice_soybean")
```
</center>

#### EVPI rice and chilli
<center>
```{r}
evpi_rice_chili <- multi_EVPI(mc = mcSimulation_table, first_out_var = "rice_chili_NPV")
plot_evpi(evpi_rice_chili, decision_vars = "NPV_decision_rice_chili")
```
</center>

### Projection to Latent Structures (PLS) analysis

#### With crop rotation of rice, soybean, and chili
<center>
```{r}
pls_result_crop_rotation <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                              resultName = names(crop_rotation_mc_simulation$y)[5], ncomp = 1)
plot_pls(pls_result_crop_rotation, threshold = 0)
```
</center>

#### With crop rotation of rice and soybean (rice-soybean-rice)
<center>
```{r}
pls_result_rice_soybean <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                             resultName = names(crop_rotation_mc_simulation$y)[6], ncomp = 1)
plot_pls(pls_result_rice_soybean, threshold = 0)
```
</center>

#### With crop rotation of rice and chili (rice-chili)
<center>
```{r}
pls_result_rice_chili <- plsr.mcSimulation(object = crop_rotation_mc_simulation,
                                           resultName = names(crop_rotation_mc_simulation$y)[7], ncomp = 1)
plot_pls(pls_result_rice_chili, threshold = 0)
```
</center>

## Results

### With crop rotation of 3 crops
<center>
```{r}
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_crop_rotation, 
                EVPIresults = evpi_crop_rotation, decision_var_name = "NPV_decision_crop_rotation", 
                cashflow_var_name = "cashflow_crop_rotation", 
                base_size = 7)
```
</center>


### With crop rotation of rice and soybean (rice-soybean-rice)
<center>
```{r}
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_rice_soybean, 
                EVPIresults = evpi_rice_soybean, decision_var_name = "NPV_decision_rice_soybean", 
                cashflow_var_name = "cashflow_rice_soybean", 
                base_size = 7)
```
</center>

### With crop rotation of rice and chili (rice-chili) 
<center>
```{r}
compound_figure(mcSimulation_object = crop_rotation_mc_simulation, 
                input_table = input_estimates, plsrResults = pls_result_rice_chili, 
                EVPIresults = evpi_rice_chili, decision_var_name = "NPV_decision_rice_chili", 
                cashflow_var_name = "cashflow_rice_chili", 
                base_size = 7)
```
</center>

## Conclusion

1. Our project has proven that selecting the appropriate crop rotation between rice, soybean, and chili to produce the best profits is crucial for achieving optimal results.
2. The decision to rotate crops between rice and chili is still applicable with slightly smaller profits.
3. Crop rotation between rice, soybeans, and rice is less efficient than other options with respect to profit and sustainability.


## Recommendation
1.We recommend Indonesian smallholder farmers to implement crop rotation either for three crops (rice, soybean, and chili) or two crops (rice and chili) as it seems more profitable than growing rice only all year around.
2.However, we would not recommend to implement crop rotation between rice and soybean as it seems not so profitable.

## What have we learned from this project?
1. Rice farming with crop rotation of soybean and chili can be implemented by Indonesian smallholder farmers to get higher income.
2. However, not every crops are profitable to be rotated with rice.
3. There are more uncertainties in crop rotation of rice and soybean compared to other scenarios. Thus, further data and research still needed.
 
## Reference

```{r add_R_bib, include=FALSE}
knitr::write_bib(c(.packages(),
                   'knitr','decisionSupport'), 'export.bib')
```

Amirrullah, Johanes. 2019. “The Effect of Various Crop Rotation on the Improvement of Soil Properties of Irrigation Paddy Field.”

Antriyandarti, Ernoiz. 2015. “Competitiveness and Cost Efficiency of Rice Farming in Indonesia.” Journal of Rural Problems 51: 74–85. https://doi.org/10.7310/arfe.51.74.

BPS. 2018. “[SOUH2018] Struktur Ongkos Usaha Tanaman Cabai Besar Per Hektar Per Musim Tanam Di Indonesia.”
———. 2022. “Statistik Indonesia 2022.”

BRIN. 2022. “Competitiveness of Indonesian Rice Prices in the International Market.” In E3S Web of Conferences. Vol. 361. EDP Sciences. https://doi.org/10.1051/e3sconf/202236101016.

Crystal, Eric, and Peter Whittlesey. 2004. “THE ROLE OF RICE IN SOUTHEAST ASIA.”

Do, Hoa, Eike Luedeling, and Cory Whitney. 2020. “Decision Analysis of Agroforestry Options Reveals Adoption Risks for Resource-Poor Farmers.” Agronomy for Sustainable Development 40 (June). https://doi.org/10.1007/s13593-020-00624-5.

Fao. 2018. “SMALL FAMILY FARMS COUNTRY FACTSHEET.” www.fao.org/family-farming/data-sources/dataportrait/farm-size/en.

Harsono, Arief, Didik Harnowo, Erliana Ginting, and Dian Adi Anggraeni Elisabeth. 2020. “Opportunities to Achieve Self-Sufficiency.” www.intechopen.com.

Jagung, Kedelai. 2017. “Nilai Produksi Dan Biaya Produksi Per Musim Tanam Per Hektar Budidaya Tanaman Padi Sawah, Padi Ladang, Jagung, Dan Kedelai, 2017 Uraian Padi Sawah Padi Ladang.”

Krisdiana, Ruly, Nila Prasetiaswati, Imam Sutrisno, Fachrur Rozi, Arief Harsono, and Made Jana Mejaya. 2021. “Financial Feasibility and Competitiveness Levels of Soybean Varieties in Rice-Based Cropping System of Indonesia.” Sustainability (Switzerland) 13 (August). https://doi.org/10.3390/su13158334.

MOALF. 2016. “Chilli Production.” https://www.southdevonchillifarm.co.uk/onli.

Mucharam, Iim, Ernan Rustiadi, Akhmad Fauzi, and Harianto. 2020. “Assessment of Rice Farming Sustainability: Evidence from Indonesia Provincial Data.” International Journal of Sustainable Development and Planning 15 (December): 1323–13332. https://doi.org/10.18280/ijsdp.150819.

Schilling, Robert. 1999. “THE SOYBEAN COMMODITY SYSTEM IN INDONESIA : OVERVIEW AND PROPOSALS.”

Setiartiti, Lilies. 2021. “Critical Point of View: The Challenges of Agricultural Sector on Governance and Food Security in Indonesia.” In E3S Web of Conferences. Vol. 232. EDP Sciences. https://doi.org/10.1051/e3sconf/202123201034.

Stantec. 2005. “Helping Our Clients Prioritise Programmes and Projects.”

Sundari, M. T., Darsono, J. Sutrisno, and E. Antriyandarti. 2021. “Analysis of Chili Farming in Indonesia.” In IOP Conference Series: Earth and Environmental Science. Vol. 905. IOP Publishing Ltd. https://doi.org/10.1088/1755-1315/905/1/012046.

USDA. 2012. “INDONESIA: Stagnating Rice Production Ensures Continued Need for Imports.”

Wandschneider, Tiago, Paul Gniffke, Teddy Kristedi, Kuntoro Boga, Witono Adiyoga, and Howard Hall N/A N/A. 2019. “Final Report: Eastern Indonesia Agribusiness Development Opportunities-Chilli Value Chain Project Number.”

Wright, Jerry, Bill Wilcke, Edward Usset, Ward Stienstra, Michael Schmitt, Gary Sands, George Rehm, et al. 2005. “SOYBEAN FIELD BOOK.”

Yoshida, Shouichi. 1981. “Fundamentals of Rice Crop Sciences.”
