# Variable estimates


input_variable_estimates <- data.frame(variable = c("Rice yield", 
                                                    "Soybean yield", 
                                                    "Chili yield", 
                                                    "Cost of labor", 
                                                    "Cost of rice seeds", 
                                                    "cost of soybean seeds",
                                                    "cost of chili seeds",
                                                    "cost of pesticides",
                                                    "cost of machinery",
                                                    "cost of harvesting"),
                              lower = c(3, 1, 5, 30000, 1500, 5000, 3000, 1500, 500000, 100000),
                              median = NA,
                              upper = c(8, 5, 10, 100000, 9000, 10000, 12000, 5000, 2000000, 1000000),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm",
                                               "posnorm", "posnorm", "posnorm", "posnorm",
                                               "posnorm", "posnorm"),
                              label = c("Rice yield (ton/ha)", 
                                        "Soybean yield (ton/ha)", 
                                        "Chili yield(ton/ha)", 
                                        "Labor(IDR/person/day)", 
                                        "Rice seeds(IDR/kg)", 
                                        "Soybean seeds(IDR/kg)",
                                        "Chili seeds(IDR/kg)",
                                        "Pesticides(IDR/kg)",
                                        "Machinery(IDR/day)",
                                        "Harvesting(IDR/day)"),
                              Description = c("Yield in a rice farm under normal condition",
                                              "Yield in a soybean farm under normal condition",
                                              "Yield in a chili farm under normal condition",
                                              "Labor costs for every farm under normal condition",
                                              "Seed costs for rice", " Seed costs for soybean",
                                              "Seed costs for chili", "Pesticides costs for every farm under normal condition",
                                              "Machinery costs for every farm under normal condition",
                                              "Harvesting costs for every farm under normal condition"))

input_variable_estimates



