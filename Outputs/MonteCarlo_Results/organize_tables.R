require(data.table)
require(dplyr)


tm = fread('Outputs/MonteCarlo_Results/tm_mc_results.csv') %>% mutate(Sensor = "TM")
etm = fread('Outputs/MonteCarlo_Results/etm_mc_results.csv') %>% mutate(Sensor = "ETM")
oli = fread('Outputs/MonteCarlo_Results/oli_mc_results.csv') %>% mutate(Sensor = "OLI")

full = rbind(tm, etm, oli)

write.csv(full, "Outputs/MonteCarlo_Results/final_results.csv")
