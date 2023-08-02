require(data.table)
require(dplyr)


tm = fread('Outputs/MonteCarlo_Results/tm_mc_results.csv') %>%   mutate(Sensor = paste(V1, "01TM"))
etm = fread('Outputs/MonteCarlo_Results/etm_mc_results.csv') %>% mutate(Sensor = paste(V1, "02ETM"))
oli = fread('Outputs/MonteCarlo_Results/oli_mc_results.csv') %>% mutate(Sensor = paste(V1, "03OLI"))

full = rbind(tm, etm, oli, fill = T) %>% arrange(Sensor)

write.csv(full, "Outputs/MonteCarlo_Results/final_results.csv")
