#install.packages("tidyverse")
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("decisionSupport")
library(decisionSupport)


microdosing_table <- read.csv("microdosing_simple.csv")


###make variable###
make_variables <- function(est, n= 1) {
  x <- random(rho = est,n = n)
  for(i in colnames(x)) 
    assign(i, as.numeric(x[1, i]),envir = .GlobalEnv)
}
make_variables <- make_variables (as.estimate(microdosing_table))


system_benefit <- function(){

  production_risk <- chance_event(production_risks_probability, 
                                  value_if = percentage_production_risk_damage,
                                  value_if_not = 1,
                                  n = years)
  
  market_risk <- chance_event(market_risks_probability, 
                              value_if = percentage_market_risk_damage,
                              #damage occurs due to production risk
                              value_if_not = 1,
                              n = years)
  #shea 
  
  shea_revenue <- vv (var_mean= shea_yield * shea_price,
                      var_CV = var_cv,
                      n = years) * market_risk
  
  shea_tree_labor <- vv(var_mean = shea_labor, 
                   var_CV = var_cv,
                   n= years)
  
  shea_profit <- shea_revenue - shea_tree_labor 
  
  #Statusquo
  cost_statusquo <- vv(status_quo_cost,
                       var_CV = var_cv,
                       n= years)
    
  revenue_statusquo <- vv(status_quo_revenue,
                          var_CV = var_cv,
                          n= years)
    
  total_benefit_sq_men <- revenue_statusquo - cost_statusquo
  
  total_benefit_sq_women <-  (revenue_statusquo + shea_profit) - cost_statusquo
  
  
  #Full dose
  cost_RD <- vv(full_dose_cost,
                var_CV = var_cv, 
                n= years)
    
  revenue_RD <- vv(full_dose_revenue,
                   var_CV = var_cv,
                   n= years) * market_risk
    
  total_benefit_RD_men <- revenue_RD - cost_RD
  
  total_benefit_RD_women <-   (revenue_RD + shea_profit) - cost_RD
  
  
  #Microdosing
  cost_MD <- vv(micro_dose_cost,
                var_CV = var_cv,
                n= years)
  
  revenue_MD <- vv (microdose_revenue, 
                    var_CV = var_cv,
                    n = years) * market_risk
  
  total_benefit_MD_men <-  revenue_MD - cost_MD
  
  total_benefit_MD_women <- (revenue_MD + shea_profit)- cost_MD
  
  
  #statusquo
  
  NPV_statusquo_men <-discount(total_benefit_sq_men, 
                               discount_rate = discount_rate, 
                               calculate_NPV = TRUE)
  
  cashflow_sq_men <- discount (total_benefit_sq_men,
                               discount_rate = discount_rate,
                               calculate_NPV = FALSE)
  
  cumulative_cashflow_sq_men <- cumsum(cashflow_sq_men)
  
  
  NPV_statusquo_women <-discount(total_benefit_sq_women, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
  
  cashflow_sq_women <- discount (total_benefit_sq_women,
                                 discount_rate = discount_rate,
                                 calculate_NPV = FALSE)
  
  cumulative_cashflow_sq_women <- cumsum(cashflow_sq_women)
  
  #Full dose
  
  NPV_full_dose_men <-discount(total_benefit_RD_men, 
                               discount_rate = discount_rate, 
                               calculate_NPV = TRUE)
  
  cashflow_RD_men <- discount (total_benefit_RD_men,
                               discount_rate = discount_rate,
                               calculate_NPV = FALSE)
  
  cumulative_cashflow_RD_men <- cumsum(cashflow_RD_men)
  
  
  NPV_full_dose_women <-discount(total_benefit_RD_women, 
                                 discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
  
  cashflow_RD_women <- discount (total_benefit_RD_women,
                                 discount_rate = discount_rate,
                                 calculate_NPV = FALSE)
  
  cumulative_cashflow_RD_women <- cumsum(cashflow_RD_women)
  
  
  #Micro-dose
  
  NPV_micro_dose_men <-discount(total_benefit_MD_men, 
                                discount_rate = discount_rate, 
                                calculate_NPV = TRUE)
  
  cashflow_MD_men <- discount (total_benefit_MD_men,
                               discount_rate = discount_rate,
                               calculate_NPV = FALSE)
  
  cumulative_cashflow_MD_men <- cumsum(cashflow_MD_men)
  
  
  NPV_micro_dose_women <-discount(total_benefit_MD_women, 
                                  discount_rate = discount_rate, 
                                  calculate_NPV = TRUE)
  
  cashflow_MD_women <- discount (total_benefit_MD_women,
                                 discount_rate = discount_rate,
                                 calculate_NPV = FALSE)
  
  cumulative_cashflow_MD_women <- cumsum(cashflow_MD_women)
  
  return(list( 
    
  NPV_statusquo_men = NPV_statusquo_men,
  NPV_statusquo_women = NPV_statusquo_women,
  NPV_full_dose_men = NPV_full_dose_women,
  NPV_full_dose_women = NPV_full_dose_women,
  NPV_micro_dose_men = NPV_micro_dose_men,
  NPV_micro_dose_women = NPV_micro_dose_women,
    
  cashflow_statusquo_men = cumulative_cashflow_sq_men,
  cashflow_statusquo_women = cumulative_cashflow_sq_women,
  cashflow_recommended_dose_men = cumulative_cashflow_RD_men,
  cashflow_recommended_dose_women = cumulative_cashflow_RD_women,
  cashflow_microdosing_men = cumulative_cashflow_MD_men,
  cashflow_microdosing_women = cumulative_cashflow_MD_women
))
}

#montecralo simulation 

AF_mc_simulation <- mcSimulation(as.estimate(microdosing_table),
                                 model_function = system_benefit,
                                 numberOfModelRuns = 10000,
                                 functionSyntax = "plainNames")

#write.csv(AF_mc_simulation, "./results/microdosing_simulation_results.csv")


## NPV distribution as boxplot
distribution_parkland <- plot_distributions(
  mcSimulation_object = AF_mc_simulation,
  vars = c("NPV_statusquo_men", "NPV_statusquo_women",
           "NPV_full_dose_men", "NPV_full_dose_women",
           "NPV_micro_dose_men", "NPV_micro_dose_women"),
  method = 'boxplot',
  colors = c("#0000FF", "#F0E442", "#E69F00",
             "#009E73", "#CC79A7", "#D55E00"), # six distinct colors
  base_size = 10
)

distribution_parkland



# Load libraries
library(ggplot2)
library(reshape2) # or tidyr for pivot_longer

# Extract only the variables of interest

df <- as.data.frame(AF_mc_simulation)[, 1:6]

# Reshape to long format
df_long <- melt(df, variable.name = "Scenario", value.name = "NPV")

# Set the order explicitly
df_long$Scenario <- factor(df_long$Scenario, levels = my_vars)

# Define colors
my_colors <- c("#0000FF", "#F0E442", "#E69F00",
               "#009E73", "#CC79A7", "#D55E00")

# Plot
ggplot(df_long, aes(x = Scenario, y = NPV, fill = Scenario)) +
  geom_boxplot() +
  scale_fill_manual(values = my_colors) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





##Cashflow


cashflow_sq_men <- plot_cashflow(mcSimulation_object = AF_mc_simulation,
                          cashflow_var_name = "cashflow_statusquo_men",
                          x_axis_name = "",
                          y_axis_name = "",
                          color_25_75 = "grey",
                          color_5_95 = "yellow",
                          color_median= "red",
                          base= 10)+
coord_cartesian(ylim = c(-1000, 5000))

cashflow_sq_men


cashflow_sq_women <- plot_cashflow(mcSimulation_object = AF_mc_simulation,
                                 cashflow_var_name = "cashflow_statusquo_women",
                                 x_axis_name = "",
                                 y_axis_name = "",
                                 color_25_75 = "grey",
                                 color_5_95 = "yellow",
                                 color_median= "red",
                                 base= 10)+ coord_cartesian(ylim = c(-1000, 5000))



cashflow_sq_women


cashflow_full_men <- plot_cashflow(mcSimulation_object = AF_mc_simulation,
                                 cashflow_var_name = "cashflow_recommended_dose_men",
                                 x_axis_name = "",
                                 y_axis_name = "",
                                 color_25_75 = "grey",
                                 color_5_95 = "yellow",
                                 color_median= "red",
                                 base= 10)+ coord_cartesian(ylim = c(-1000, 5000))

cashflow_full_men


cashflow_full_women <- plot_cashflow(mcSimulation_object = AF_mc_simulation,
                                   cashflow_var_name = "cashflow_recommended_dose_women",
                                   x_axis_name = "",
                                   y_axis_name = "",
                                   color_25_75 = "grey",
                                   color_5_95 = "yellow",
                                   color_median= "red",
                                   base= 10)+ coord_cartesian(ylim = c(-1000, 5000))



cashflow_full_women


#micro
cashflow_micro_men <- plot_cashflow(mcSimulation_object = AF_mc_simulation,
                                   cashflow_var_name = "cashflow_microdosing_men",
                                   x_axis_name = "",
                                   y_axis_name = "",
                                   color_25_75 = "grey",
                                   color_5_95 = "yellow",
                                   color_median= "red",
                                   base= 10)+ coord_cartesian(ylim = c(-1000, 5000))

cashflow_micro_men


cashflow_micro_women <- plot_cashflow(mcSimulation_object = AF_mc_simulation,
                                     cashflow_var_name = "cashflow_microdosing_women",
                                     x_axis_name = "",
                                     y_axis_name = "",
                                     color_25_75 = "grey",
                                     color_5_95 = "yellow",
                                     color_median= "red",
                                     base= 10)+ coord_cartesian(ylim = c(-1000, 5000))



cashflow_micro_women

combined_plot_raw <- (
  cashflow_sq_men + cashflow_sq_women +
    cashflow_full_men + cashflow_full_women +
    cashflow_micro_men + cashflow_micro_women
) +
  plot_layout(guides = "collect", ncol = 2) &
  theme(legend.position = "right")

combined_plot_raw


# Step 3: Add single, shared axis labels using patchwork's annotation system
combined_plot <- combined_plot_raw &
  labs(
    x = "Years of intervention",
    y = "Cashflow in $/ha"
  )

# Show the final combined plot
combined_plot
  

combined_plot






