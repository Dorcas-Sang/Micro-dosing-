getwd()

###microdosing###

####farm level benefits####

##install and run packages##

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("decisionSupport")
library(decisionSupport)

#We will develop a base function to be used later to estimate our outcomes
source('fucntions_microdosing.R') #Remember to create the base R function


##load and read table
microdosing_table <- read.csv("md_input_variables.csv")
microdosing_table

###make variable###
make_variables <- function(est, n= 1) {
  x <- random(rho = est,n = n)
  for(i in colnames(x)) 
    assign(i, as.numeric(x[1, i]),envir = .GlobalEnv)
}

make_variables <- make_variables (as.estimate(microdosing_table))

###Decision options###
#We are comparing both the status quo and recommended fertilizer dose to microdosing
#status quo = maize + shea - fertilizer #the farmers does not apply fertilizer to the field
#In status quo the farmer does not apply fertilizer
#option 1 = maize + shea + recommended fertilizer usage 
#For option 1 the farmer applies fertilizer to the field at the recommended rate of 200kg/ha
#option 2 = maize + shea + 1.5g microdose fertilizer 
#For option 2 the farmer applies just a doze of fertilizer to the field 
#in option 2 the farmer can apply a dose of up to 2.6g
#In our case, the farmer applies 1.5g, thus about 95-100kg is applied per hectare of land


####model functions####
system_options_functions <- function(){
  
  ####systems risks#### All the risks associated with the system
  
  ##maize production risk## 
  #This includes all the risks associated with maize production e.g. floods, droughts, pest and diseases and bush fires
  #We only include production risk for maize because shea grows naturally and farmers do not plant it
  maize_production_risk <- chance_event(maize_risk, value_if = maize_risk_percent_damage,
                             value_if_not = 1,
                             n = years)
  
  
  
  ##market/price risk## 
  #This is due to fluctuation in prices and inflation
  #This applies to both maize and shea
  
  maize_market_risk <- chance_event(maize_market_risk, value_if = maize_market_risk_percent_damage,
                                    #damage occurs due to production risk
                                    value_if_not = 1,
                                    n = years)
  
  shea_market_risk <- chance_event(shea_market_risk, value_if = shea_market_risk_percent_damage,
                                   #damage occurs due to production risk
                                   value_if_not = 1,
                                   n = years)

   
  ####system cost####
  
  ##Generic cost## 
  #It applies to all costs that occur in all the options
  #Generic cost includes, machinery cost, storage cost, land cost, as well as labour, inputs and other costs
  
  
  
  machinery_cost <- vv (var_mean = tractor_services + harvester_services + sheller_services + transport,
                        #cost of all machinery services on the farm. This is generic cost
                        var_CV = var_cv,
                        n = years)
  
  storage_cost <- vv (var_mean = storage_bag + pesticides,#cost of storage of maize
                      var_CV = var_cv,
                      n = years)
  
  land_cost <- vv (var_mean = land_acquisition,# cost incurred in acquiring or using land
                   var_CV = var_cv,#farmers who do not own lands pay cola or small amount to use the land
                   n  = years)#the money can be equivalent to bag of fertilizer or quantity of yields
  
  total_infrastructure_cost <- machinery_cost + storage_cost + land_cost #total cost of infrastructure
  
  total_infrastrucuture_cost1 <- vv (var_mean = total_infrastructure_cost,
                                     var_CV = var_cv,
                                     n = years)
  
  labour_cost <- vv (var_mean = hired_labour,
                     var_CV = var_cv,#all cost associated with using hired labour on the farm excluding fertilizer application
                     n = years) #includes sowing, pesticides and weedicide application
  
  inputs_cost <- vv (var_mean = seed + pesticides + weedicides,#all cost for inputs excluding fertilizer
                     var_CV = var_cv,
                     n = years)
  
  other_cost <- (tools + maintenance) #the cost of farm tools such as cutlass, hoe and maintenance like sharpening cost
  #This cost is not recurring but can occur periodically during the farming seasons
  
  other_cost1 <- vv (var_mean = other_cost,
                     var_CV = var_cv,
                     n = years)
  
  ##cost associated with the status quo## 
  #summation of all cost involved in status quo (maize + shea - fertilizer)
  status_quo_cost <- (total_infrastrucuture_cost1 + labour_cost + other_cost1)
  
  
  ##cost associated with option 1 (maize + shea + recommended fertilizer)
  #There is a cost for fertilizer application in this option
  #Fertilizer is applied widely through broadcasting or spraying
  #Famers hire labour for this and pay by day
  option1_cost <- vv (var_mean = (fertilizer_full_dose_quantity * fertilizer_price) + fertilizer_application_full_dose,
                      var_CV = var_cv,
                      n = years)
  
  option1_total_cost <- (option1_cost + total_infrastrucuture_cost1 + labour_cost + other_cost1)
  
  
  ##cost associated with option 2 (maize + shea + microdosing)
  #There is both fertilizer cost and application cost
  #Microdsing dosing requires more time for fertilizer application
  option2_cost <- vv (var_mean = (fertilizer_microdose_quantity * fertilizer_price) + fertilizer_application_microdose,
                      var_CV = var_cv,
                      n = years)
  
  option2_total_cost <- (option2_cost + total_infrastrucuture_cost1 + labour_cost + other_cost1)
  
  
  #### System benefits #### Benefits associated with each of the three options
  
  
  #Benefits from status quo # (maize + shea without fertilizer)
  
  #Maize component# This comprise both maize yields and residue from maize
  
  maize_profit_status_quo <- vv (maize_yields_status_quo * maize_price, #yield (kg/ha)
                                 var_CV = var_cv,
                                 n = years)
  
  maize_residue_profit_status_quo <- vv (maize_residue_status_quo * residue_price, #biomass profit (kg/ha)
                                         var_CV = var_cv,
                                         n = years)
  total_status_quo_maize_income <- (maize_profit_status_quo + maize_residue_profit_status_quo) * market_risk
  
  #shea component# This constitutes only shea yields but not residue 
  
  shea_profit_status_quo <- vv (shea_yields_status_quo * shea_price,
                                var_CV = var_cv,
                                n = years)
  
  
  total_status_quo_shea_income <- (shea_profit_status_quo) * market_risk 
  
  
  
  #Benefits from option1 # (maize + shea + recommended fertilizer)
  
  #maize component#
  
  maize_profit_RF <- vv (maize_yields_full_dose * maize_price,
                         var_CV = var_cv,
                         n = years)
  
  maize_residue_profit_RF <- vv (maize_residue_full_dose * residue_price,
                                 var_CV = var_cv,
                                 n = years)
  
  total_maize_RF_income <- (maize_profit_RF + maize_residue_profit_RF) * market_risk 
  

  #shea component#
  
  shea_profit_RF <- vv (shea_yields_full_dose * shea_price,
                        var_CV = var_cv,
                        n = years)
  
  
  total_shea_RF_income <- (shea_profit_RF) * market_risk
  
  
  
  #Benefits from option2 # (maize + shea + fertilizer microdosing)
  
  #maize component
  
  maize_profit_md <- vv (maize_yields_microdose * market_price,
                         var_CV = var_cv,
                         n = years)
  
  maize_residue_profit_md <- vv (maize_residue_microdose * residue_price,
                                 var_CV = var_cv,
                                 n = years)
  
  total_maize_md_income <- (maize_profit_md + maize_residue_profit_md) * (market_risk) * (farmer_risk)
  
 
  #shea component
  
  shea_profit_md <- vv (shea_yields_microdose * shea_price,
                        var_CV = var_cv,
                        n = years)
  
  
  total_shea_md_income <- (shea_profit_md + shea_residue_profit_md) * market_risk
  
  
  ##Environmental benefits## Benefits of the systems on the environment
  
  #Carbon sequestration# The benefits of the shea tree in absorbing carbon in the soil
  #Carbon sequestration potential of shea trees is lost when cut down.
  # So this can be affected by competition or resource conflict
  
  carbon_sequestration <- vv (carbon_sequestration,
                              var_CV = var_cv,
                              n = years) * resource_conflict_risk
  
  
  #Nutrient replenishment# The presence of maize and shea biomass replenishes the soil
  #This is affected by competing uses for livestock, fuel wood (resource conflict)
  
  soil_replenishment <- vv (soil_replenished,
                            var_CV = var_cv,
                            n = years) * resource_conflict_risk
  
  #flood prevention
  #Shea trees reduces flooding and erosion
  flood_control <- vv (erosion_control,
                       var_CV = var_cv,
                       n = years)
  
  
 
  ####Social benefits# the benefits that households derive from the options.
  #This benefit might be affected by competition for resources and market risk
  
  #Women benefits. Benefits women derive from shea in the system
  #In Northern Ghana, shea is a woman crop so the benefits are associated with women 
  women_benefit_raw_shea <- vv (shea_yields_statu_quo * shea_price,
                       var_CV = var_cv,
                       n = years) * (market_risk) * (resource_conflict)
  
  women_benefit_processed_shea <- vv (processed_shea_quantity * sheabutter_market_price,
                         var_CV = var_cf,
                         n = years) * market_risk
  
  women_total_beneift <- (women_benefit_raw_shea + women_benefit_processed_shea)
  
  
  #Food security benefits. More food at home for the household, which improves food security
  food_security <- vv(household_food_security * food_price,
                      var_CV = var_cv,
                      n = years) * market_risk
  
  #The profit to the household contributes to improved educational support to children
  #In Ghana basic and secondary education are free, particularly for public schools
  #Parents will not pay fees but rather can contribute better to provide support toward children education
  #For instance, children get higher pocket money for school, buy required books and materials etc.
 
  child_education <- chance_event(profit_probability, 
                                  value_if = education_support,
                                value_if_not = 1,
                                n = years)
  
  #saving money from low fertilizer for microdosing
  #Compared to the recommended fertilizer dose option, a farmer can save from microdosing
  #This saving is due to the reduced quantity of fertilizer used and the cost of fertilizer
  
  saved_money_for_fertilizer <- vv(option1_cost - option2_cost,
                                 var_cv = var_cv,
                                 n = years)
  
  
  ####Total Economic Benefits from the 3 options
  # This includes summation of all the benefits associatited with each options minus the cost
  #The benefits includes profits, environmental, and social benefits, which are summed up as total benefits for the system
  
  #Profits from the 3 options
  status_quo_profit <- ((total_status_quo_maize_income + total_status_quo_shea_income) - status_quo_cost)/exchange_rate
  mean_status <- range(status_quo_profit)  #to determine the range profit of the status quo
  
  full_dose_profit_raw <- ((total_maize_RF_income + total_shea_RF_income) - option1_total_cost)/exchange_rate
  full_dose_profit <- full_dose_profit_raw - statu_quo_profit
  
  microdose_profit_raw <- ((total_maize_md_income + total_shea_md_income) - option2_total_cost)/exchange_rate
  microdose_profit <- microdose_profit_raw - statu_quo_profit
  
  #Environmental benefits
  envt_benefits_status_quo <- (carbon_sequestration + soil_replenishment + flood_control)/exchange_rate
  
  envt_benefits_full_dose_raw <- (carbon_sequestration + soil_replenishment + flood_control)/exchange_rate
  envt_benefits_full_dose <- envt_benefits_full_dose_raw - envt_benefits_status_quo
  
  envt_benefits_microdose_raw <- (carbon_sequestration + soil_replenishment + flood_control)/exchange_rate
  envt_benefits_microdose <- envt_benefits_microdose_raw - envt_benefits_status_quo
  
  
  #Social benefits
   social_benefits_status_quo <- (women_total_beneift)/exchange_rate
   
   social_benefits_full_dose_raw <- (women_total_beneift + food_security)/exchange_rate
   social_benefits_full_dose <- social_benefits_full_dose_raw - social_benefits_status_quo
   
   social_benefits_microdose_raw <- (women_total_beneift + food_security + saved_money_for_fertilizer)/exchange_rate
   social_benefits_microdose <- social_benefits_microdose_raw - social_benefits_status_quo
   
   #Total benefits. Summation of all benefits from each option
   total_benefits_status_quo <- (statu_quo_profit + envt_benefits_status_quo + social_benefits_status_quo)
   
   total_benefits_full_dose <- (full_dose_profit + envt_benefits_full_dose + social_benefits_status_quo)
   
   total_benefits_microdose <- (microdose_profit + envt_benefits_microdose + social_benefits_microdose)
 
   
   ####Analysis####
   
   #Net Present Value (NPV)
   #Cash flow analysis: projected trend of monetary return based on the profit
   #Discount rate is time value for money
   
   #status quo# Maize + shea without fertilizer
   status_quo_profit <- discount(status_quo_profit, discount_rate = discount_rate, 
                                calculate_NPV = TRUE)
   
   status_quo_envt <- discount(envt_benefits_status_quo, discount_rate = discount_rate, 
                               calculate_NPV = TRUE)
   
   status_quo_social <- discount(social_benefits_status_quo, discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
   
   NPV_status_quo <- discount(total_benefits_status_quo, discount_rate = discount_rate, 
                      calculate_NPV = TRUE)
   
   cashflow_status_quo <- discount (total_benefits_status_quo, discount_rate = discount_rate,
                            calculate_NPV = FALSE)
   
   cumulative_cashflow_status_quo <- cumsum(cashflow_status_quo)
   
   
   #Option 1. Maize + shea with recommended fertilizer
   full_dose_profit <- discount(full_dose_profit, discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
   
   full_dose_envt <- discount(envt_benefits_full_dose, discount_rate = discount_rate, 
                               calculate_NPV = TRUE)
   
   full_dose_social <- discount(social_benefits_full_dose, discount_rate = discount_rate, 
                                 calculate_NPV = TRUE)
   
   NPV_full_dose <- discount(total_benefits_full_dose, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)
   
   cashflow_full_dose <- discount (total_benefits_full_dose, discount_rate = discount_rate,
                                    calculate_NPV = FALSE)
   
   cumulative_cashflow_full_dose <- cumsum(cashflow_full_dose)
   
   
   #Option 2. Maize + shea + microdose
   microdose_profit <- discount(microdose_profit, discount_rate = discount_rate, 
                                calculate_NPV = TRUE)
   
   microdose_envt <- discount(envt_benefits_microdose, discount_rate = discount_rate, 
                              calculate_NPV = TRUE)
   
   microdose_social <- discount(social_benefits_microdose, discount_rate = discount_rate, 
                                calculate_NPV = TRUE)
   
   NPV_microdose <- discount(total_benefits_microdose, discount_rate = discount_rate, 
                             calculate_NPV = TRUE)
   
   cashflow_microdose <- discount (total_benefits_microdose, discount_rate = discount_rate,
                                   calculate_NPV = FALSE)
   
   cumulative_cashflow_microdose <- cumsum(cashflow_microdose)
   
   
   ###Calling anything that needs to plotted##
   ##profit, environmental, household and total of all benefits ##
   
   return(list(profit_statusquo = status_quo_profit,
               profit_option_1 = full_dose_profit,
               profit_option_2 = microdose_profit,
               environmental_benefit_status_quo = status_quo_envt,
               environmental_benefit_option_1 = full_dose_envt,
               environmental_benefit_option_2 = microdose_envt,
               social_benefit_status_quo = status_quo_social,
               social_benefit_option_1 = full_dose_social,
               social_benefit_option_2 = microdose_social,
               NPV_status_quo = NPV_status_quo,
               NPV_option_1 = NPV_full_dose,
               NPV_option_2 = NPV_microdose,
               cashflow_statusquo = cumulative_cashflow_status_quo,
               cashflow_option_1 = cumulative_cashflow_full_dose,
               cashflow_option_2= cumulative_cashflow_microdose))
   
} 


####Monte Carlo simulation

Microdose_mc_simulation <- mcSimulation(as.estimate(table), 
                                   model_function = system_benefits,
                                   numberOfModelRuns = 1000,
                                   functionSyntax = "plainNames")

write.csv(microdose_mc_simulation, "./microdose_mc_simulation_results.csv")


####Data subset####

microdosing_mc_simulation_result <-read.csv("microdose_mc_simulation_results.csv",header= TRUE, sep=",")

####Plots####

install.packages("gridExtra")
install.packages("cowplot")
install.packages("gganimate")
install.packages("ggpubr")

library(cowplot) #For building plots together
library(gganimate)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra) #For building plots together
library(tidyverse)
library(patchwork) #For building plots together

###Doing box plot and smooth plot to choose from later###

##Profit visualization#### 

profit_table<-microdosing_mc_simulation_result [,c(3:7)] 

profit_data_frame <- data.frame(profit_table)

microdosing_profit <- profit_data_frame %>% 
  pivot_longer(cols = y.profit_statusquo:y.profit_option_2, names_to = "statusquo_full_dose_microdosing_options",
               values_to = 'Profit')

#### Plotting  profit distribution ####

### Box plot to see how they overlap ###

profit_plot_box= ggplot(microdosing_profit, aes(x = Profit, fill = statusquo_full_dose_microdosing_options, color= statusquo_full_dose_microdosing_options)) +                       
  geom_boxplot()+
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Farm Profit ($) compared to -452 to -474 $ statusquo") ###change this to actual statusquo value

profit_plot_box

#Save profit boxplot

ggsave("profit_boxplot.png", plot = profit_plot_box, width = 10, height = 8, dpi = 300)


### Smooth Plot ###

profit_plot_smooth= ggplot(microdosing_profit, aes(x = Profit, fill = statusquo_full_dose_microdosing_options, color= statusquo_full_dose_microdosing_options)) +                       
  geom_density(alpha = 0.05)+  
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Farm Profit ($) compared to -452 to -474 $ statusquo") ###change this to actual statusquo value

profit_plot_smooth <- profit_plot_smooth+ theme_bw() +theme(legend.position = c(.7, .8))+
  theme(legend.title = element_blank())+
  ggtitle("Farm- level profit of status quo, full dose and microdose")+
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

profit_plot_smooth

#Save profit smooth plot 
ggsave("profit_smoothplot.png", plot = profit_plot_smooth, width = 10, height = 8, dpi = 300)


#### Environmental benefits visualization#### 

environmental_table<-microdosing_profit [,c(9:13)] 

env_data_frame <- data.frame(environmental_table)

microdosing_env <- env_data_frame %>% 
  pivot_longer(cols = y.environmental_benefit_status_quo:y.environmental_benefit_option_2, names_to = "ISFM_Components",
               values_to = 'Environmental_benefits')

#### Plotting  environmental benefits ####

#Box plot environmental benefits
env_plot_box= ggplot(microdosing_env, aes(x = Environmental_benefits, fill = ISFM_Components, color= ISFM_Components)) +                       
  geom_boxplot()+
  scale_fill_colorblind()+
  ylab("Probability density")+
  xlab("Environmental benefits (in USD)")

env_plot_box

#save environmental plot benefits
ggsave("environmental_boxplot.png", plot = env_plot_box, width = 10, height = 8, dpi = 300)


