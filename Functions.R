#Given an inputted yearly prouction of oil in barrels or gallons, this function 
#calculates the amount of co2 produced by a company per ton of oil outputted
#This metric gives an overall sense of how "green" or efficient production is.
unit_co2_oil <- function(oil_out, units = c("barrels", "gallons"), 
                         emissions_yr, preds_co2, preds_oil){   #oil/day, emiss/year
  
  if(units == "barrels"){
    tons_oil_month <- (oil_out * 42 * 365)/(264.3405 * 12)
  } else{
    tons_oil_month <- (oil_out * 365)/12
  }
  
  preds_co2 <- data.frame(rnorm(12, emissions_yr/12, 10000))
  preds_oil <- data.frame(rnorm(12, tons_oil_month, 325000))
  co2_prod_per_ton <- (preds_co2/preds_oil)
  
  return(mean(co2_prod_per_ton[,1]))
}


#Calculates the contributions to energy production for electricity
#to_hydro and to_wind allows the user to designate how much of the 40% cut in energy production
#from fossil fuels should be off-loaded to hydroelectric and wind power
#The function then produces graphs of energy contributions before and after EPA reductions
prop_en_sources_ELEC <- function(coal_petrol, nat_gas, wind, hydro, total, to_hydro, to_wind){
  
  other <- total - coal_petrol - nat_gas - wind - hydro
  
  
  
  props <- data.frame(type = c("Coal and Petrol", "Natural Gas", "Wind", "Hydroelectric", "Other"), 
                      proportion = c(coal_petrol/total, nat_gas/total, wind/total, 
                                     hydro/total, other/total), id = rep("Before", 5)  ) 
  
  to_allocate <- (0.4*coal_petrol) + (0.4*nat_gas)
  allo_hydro <- to_hydro*to_allocate
  allo_wind <- to_wind*to_allocate
  
  props_chg <- data.frame(type = c("Coal and Petrol", "Natural Gas", "Wind", "Hydroelectric", "Other"), 
                          proportion = c(.6*coal_petrol/total, .6*nat_gas/total, (wind + allo_wind) /total, 
                                         (hydro+ allo_hydro)/total, other/total), id = rep("After", 5) ) 
  
  all <- data.frame(rbind(props, props_chg))
  
  all$type <- factor(all$type, levels = c("Other", "Wind", "Hydroelectric", "Natural Gas", "Coal and Petrol"))
  all$id <- factor(all$id, levels = c("Before", "After"))
  
  plot <- ggplot(all, aes(x=type, y=proportion, fill= id)) + 
    geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + 
    labs(title="Proportion of Annual MwH Produced\nby Energy Source", x="", 
         y="Fraction of Annual Energy Produced") + theme(legend.position = "bottom") + 
    scale_fill_discrete(name = "EPA Reduction\nin Effect") + coord_flip()
  
  print(plot)
  return(all)
}


tab <- prop_en_sources_ELEC(357,243,173,31,1000, 0.25, 0.75)

#Calculates how much co2 emissions are averted when using hydroelectric or wind power
#in lieu of fossil fuels before and after EPA reductions
co2_averted <- function(bef_af_tab, total){
  
  bef_af_tab %>% filter(., type == "Wind" | type == "Hydroelectric") %>% 
    mutate(., prop_tot_oil = proportion*total*1.0174, prop_tot_gas = proportion*total*0.4516) -> av

  plot_oil <- ggplot(av, aes(x=type, y=prop_tot_oil, fill= id)) + 
    geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + 
    labs(title="Emissions Averted by Replacing Coal and\nPetrol in Electricity Production", x="", 
         y="CO2 Emissions Averted\n(metric tonnes)") + theme(legend.position = "bottom") + 
    scale_fill_discrete(name = "EPA Reduction\nin Effect") 
  
  print(plot_oil)
  
  plot_gas <- ggplot(av, aes(x=type, y=prop_tot_gas, fill= id)) + 
    geom_bar(stat = "identity", width = 0.5, position = position_dodge()) + 
    labs(title="Emissions Averted by Replacing Natural\nGas in Electricity Production", x="", 
         y="CO2 Emissions Averted\n(metric tonnes)") + theme(legend.position = "bottom") + 
    scale_fill_discrete(name = "EPA Reduction\nin Effect") 
  
  print(plot_gas)
}
co2_averted(tab,10000)








