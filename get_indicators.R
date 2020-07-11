#' @title Get ecosystem indicator data
#' @description  Functions calcuate indicator
#' @details INPUT: 1) Biomass data, Catch data, groupings by indicator
#' @details OUTPUT: 1) Table of indicatos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date May 2020

get_indicators <- function(this.avvalue,biomass.data,catch.data, reef.groups, pelagic.groups, demersal.groups, planktivore.groups, piscivore.groups, shrimp.groups, forage.groups, elasmobranch.groups){
  
  #Reef fish catch
  
  this.biom.data <- biomass.data %>% 
    filter(preyvalue==this.avvalue) 
  
  this.catch.data <- catch.data %>% 
    filter(preyvalue==this.avvalue) 
  
  reef.catch <- this.catch.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name %in% reef.groups) %>% 
    summarise(oil_value=sum(tot_catch),base_value=sum(tot_catch_base)) %>% 
    mutate(indicator="Reef fish catch")
  
  #pelagic to demersal ratio
  
  pel.biom <- this.biom.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name %in% pelagic.groups) %>% 
    summarise(oil_value=sum(biomass),base_value=sum(biomass_base)) %>% 
    mutate(habit = "pelagic") %>% 
    pivot_longer(cols=c(oil_value,base_value),names_to = "scenario",values_to="biomass_pel")
  
  dem.biom <- this.biom.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name %in% demersal.groups) %>% 
    summarise(oil_value=sum(biomass),base_value=sum(biomass_base)) %>% 
    mutate(habit = "demersal") %>% 
    pivot_longer(cols=c(oil_value,base_value),names_to = "scenario",values_to="biomass_dem")
  
  pel.dem.ratio <- dem.biom %>% 
    left_join(pel.biom, by="scenario") %>% 
    mutate(value=biomass_pel / biomass_dem) %>% 
    dplyr::select(value,scenario) %>% 
    pivot_wider(names_from=scenario,values_from = value) %>% 
    mutate(indicator = "Pelagic to demersal ratio")
  
  #piscivore to planktivore ratio
  plank.biom <- this.biom.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name %in% planktivore.groups) %>% 
    summarise(oil_value=sum(biomass),base_value=sum(biomass_base)) %>% 
    mutate(habit = "planktivore") %>% 
    pivot_longer(cols=c(oil_value,base_value),names_to = "scenario",values_to="biomass_pla")
  
  pisci.biom <- this.biom.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name %in% piscivore.groups) %>% 
    summarise(oil_value=sum(biomass),base_value=sum(biomass_base)) %>% 
    mutate(habit = "piscivore") %>% 
    pivot_longer(cols=c(oil_value,base_value),names_to = "scenario",values_to="biomass_pis")
  
  pisc.plan.ratio <- plank.biom %>% 
    left_join(pisci.biom, by="scenario") %>% 
    mutate(value=biomass_pis / biomass_pla) %>% 
    dplyr::select(value,scenario) %>% 
    pivot_wider(names_from=scenario,values_from = value) %>% 
    mutate(indicator = "Piscivore to planktonic ratio")
  
  #Red snapper biomass
  
  redsnapper.biom <- this.biom.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name == "red snapper") %>% 
    summarise(oil_value=sum(biomass),base_value=sum(biomass_base)) %>% 
    mutate(indicator = "Red snapper biomass")
  
  #Shrimp biomass
  
  shrimp.biom <- this.biom.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name %in% shrimp.groups) %>% 
    summarise(oil_value=sum(biomass),base_value=sum(biomass_base)) %>% 
    mutate(indicator = "Shrimp biomass")
  
  #Forage fish biomass
  
  forage.biom <- this.biom.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name %in% forage.groups) %>% 
    summarise(oil_value=sum(biomass),base_value=sum(biomass_base)) %>% 
    mutate(indicator = "Forage fish biomass")
  
  #Elasmobranch biomass
  
  elasmo.biom <- this.biom.data %>% 
    mutate(long_name=tolower(long_name)) %>% 
    filter(long_name %in% elasmobranch.groups) %>% 
    summarise(oil_value=sum(biomass),base_value=sum(biomass_base)) %>% 
    mutate(indicator = "Elasmobranch biomass")
  
  indicator.data <- bind_rows(elasmo.biom, forage.biom,shrimp.biom,redsnapper.biom,pisc.plan.ratio, pel.dem.ratio, reef.catch) %>% 
    mutate(availability = this.avvalue)
  
  return(indicator.data)
}