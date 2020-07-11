#' @title Get biomass data
#' @description  Extract biomass data from outputs
#' @details INPUT: 1)  Biomass data, 2) Time steps desired
#' @details OUTPUT: 1) Table of indicatos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date May 2020



get_biomass <- function(eachfile, prey.list, max.time, this.scenario){
  
  print(prey.list[eachfile])
  
  this.file <- prey.list[eachfile]
  
  run.num <- this.file %>% 
    str_split("/") %>% 
    unlist %>% 
    .[7] %>% 
    str_split("_") %>% 
    unlist %>% 
    .[2] %>% 
    as.numeric
  
  pprey.value <- mult.seq[run.num]
  
  print(pprey.value) 
  
  prey.data <- fread(prey.list[eachfile], select= c(1:87)) %>% 
    pivot_longer(c(-Time,-Box), names_to = "species",values_to="biomass") %>% 
    mutate(preyvalue=pprey.value, scenario = this.scenario)
    
  
  prey.time.data <- prey.data %>% 
    filter(Time==0 | Time %in% max.time)
  
  return(prey.time.data)
  
}
