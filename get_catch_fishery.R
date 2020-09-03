#' @title Get catch data
#' @description  Extract catch data from outputs
#' @details INPUT: 1)  Catch per fishery data, 2) Time steps desired
#' @details OUTPUT: 1) Catch per fishery for desired time steps
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date August 2020



get_catch_fishery <- function(eachfile, prey.meso.list, max.time, this.scenario){
  
  print(prey.meso.list[eachfile])
  
  this.file <- prey.meso.list[eachfile]
  
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
  
  prey.data <- fread(prey.meso.list[eachfile]) %>% 
    pivot_longer(c(-Time,-Fishery), names_to = "species",values_to="catch") %>% 
    mutate(preyvalue=pprey.value, scenario = this.scenario) %>% 
    filter(!grepl("Dummy",Fishery))
    
  
  prey.time.data <- prey.data %>% 
    filter(Time %in% max.time) %>% 
    group_by(Fishery,species,preyvalue,scenario) %>% 
    summarise(catch=sum(catch))
    
  
  return(prey.time.data)
  
}
