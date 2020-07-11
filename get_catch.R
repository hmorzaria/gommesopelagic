#' @title Get catch data
#' @description  Extract catch data from outputs
#' @details INPUT: 1)  Catch data, 2) Time steps desired
#' @details OUTPUT: 1) Table of indicatos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date May 2020


get_catch <- function(eachfile, prey.list, max.time,this.scenario){
  
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
  
  prey.data <- fread(prey.list[eachfile], select= c(1:70)) %>% 
    pivot_longer(-Time, names_to = "species",values_to="catch") %>% 
    mutate(preyvalue=pprey.value, scenario = this.scenario)
  
  
  prey.time.data <- prey.data %>% 
    filter(Time==0 | Time==max.time)
  
}