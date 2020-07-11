#' @title Get predation mortality data
#' @description  Estimate prey mortality contribution
#' @details INPUT: 1) Mortper pred data, availability values
#' @details OUTPUT: 1) Predation values by group
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date May 2020


get_mort <- function(this.scenario,mult.seq, col.names) {
  
  print(this.scenario)
  
  setwd(this.scenario)
  
  this.run <- this.scenario %>% 
    str_split("/") %>% 
    unlist %>% 
    .[2] %>% 
    str_split("_") %>% 
    unlist %>% 
    .[3]
  
  this.simulation <- this.scenario %>% 
    str_split("/") %>% 
    unlist %>% 
    .[2] %>% 
    str_split("pprey") %>% 
    unlist %>% 
    .[2] %>% 
    as.numeric
  
  this.availability <- mult.seq[this.simulation]
  
  
  mort.prey <- fread("GOM_OUTMortPerPred.txt", skip = 3, fill = TRUE) %>% 
    as_tibble() %>% 
    setNames(col.names) %>% 
    filter(!is.na(startN)) %>% 
    mutate(Time = as.numeric(Time)) %>% 
    dplyr::select(-startN, -mL, -mQ, -implicitMortality,-TotalPredMort)
  
  initial.mort <- mort.prey %>% 
    filter(Time==30) %>% 
    pivot_longer(cols=c(-Time,-prey), names_to = "predator",values_to = "ini_mortality") %>% 
    dplyr::select(-Time) %>% 
    mutate(ini_prey = if_else(ini_mortality!=0,1,ini_mortality))
  
  end.mort <- mort.prey %>% 
    filter(Time==5474.5)%>% 
    pivot_longer(cols=c(-Time,-prey), names_to = "predator",values_to = "end_mortality") %>% 
    dplyr::select(-Time) %>% 
    mutate(end_prey = if_else(end_mortality!=0,1,end_mortality))
  
  tot.mort <- initial.mort %>% 
    left_join(end.mort, by=c("prey","predator")) %>% 
    mutate(new_prey = if_else(ini_prey==0 & end_prey ==1, 1,0),
           lost_prey = if_else(ini_prey==1 & end_prey ==0,1,0))
  
  prey.summary <- tot.mort %>% 
    group_by(predator) %>% 
    summarize(tot_ini = sum(ini_prey), 
              tot_end = sum(end_prey), 
              tot_new=sum(new_prey), 
              tot_lost = sum(lost_prey)) %>% 
    mutate(availability = this.availability, run = this.run)
  
    sum.mort <- tot.mort %>% 
      group_by(predator) %>% 
      summarise(sum_ini_mort=sum(ini_mortality),sum_end_mort=sum(end_mortality))

    mort.summary <- tot.mort %>% 
      left_join(sum.mort, by="predator") %>% 
      mutate(prop_end = end_mortality/sum_end_mort) %>% 
      mutate(prop_end=if_else(is.nan(prop_end),0,prop_end)) %>% 
      arrange(predator,prey) %>% 
      mutate(availability = this.availability, run = this.run) %>% 
      group_by(predator) %>% 
      mutate(sum_prop_end = sum(prop_end)) %>% 
      filter(sum_prop_end!=0)
    
  print(paste(this.availability,this.run))
    
  setwd("~/gom_runs/runs_ppreyvalues")
  
  return(mort.summary)
}