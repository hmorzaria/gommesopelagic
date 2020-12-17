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
  
  combined.mort <- initial.mort %>% 
    left_join(end.mort, by=c("prey","predator")) 
    
  tot.mort <- combined.mort %>% 
    group_by(predator) %>% 
    summarize(tot_ini_mortality=sum(ini_mortality), tot_end_mortality=sum(end_mortality))
    
  tot.norm.mort <- combined.mort %>% 
    left_join(tot.mort, by=c("predator")) %>% 
    mutate(norm_ini_mortality = ini_mortality/ tot_ini_mortality,
           norm_end_mortality = end_mortality/ tot_end_mortality) %>% 
    mutate(new_prey = if_else(ini_prey==0 & end_prey ==1, 1,0),
           lost_prey = if_else(ini_prey==1 & end_prey ==0,1,0)) %>% 
    mutate(norm_ini_mortality=if_else(is.nan(norm_ini_mortality),0,norm_ini_mortality)) %>% 
    mutate(norm_end_mortality=if_else(is.nan(norm_end_mortality),0,norm_end_mortality)) %>% 
    mutate(availability=this.availability,run=this.run, run_no=this.simulation)
  
 print(paste(this.availability,this.run))
    
  setwd("~/gom_runs/runs_ppreyvalues")
  
  return(tot.norm.mort)
}