#' @title calc niche width
#' @description  Calculate niche width
#' @details INPUT: 1) predation mortality data, 2) common.habitat, 3) meso.predators, 4) demersal.sp
#' @details OUTPUT: 1) niche width data across availability values
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


calc_niche_width <- function(this.predator,heatmap.prey, mult.seq,this.run){
  
  #this.predator = "AMB"
  print(paste("Calculate niche width for",this.predator))
  
  no.avs <- list()
  
  for(eachavailability in 1:length(mult.seq)){
    
    #eachavailability = 1
    
    this.avail <- mult.seq[eachavailability]
    print(this.avail)
    
    this.niche.width <- heatmap.prey %>% 
      filter(predator==this.predator, prop_end!=0, availability==this.avail, run==this.run) %>% 
      mutate(ln_prop_end = log(prop_end), pi = prop_end * ln_prop_end) %>% 
      group_by(predator,availability) %>% 
      summarise(H=-sum(pi), no_resources = n()) 
    
    no.avs[[eachavailability]] <- this.niche.width
    
  }
  
  this.niche.data <- no.avs %>% 
    bind_rows()
  
  return(this.niche.data)
  
}
