#' @title plot fishery catch
#' @description  Plot fishery catch as a point plot
#' @details INPUT: 1) fishery catch, 2) fleets
#' @details OUTPUT: 1) plots fishery catch by prey guild for the whole model domain
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


plot_fishery_catch <- function(fishery.ratio.data, gom_fisheries, fish.groups, invert.fleets)
{
  
  ratio.plot.meso.data <- fishery.ratio.data %>% 
    ungroup %>% 
    filter(Code %in% fish.groups) %>% 
    filter(!Fishery %in% invert.fleets) %>% 
    group_by(preyvalue, Fishery) %>% 
    summarise(tot_catch = sum(catch), tot_catch_base = sum(base_catch)) %>% 
    ungroup %>% 
    mutate(catch_ratio = tot_catch / tot_catch_base) %>% 
    filter(!is.nan(catch_ratio)) %>% 
    left_join(gom_fisheries, by="Fishery")
  
  
  red.pal2 <- redmonder.pal(8,"qPBI")
  
  col.pal <- c(red.pal2[1:4],red.pal2[6:8])
  
  ratio.plot.meso <- ratio.plot.meso.data %>% 
    ggplot(aes(x=preyvalue, y=catch_ratio, color= Location)) + 
    geom_point()+
    scale_color_manual(values=col.pal, name = "Fishery type")+
    facet_wrap(Name ~., scales="free_y")+
    #facet_wrap(habitat_classification ~.)+
    theme_classic()+
    theme(legend.position = "bottom")+
    ylab("Catch ratio") + 
    xlab("Prey availability value") +
    theme(text = element_text(size = 12, family = "Helvetica"))  
  
  
  ggsave("pprey_catch_ratio_plot.png", ratio.plot.meso, device="png",height = 6, width=8)
  
}
