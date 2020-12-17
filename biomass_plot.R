#' @title plot biomass
#' @description  Plot biomass ratio oil/no oil
#' @details INPUT: 1) biomass data, 2) west florida polygons, 3)habitat classification, 4) group names, 5) demersal species names
#' @details OUTPUT: 1) biomass plots 
#' @details Used https://briatte.github.io/ggnet/ 
#' @details Used ggraph https://www.jessesadler.com/post/network-analysis-with-r/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



plot_biomass <- function(ratio.data,west.fl.pol,common.habitat,meso.predators, demersal.sp){
  
  meso.predators.vector <- meso.predators %>% 
    filter(!grepl("bird",label)) %>% 
    pull(label)
    
    ratio.plot.meso.data <- ratio.data %>% 
      group_by(Code,preyvalue,scenario,scenario_base,long_name,guild,habit) %>% 
      summarise(biomass=sum(biomass),biomass_base = sum(biomass_base)) %>% 
      ungroup %>% 
      mutate(ratio = biomass/biomass_base) %>% 
      left_join(common.habitat, by = "long_name") %>% 
      filter(grepl("pelagic",habitat_classification) | grepl("Pelagic",habitat_classification) |   grepl("Reef",habitat_classification)) %>% 
      mutate(habitat_classification = as.factor(habitat_classification)) %>% 
      filter(!long_name %in% demersal.sp)
    
    red.pal1 <- redmonder.pal(8,"qMSOSlp")
    red.pal2 <- redmonder.pal(8,"qMSOAsp")
    
    #col.pal <- c(red.pal1[1],red.pal2[7],red.pal2[4],red.pal1[3], red.pal1[4],red.pal1[5],red.pal1[6])
    
    col.pal <- c(red.pal2[7],red.pal2[4], red.pal1[4],red.pal1[5],red.pal1[6])
    
    ratio.plot.meso <- ratio.plot.meso.data %>% 
      ggplot(aes(x=preyvalue, y=ratio)) + 
      geom_point(aes(fill = habitat_classification), size = 1.5, pch=21, color="grey45")+
      scale_fill_manual(values=col.pal, name = "Habitat classification")+
      facet_wrap(long_name ~., scales="free_y")+
      #facet_wrap(habitat_classification ~.)+
      theme_classic()+
      theme(legend.position = "bottom")+
      ylab("Biomass ratio") + 
      xlab("Prey availability value") 
    # stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, colour="grey50")  #quadratic
    #  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE)+
    
    ggsave("pprey_ratio_pelagic_plot.png", ratio.plot.meso, device="png",width=20, height = 10, dpi=300)
    
    ratio.data.meso.wf <- ratio.data %>% 
      filter(Box %in% west.fl.pol) %>% 
      group_by(Code,preyvalue,scenario,scenario_base,long_name,guild,habit) %>% 
      summarise(biomass=sum(biomass),biomass_base = sum(biomass_base)) %>% 
      ungroup %>% 
      mutate(ratio = biomass/biomass_base) %>% 
      left_join(common.habitat, by = "long_name") %>% 
      filter(grepl("pelagic",habitat_classification) | grepl("Pelagic",habitat_classification) |     grepl("Reef",habitat_classification)) %>% 
      mutate(habitat_classification = as.factor(habitat_classification)) %>% 
      filter(!long_name %in% demersal.sp)
    
    ratio.plot.meso.wf <- ratio.data.meso.wf %>% 
      ggplot(aes(x=preyvalue, y=ratio)) + 
      geom_point(aes(fill = habitat_classification), size = 1.5, pch=21, color="grey45")+
      scale_fill_manual(values=col.pal, name = "Habitat classification")+
      facet_wrap(long_name ~., scales="free_y")+
      #facet_wrap(habitat_classification ~.)+
      theme_classic()+
      theme(legend.position = "bottom")+
      ylab("Biomass ratio") + 
      xlab("Prey availability value") 
    
    ggsave("pprey_ratio_pelagic_plot_wf.png", ratio.plot.meso.wf, device="png",width=20, height = 10, dpi=300)
    
    
    col.pal <- c(red.pal1[1],red.pal2[7],red.pal2[4],red.pal1[3], red.pal1[4],red.pal1[5],red.pal1[6])
    
    ratio.data.all <- ratio.data %>% 
      group_by(Code,preyvalue,scenario,scenario_base,long_name,guild,habit) %>% 
      summarise(biomass=sum(biomass),biomass_base = sum(biomass_base)) %>% 
      ungroup %>% 
      mutate(ratio = biomass/biomass_base) %>%  
      left_join(common.habitat, by = "long_name") %>% 
      filter(!is.na(habitat_classification))
    
    ratio.plot.all <- ratio.data.all %>% 
      ggplot(aes(x=preyvalue, y=ratio)) + 
      geom_point(aes(fill = habitat_classification), size = 1.5, pch=21, color="grey45")+
      scale_fill_manual(values=col.pal, name = "Habitat classification")+
      facet_wrap(long_name ~., scales="free_y")+
      #facet_wrap(habitat_classification ~.)+
      theme_classic()+
      theme(legend.position = "bottom")+
      ylab("Biomass ratio") + 
      xlab("Prey availability value") 
    
    ggsave("pprey_ratio_plot.png", ratio.plot.all, device="png",width=20, height = 10, dpi=300)
    
    ratio.data.all.wf <- ratio.data %>% 
      filter(Box %in% west.fl.pol) %>% 
      group_by(Code,preyvalue,scenario,scenario_base,long_name,guild,habit) %>% 
      summarise(biomass=sum(biomass),biomass_base = sum(biomass_base)) %>% 
      ungroup %>% 
      mutate(ratio = biomass/biomass_base) %>%  
      left_join(common.habitat, by = "long_name") %>% 
      filter(!is.na(habitat_classification))
    
    ratio.data.plot.wf <- ratio.data.all.wf %>% 
      ggplot(aes(x=preyvalue, y=ratio)) + 
      geom_point(aes(fill = habitat_classification), size = 1.5, pch=21, color="grey45")+
      scale_fill_manual(values=col.pal, name = "Habitat classification")+
      facet_wrap(long_name ~., scales="free_y")+
      #facet_wrap(habitat_classification ~.)+
      theme_classic()+
      theme(legend.position = "bottom")+
      ylab("Biomass ratio") + 
      xlab("Prey availability value") 
    
    ggsave("pprey_ratio_plot_wf.png", ratio.data.plot.wf, device="png",width=20, height = 10, dpi=300)
    
    
}
