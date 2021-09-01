#' @title plot catch
#' @description  Plot catch as a point plot
#' @details INPUT: 1) ratio.data, 2) west.fl.pol, 3) common.habitat, 4) meso.predators, 5) demersal.sp
#' @details OUTPUT: 1) plots mesopelagic predators model domain 2) plots all fish groups model domain
#' @details Used https://briatte.github.io/ggnet/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



plot_catch <- function(ratio.data, common.habitat, atlantis.groups, meso.predator.fish, demersal.sp, west.fl.pol){
  
 
  ratio.plot.meso.data <- ratio.data %>% 
    group_by(preyvalue, long_name) %>% 
    summarise(tot_catch = sum(catch), tot_catch_base = sum(catch_base)) %>% 
    ungroup %>% 
    mutate(catch_ratio = tot_catch / tot_catch_base) %>% 
    left_join(common.habitat, by = "long_name") %>% 
    filter(grepl("pelagic",habitat_classification) | grepl("Pelagic",habitat_classification) |   grepl("Reef",habitat_classification)) %>% 
    filter(long_name %in% meso.predator.fish) %>% 
    filter(!long_name %in% demersal.sp) %>% 
    filter(!long_name =="Other tuna") %>% 
    mutate(habitat_classification = as.factor(habitat_classification))
  
  red.pal1 <- redmonder.pal(8,"qMSOSlp")
  
  col.pal <- c(red.pal1[3],red.pal1[2],red.pal1[5])
  #col.pal <- c(red.pal1[6],red.pal1[8],red.pal1[7],red.pal1[4], red.pal1[3],red.pal1[2],red.pal1[5])
  
  ratio.plot.meso <- ratio.plot.meso.data %>% 
    ggplot(aes(x=preyvalue, y=catch_ratio, color = habitat_classification)) + 
    geom_point()+
    scale_color_manual(values=col.pal, name = "Habitat classification")+
    facet_wrap(long_name ~., scales="free_y")+
    #facet_wrap(habitat_classification ~.)+
    theme_classic()+
    theme(legend.position = "bottom")+
    ylab("Biomass ratio") + 
    xlab("Prey availability value") +
    theme(text = element_text(size = 12, family = "Helvetica"))  
  # stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, colour="grey50")  #quadratic
  #  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE)+
  
  ggsave("pprey_catch_ratio_pelagic_plot.png", ratio.plot.meso, device="png", height = 8, width=9)
  
  ratio.plot.meso.data.wf <- ratio.data %>% 
    filter(Box %in% west.fl.pol) %>% 
    filter(long_name %in% meso.predator.fish) %>% 
    group_by(Code,preyvalue,scenario,scenario_base,long_name,guild,habit) %>% 
    summarise(biomass=sum(biomass),biomass_base = sum(biomass_base)) %>% 
    ungroup %>% 
    mutate(ratio = biomass/biomass_base) %>% 
    left_join(common.habitat, by = "long_name") %>% 
    filter(grepl("pelagic",habitat_classification) | grepl("Pelagic",habitat_classification) |   grepl("Reef",habitat_classification)) %>% 
    filter(!long_name %in% demersal.sp) %>% 
    filter(!long_name =="Other tuna") %>% 
    mutate(habitat_classification = as.factor(habitat_classification))
  
  
  ratio.plot.meso.wf <- ratio.plot.meso.data.wf %>% 
    ggplot(aes(x=preyvalue, y=ratio)) + 
    geom_point(aes(color = habitat_classification))+
    scale_color_manual(values=col.pal, name = "Habitat classification")+
    #facet_wrap(long_name ~.)+
    facet_wrap(long_name ~., scales="free_y")+
    #facet_wrap(habitat_classification ~., scales="free_y")+
    theme_classic()+
    theme(legend.position = "bottom")+
    ylab("Biomass ratio") + 
    xlab("Prey availability value") +
    theme(text = element_text(size = 12, family = "Helvetica"))  

  ggsave("pprey_catch_ratio_pelagic_plot_wf.png", ratio.plot.meso.wf, device="png",height = 8, width=9)
  
  
  col.pal2 <- c(red.pal1[6],red.pal1[7],red.pal1[4],red.pal1[3], red.pal1[2],red.pal1[5])
  
  meso.predators.dem <- meso.predators %>%
    dplyr::select(-habitat_classification) %>% 
    dplyr::rename(long_name = label) %>% 
    left_join(common.habitat, by="long_name") %>% 
    filter(grepl("pelagic",habitat_classification) | grepl("Pelagic",habitat_classification) |   grepl("Reef",habitat_classification)) %>% 
    pull(long_name)
  
    ratio.data.all <- biomass.ratio.data %>% 
    filter(!long_name %in% meso.predators.dem) %>% 
    group_by(Code,preyvalue,scenario,scenario_base,long_name,guild,habit) %>% 
    summarise(biomass=sum(biomass),biomass_base = sum(biomass_base)) %>% 
    ungroup %>% 
    mutate(ratio = biomass/biomass_base) %>%  
    left_join(common.habitat, by = "long_name") %>% 
    filter(!is.na(habitat_classification)) %>% 
    mutate(habitat_classification=as.factor(habitat_classification))
  
  ratio.plot.all <- ratio.data.all %>% 
    ggplot(aes(x=preyvalue, y=ratio, color = habitat_classification)) + 
    geom_point()+
    scale_color_manual(values=col.pal2, name = "Habitat classification")+
    facet_wrap(long_name ~., scales="free_y")+
    #facet_wrap(habitat_classification ~.)+
    theme_classic()+
    theme(legend.position = "bottom")+
    ylab("Biomass ratio") + 
    xlab("Prey availability value") +
    theme(text = element_text(size = 12, family = "Helvetica"))  
  
  ggsave("pprey_catch_ratio_plot.png", ratio.plot.all, device="png",height = 8, width=9)
  
  
  ratio.data.all.wf <- ratio.data %>% 
    filter(Box %in% west.fl.pol) %>% 
    filter(!long_name %in% meso.predators.dem) %>% 
    group_by(Code,preyvalue,scenario,scenario_base,long_name,guild,habit) %>% 
    summarise(biomass=sum(biomass),biomass_base = sum(biomass_base)) %>% 
    ungroup %>% 
    mutate(ratio = biomass/biomass_base) %>%  
    left_join(common.habitat, by = "long_name") %>% 
    filter(!is.na(habitat_classification)) %>% 
    mutate(habitat_classification=as.factor(habitat_classification))
  
  
  ratio.plot.all.wf <- ratio.data.all.wf %>% 
    ggplot(aes(x=preyvalue, y=ratio, color = habitat_classification)) + 
    geom_point()+
    scale_color_manual(values=col.pal2)+
    facet_wrap(long_name ~., scales="free_y")+
    #facet_wrap(long_name ~.)+
    theme_classic()+
    theme(legend.position = "bottom")+
    labs(color="Habitat classification")+
    ylab("Biomass ratio") + 
    xlab("Prey availability value")  +
    theme(text = element_text(size = 12, family = "Helvetica"))  
  
  ggsave("pprey_catch_ratio_plot_wf.png", ratio.plot.all.wf, device="png",height = 8, width=9)
  
  
  
}
