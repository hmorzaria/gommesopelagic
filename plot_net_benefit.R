#' @title Estimate net benefit
#' @description  Calcuate net benefit
#' @details INPUT: 1) Biomass data, Catch data, Catch by fleet
#' @details OUTPUT: 1) Gross benefit , 2) Discounted Net benefit
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date August 2020



plot_net_benefit <- function(box.catch, west.fl.pol, value.group, common.habitat, fish.groups){
  
  catch.data.wf <- box.catch %>% 
    filter(Box %in% west.fl.pol) %>% 
    group_by(Code,preyvalue,long_name,guild,habit) %>% 
    summarise(tot_catch = sum(catch), tot_catch_base = sum(catch_base)) %>% 
    ungroup
  
  value.catch.wf <- catch.data.wf %>% 
    left_join(value.group, by="long_name") %>% 
    filter(!is.na(dollar_kg)) %>% 
    mutate(gross_value_mt=tot_catch*(dollar_kg*1000)) %>% 
    mutate(gross_value_base_mt=tot_catch_base*(dollar_kg*1000)) 
  
  write_csv(value.catch.wf, "catch_value_table_wf.csv")
  
  net.benefit.wf <- value.catch.wf %>% 
    left_join(common.habitat, by="long_name") %>% 
    filter(Code %in% fish.groups) %>% 
    mutate(habitat_classification = as.factor(habitat_classification)) %>% 
    group_by(preyvalue, habitat_classification) %>% 
    summarise(gross_value_mt = sum(gross_value_mt), gross_value_base_mt = sum(gross_value_base_mt)) %>% 
    ungroup %>% 
    mutate(undiscounted_NBt =  gross_value_mt * (1 - cost.rate)) %>% 
    mutate(undiscounted_NBt_base =  gross_value_base_mt * (1 - cost.rate)) %>% 
    mutate(ratio_NBt = ((undiscounted_NBt / undiscounted_NBt_base)*100)-100)
  
  
  red.pal1 <- redmonder.pal(8,"qMSOSlp")
  #red.pal2 <- redmonder.pal(9,"sPBIGn")
  #red.pal3 <- redmonder.pal(8,"qMSOBuWarm")
  #red.pal4 <- redmonder.pal(9,"sPBIYlGn")
  
  
  col.pal <- c(red.pal1[7],red.pal1[4],red.pal1[1],red.pal1[3],red.pal1[2],red.pal1[5])
  
  ratio.plot.net.wf <- net.benefit.wf %>% 
    ggplot() + 
    geom_col(aes(x=preyvalue, y=ratio_NBt, color= habitat_classification, fill = habitat_classification))+
    scale_color_manual(values=col.pal, name = "Fishery type")+
    scale_fill_manual(values=col.pal, name = "Fishery type")+
    facet_wrap(habitat_classification ~., scales="free_y")+
    #facet_wrap(habitat_classification ~.)+
    theme_classic()+
    theme(legend.position = "none")+
    ylab("Percent change undiscounted Net benefit") + 
    xlab("Prey availability value") 
  
  ggsave("pprey_netb_ratio_plot_wf.png", ratio.plot.net.wf, device="png",width=10,dpi=350)
  
  
  
  catch.data <- box.catch %>% 
    group_by(Code,preyvalue,long_name,guild,habit) %>% 
    summarise(tot_catch = sum(catch), tot_catch_base = sum(catch_base)) %>% 
    ungroup
  
  value.catch <- catch.data %>% 
    left_join(value.group, by="long_name") %>% 
    filter(!is.na(dollar_kg)) %>% 
    mutate(gross_value_mt=tot_catch*(dollar_kg*1000)) %>% 
    mutate(gross_value_base_mt=tot_catch_base*(dollar_kg*1000)) 
  
  write_csv(value.catch, "catch_value_table.csv")
  
  net.benefit <- value.catch %>% 
    left_join(common.habitat, by="long_name") %>% 
    filter(Code %in% fish.groups) %>% 
    mutate(habitat_classification = as.factor(habitat_classification)) %>% 
    group_by(preyvalue, habitat_classification) %>% 
    summarise(gross_value_mt = sum(gross_value_mt), gross_value_base_mt = sum(gross_value_base_mt)) %>% 
    ungroup %>% 
    mutate(undiscounted_NBt =  gross_value_mt * (1 - cost.rate)) %>% 
    mutate(undiscounted_NBt_base =  gross_value_base_mt * (1 - cost.rate)) %>% 
    mutate(ratio_NBt = ((undiscounted_NBt / undiscounted_NBt_base)*100)-100)
  
  
  red.pal1 <- redmonder.pal(8,"qMSOSlp")
  #red.pal2 <- redmonder.pal(9,"sPBIGn")
  #red.pal3 <- redmonder.pal(8,"qMSOBuWarm")
  #red.pal4 <- redmonder.pal(9,"sPBIYlGn")
  
  
  col.pal <- c(red.pal1[7],red.pal1[4],red.pal1[1],red.pal1[3],red.pal1[2],red.pal1[5])
  
  ratio.plot.net <- net.benefit %>% 
    ggplot() + 
    geom_col(aes(x=preyvalue, y=ratio_NBt, color= habitat_classification, fill = habitat_classification))+
    scale_color_manual(values=col.pal, name = "Fishery type")+
    scale_fill_manual(values=col.pal, name = "Fishery type")+
    facet_wrap(habitat_classification ~., scales="free_y")+
    #facet_wrap(habitat_classification ~.)+
    theme_classic()+
    theme(legend.position = "none")+
    ylab("Percent change in profit") + 
    xlab("Prey availability value") 
  
  ggsave("pprey_netb_ratio_plot.png", ratio.plot.net, device="png",width=10,dpi=350)
  
}


#write.csv(NBt,file= paste("NBt.groups",sc.list[ScenarioIndex],".csv"))

# discount.rate = 1/(1 + interest.rate)
# NBt$year = -1:29
# NBt$NPVt = NBt$NBt * (discount.rate^NBt$year)




