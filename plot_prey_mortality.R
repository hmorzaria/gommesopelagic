#' @title plot prey mortality
#' @description  Plot prey mortality as a point plot
#' @details INPUT: 1) prey mortality data, 2) common.habitat, 3) meso.predators, 4) demersal.sp
#' @details OUTPUT: 1) plots predation mortality by prey guild for the whole model domain
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


plot_prey_mortality <- function(heatmap.prey, common.habitat, atlantis.groups, 
                                meso.predator.fish, demersal.sp, this.run, fish.groups){


heatmap.data.prey <- heatmap.prey %>% 
  left_join(common.habitat, by = "long_name") %>% 
  filter(grepl("pelagic",habitat_classification) | grepl("Pelagic",habitat_classification) |   grepl("Reef",habitat_classification)) %>% 
  dplyr::select(-habitat_classification) %>% 
  rename(long_name_pred = long_name) %>% 
  filter(long_name_pred %in% meso.predator.fish) %>% 
  filter(!long_name_pred %in% demersal.sp) %>% 
  filter(run==this.run) %>% 
  left_join(atlantis.groups, by="prey") %>% 
  left_join(common.habitat, by = "long_name") %>% 
  mutate(habitat_classification = if_else(is.na(habitat_classification), Guild, habitat_classification)) %>% 
  mutate(habitat_classification= as.factor(habitat_classification)) %>% 
  droplevels()
  
plot1.groups <- heatmap.data.prey %>% 
  distinct(long_name_pred) %>% 
  pull(long_name_pred)

heatmap.data.prey.rev <- heatmap.data.prey %>% 
  mutate(habitat_classification = as.character(habitat_classification)) %>% 
  mutate(habitat_classification = if_else(habitat_classification %in% c("Epibenthos","Squid","Infauna","Filter feeders"),"Invertebrates",
                                          if_else(habitat_classification %in% c("Seabirds","Marine mammals","Sea turtles"),"Other vertebrates",
                                                                                if_else(habitat_classification %in% c("Primary producers","Plankton","Detritus","Bacteria"),"Primary producers & detritus",habitat_classification))
                                          )) %>% 
  mutate(habitat_classification = as.factor(habitat_classification))
                                      

red.pal1 <- redmonder.pal(8,"qMSOSlp")
red.pal2 <- redmonder.pal(8,"qMSOMed")
red.pal3 <- redmonder.pal(8,"qPBI")
red.pal4 <- redmonder.pal(8,"qMSOAsp")


col.pal <- c(
  red.pal1[6], #bathydemersal
  red.pal1[7], #Benthopelagic
  red.pal1[4], #Demersal
  red.pal2[6], #Invertebrates
  red.pal4[4], #mesopelagics
  red.pal2[2], #Other vertebrates
  red.pal1[3], #Pelagic neritic
  red.pal1[2], #pelagic oceanic
  red.pal2[5], # Primary producers
  red.pal1[5] #reef associated
 )

# heatmap.data.sum <- heatmap.data.prey %>% 
#   group_by(availability, long_name_pred, habitat_classification) %>% 
#   summarize(tot_prop = sum(prop_end)) %>% filter(long_name_pred=="Gag grouper")

prey.mortality <- heatmap.data.prey.rev %>% 
 # mutate(availability=as.factor(availability)) %>% 
  ggplot(aes(x=availability, y=norm_end_mortality, fill=habitat_classification)) +
  #geom_area()+
  geom_col(position="stack", width = 1)+
  scale_fill_manual(values=col.pal, name="Prey habitat classification") +
  #scale_shape_manual(values=myshapes, name="Prey guild")+
  facet_wrap(long_name_pred ~.,scales="free_y")+
  theme_classic() +
  ylab("Predation mortality") + 
  xlab("Prey availability")+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks = c(-0.5,0,0.5,1),labels = c(0,0.2,0.4,0.6)) +
  theme(text = element_text(size = 12, family = "Helvetica"))  


ggsave(paste0("pprey_meso_mortality_plot_",this.run,".png"), prey.mortality,  device="png",height = 9, width=11)

heatmap.data.prey <- heatmap.prey %>% 
  filter(predator %in% fish.groups) %>% 
  filter(!long_name %in% plot1.groups) %>% 
  rename(long_name_pred = long_name) %>% 
  filter(run==this.run) %>% 
  left_join(atlantis.groups, by="prey") %>% 
  left_join(common.habitat, by = "long_name") %>% 
  mutate(habitat_classification = if_else(is.na(habitat_classification), Guild, habitat_classification)) %>% 
  mutate(habitat_classification= as.factor(habitat_classification)) %>% 
  droplevels()

heatmap.data.prey.rev <- heatmap.data.prey %>% 
  mutate(habitat_classification = as.character(habitat_classification)) %>% 
  mutate(habitat_classification = if_else(habitat_classification %in% c("Epibenthos","Squid","Infauna","Filter feeders"),"Invertebrates",
                                          if_else(habitat_classification %in% c("Seabirds","Marine mammals","Sea turtles"),"Other vertebrates",
                                                  if_else(habitat_classification %in% c("Primary producers","Plankton","Detritus","Bacteria"),"Primary producers & detritus",habitat_classification))
  )) %>% 
  mutate(habitat_classification = as.factor(habitat_classification))

prey.mortality <- heatmap.data.prey.rev %>% 
  # mutate(availability=as.factor(availability)) %>% 
  ggplot(aes(x=availability, y=norm_end_mortality, fill=habitat_classification)) +
  #geom_area()+
  geom_col(position="stack", width = 1)+
  scale_fill_manual(values=col.pal, name="Prey habitat classification") +
  #scale_shape_manual(values=myshapes, name="Prey guild")+
  facet_wrap(long_name_pred ~.,scales="free_y")+
  theme_classic() +
  ylab("Predation mortality") + 
  xlab("Prey availability")+
  theme(legend.position="bottom")+
  scale_x_continuous(breaks = c(-0.5,0,0.5,1),labels = c(0,0.2,0.4,0.6)) +
  theme(text = element_text(size = 12, family = "Helvetica"))  


ggsave(paste0("pprey_other_mortality_plot_",this.run,".png"), prey.mortality,  device="png",height = 9, width=11)


}
# ggplot(heatmap.data, aes(fill=prey_name, y=prop_change, x=long_name, group=num_availability)) + 
#    geom_bar(stat="identity", position = "dodge")+
#    theme_classic() +
#   coord_flip()+
#   facet_wrap(num_availability ~.)


# cl <- colors(distinct = TRUE)
# set.seed(345215) # to set random generator seed
# mycols2 <- sample(cl, length(unique(heatmap.data$prey_guild)))

# mycols2 <- c("tan4", #bacteria
#              "magenta3", #demersal
#              "orange", #epibenthos
#              "deepskyblue1", #filterfeeders
#              "dodgerblue4", #mesopelagic
#              "skyblue3", #pelagic
#              "darkseagreen", # plankton
#              "seagreen4", #primary producers
#              "cyan3", # reef fish
#              "violetred4", # sea turtles
#              "darkorchid2", # seabirds
#              "darkgoldenrod3") # squid



# ggplot(heatmap.data, aes(x=base_prop_end, xend=prop_end, y=availability, group=prey_guild)) + 
#   geom_dumbbell(point.colour.r="#a3c4dc", 
#                 size=0.75, 
#                 point.colour.l="#0e668b") + 
#   facet_wrap(long_name ~.)
# scale_x_continuous(label=prop_end)
# 
# ggplot(heatmap.data, aes(x=availability, y=prop_end, group=prey, color = prey_guild)) + 
#   geom_boxplot() +
#   scale_color_manual(values=mycols2) +
#   #  scale_color_brewer(palette = "Dark2")+
#   facet_wrap(long_name ~.)
# 
# ggplot(heatmap.data, aes(area = prop_end_per, fill = prey_guild, label = prey_guild)) +
#   geom_treemap() +
#   geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
#                     grow = TRUE)+
#   facet_wrap(long_name ~.)
# 
# ggplot(heatmap.data, aes(prop_end, colour = long_name)) + stat_ecdf()+
#   facet_wrap(availability ~.)
# 
# # Plot again
# ggplot(heatmap.data, aes(x=availability, y=prop_end_per, fill=prey_guild)) + 
#   geom_area()+
#   scale_color_brewer(palette = "Dark2")+
#   facet_wrap(long_name ~.)
# 
# ggplot(heatmap.data, aes(fill=prey_guild, y=prop_end, x=availability)) + 
#   geom_bar(position="stack", stat="identity")+
#   theme_classic() +
#   facet_wrap(long_name ~.)
# 
# # filter(prop_end!=0) %>%   
# heatmap.data %>% 
#   ggplot(aes(predator,prey, fill = prop_end)) + 
#   geom_tile(aes(fill = prop_end)) + 
#   scale_fill_gradient(low = "lightblue", high = "firebrick4")+
#   theme_classic() +
#   facet_wrap(availability ~.)


