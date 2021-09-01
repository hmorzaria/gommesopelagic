#' @title plot niche width
#' @description  Plot niche width as a point plot
#' @details INPUT: 1) niche.width.data, 2) common.habitat, 3) meso.predators, 4) demersal.sp
#' @details OUTPUT: 1) plot of niche width data across availability values
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com




plot_niche_width <- function(niche.data, atlantis.groups, common.habitat, 
                             meso.predator.fish, demersal.sp, file.name) {
  
  niche.width.stan <- niche.data %>% 
    left_join(atlantis.groups, by="predator") %>% 
    left_join(common.habitat, by = "predator name")
  
  niche.width.data <- niche.width.stan %>% 
    filter(`predator name` %in% meso.predator.fish) %>% 
    filter(!`predator name` %in% demersal.sp) %>% 
    filter(grepl("pelagic",habitat_classification) | grepl("Pelagic",habitat_classification) |   grepl("Reef",habitat_classification)) %>% 
    filter(!is.na(n)) %>% 
    mutate(`predator name` = str_to_sentence(`predator name`))
  
  red.pal1 <- redmonder.pal(8,"qMSOSlp")
  
  col.pal <- c(red.pal1[3],red.pal1[2],red.pal1[5])
  
  niche.plot <- ggplot(niche.width.data, aes(x=availability, y=standarized_H2, color=habitat_classification)) +
    geom_point() +
    scale_color_manual(values=col.pal, name="Habitat classification") +
    # scale_shape_manual(values=myshapes, name="Habitat classification")+
    facet_wrap(`predator name` ~., scales="free_y")+
    theme_classic() +
    ylab("Trophic niche width") + 
    xlab("Prey availability")+
    theme(legend.position="bottom")+
    theme(text = element_text(size = 12, family = "Helvetica"))  
  
  ggsave(file.name, niche.plot, device="png",height = 8, width=10)
  
}