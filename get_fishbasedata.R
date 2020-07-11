#' @title Retrieve data from FishBase
#' @description  Functions to retrieve species information for fishbase
#' @details INPUT: 1) List of species
#' @details OUTPUT: 1) Table of species characteristics
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com

# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#' LIBRARIES
#' -----------------
# List of packages for session


#use to test
#list_fields("milieu")
#eachname <- "Dasyatis americana"


get_fishbase <- function(eachname){
 
  new.name <- validate_names(eachname)
  
  sp.table <- species(new.name)
  
  res.table <- sp.table %>% 
    dplyr::select(Species,DemersPelag) %>% 
    mutate(old_name = eachname)
  
  return(res.table)
}
