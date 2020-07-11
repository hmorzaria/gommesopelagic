#' @title Get catch data by polygon
#' @description  Extract catch data from outputs
#' @details INPUT: 1)  Catch data, 2) Time steps desired
#' @details OUTPUT: 1) Table of indicatos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date May 2020


get_sp_catch <- function(this.species, ncin, max.time) {
  
  #dim is box,timesteps (184)
  # last 27 timesteps are last three years (reported every 73 days)
  catch.data <- var.get.nc(ncin, variable = paste0("Tot_",this.species,"_Catch")) 
  
  rec.data <- var.get.nc(ncin, variable = paste0("Tot_",this.species,"_RecCatch")) 
  
  disc.data <- var.get.nc(ncin, variable = paste0("Tot_",this.species,"_Discards")) 
  
  var.data <- catch.data + rec.data + disc.data 
  
  start.time <- 184-26
  end.time <- 184
  
  this.var.data <- var.data[,start.time:end.time]
  
  catch.data <- as_tibble(this.var.data, .name_repair = "unique") %>% 
    setNames(max.time) %>% 
    mutate(Box=0:65) %>% 
    pivot_longer(cols=c(-"Box"),names_to="timestep",values_to = "catch") %>% 
    mutate(species=this.species)
  
  return(catch.data)
  
}

get_catch_box <- function(eachfile, prey.list, max.time,this.scenario, catch.species){
  
  print(prey.list[eachfile])
  
  this.file <- prey.list[eachfile]
  
  run.num <- this.file %>% 
    str_split("/") %>% 
    unlist %>% 
    .[7] %>% 
    str_split("_") %>% 
    unlist %>% 
    .[2] %>% 
    as.numeric
  
  pprey.value <- mult.seq[run.num]
  
  print(pprey.value) 
  
  ncfname <- prey.list[eachfile]
  
  # open netCDF file
  ncin <- open.nc(ncfname)
  print.nc(ncin) #lists basic info about file
  
  prey.data <- lapply(catch.species, get_sp_catch,ncin, max.time) %>% 
    bind_rows %>% 
    mutate(preyvalue=pprey.value, scenario = this.scenario)
  
}