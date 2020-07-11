#' @title Get pprey matrix
#' @description  Extract pprey matrix from biology prm file
#' @details INPUT: 1)  availability matrix
#' @details OUTPUT: 1) availability as a data frame
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date May 2020

get_pprey <- function(preynames, functionalgroups, availabilities) {
  

  prey.names <- fread(preynames, header=TRUE) %>% 
    dplyr::select(prey) %>% 
    mutate(prey = tolower(prey)) %>% 
    pull(prey)
  
  functional.groups <- fread(functionalgroups, header=TRUE) %>% tbl_df %>% 
    mutate(`predator name`=tolower(`Long Name`)) %>% 
    dplyr::select(`predator name`, Code) %>% 
    setNames(c("predator name", "predator"))
  
  prm.data <-  scan(availabilities, character(0), sep = "\n")
  
  # separate each line
  
  group.no <- "94"
  
  prey.test <- regexpr(pattern = "pPREY", text = prm.data) %>% 
    gsub("-1",FALSE,.) %>% 
    gsub("1",TRUE,.) %>% 
    gsub("2",TRUE,.) %>% 
    gsub("3",TRUE,.) %>% 
    as.logical() 
  
  prey.headers <- prm.data[prey.test] %>% 
    as.data.frame() %>% tbl_df %>% 
    na.omit %>% 
    setNames("prey") %>% 
    filter(!grepl("is the availability",prey)) %>% 
    separate(prey,c("name","groups")," ") %>% 
    mutate(name=gsub(",",'',name)) %>% 
    mutate(name=gsub("94",'',name)) %>% 
    dplyr::select(name)
  
  numeric.test <- regexpr(pattern = "pPREY", text = prm.data) %>% 
    gsub("-1",TRUE,.) %>% 
    gsub("1",FALSE,.) %>% 
    gsub("2",FALSE,.) %>% 
    gsub("3",FALSE,.) %>% 
    as.logical() 
  
  all.prey.data <- prm.data[numeric.test] %>% 
    as.data.frame() %>% 
    tbl_df %>% 
    na.omit %>% 
    setNames("prey") %>% 
    separate(prey,prey.names," ") %>% 
    mutate_each(funs(as.numeric)) %>% 
    na.omit %>% 
    bind_cols(prey.headers) %>% 
    dplyr::select(name, everything()) %>% 
    mutate(group = name, group = gsub("pPREY","",group), group = if_else(nchar(group)==3,paste(2,group,2,sep=""),group)) %>% 
    mutate(group = if_else(nchar(group)==2,paste(2,group,92,sep=""),group)) %>% 
    separate(group,c("prey type","predator"),1) %>% 
    separate(predator,c("predator","predator type"),3) %>% 
    mutate(predator = gsub("9","",predator)) %>% 
    mutate(`prey type`=ifelse(`prey type`==1,"juvenile","adult")) %>% 
    mutate(`predator type`=ifelse(`predator type`==1,"juvenile","adult")) %>% 
    left_join(functional.groups,by="predator") %>% 
    mutate(index= as.factor(1:nrow(.))) %>% 
    dplyr::select(index, name,predator,`predator name`,`predator type`,`prey type`, everything())
  
  prey.data <- all.prey.data %>%
    gather(`prey name`,availability,7:ncol(.)) %>% 
    mutate(name= as.factor(name))
  
  prey.data.stat <- prey.data %>% 
    group_by(`predator name`) %>% 
    mutate(mean_av = mean(availability)) %>% 
    mutate(min_av = min(availability)) %>% 
    mutate(max_av = max(availability)) %>% 
    ungroup

  prey.frame.names <- names(all.prey.data)[7:ncol(all.prey.data)]
  
  write_lines(prey.frame.names, "prey_frame_names.csv")
  
  write_csv(prey.data.stat, "prey_data_stat.csv")
  
  return(prey.data.stat)
    
}

