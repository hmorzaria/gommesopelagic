#' Code to read and modify availabilities matrix
#'  
#' @author Hem Nalini Morzaria Luna
#' @date January 2017
#' PDF distributions were calculated in FitDietDirichelet7.r
#' Uses then Atlantis_runs.R to get runs 
#' 


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


make_availabilities <- function(prey.data.stat, permutations, betapath, workpath, savepath) {

  setwd(betapath)
  
  beta.files <- list.files(pattern="*PDF.csv")
  
  # tried to use parallel computing for this loop but the hydromad package wouldn't load properly
  
  beta.result.table <- list()
  
  #retrieve beta distributions for each predator
  for(thisfile in 1:length(beta.files)){
    
    print(thisfile)
    
    pred.name <- beta.files[thisfile] %>% 
      strsplit(.,"_") %>% 
      unlist() %>% .[1] %>% 
      tolower()
    
    pbeta.file <- fread(beta.files[thisfile], header=TRUE, verbose=TRUE, showProgress = TRUE) %>% 
      tbl_df %>% 
      dplyr::select(predator,`prey name`, availability,mode) %>% 
      mutate(`predator`=tolower(predator)) %>% 
      mutate(`prey name`=tolower(`prey name`)) %>% 
      setNames(c("predator name","prey name","beta","mode")) %>% 
      na.omit
    
    mode.table <- pbeta.file %>% 
      distinct(`predator name`,`prey name`,mode)
    
    if(nrow(pbeta.file)!=0){
      
      beta.names <- pbeta.file %>% 
        distinct(`prey name`) %>% 
        pull(`prey name`)
      
      beta.result <- vector("list", length(beta.names))
      
      # sample beta distribution using latin hypercube
      for(eachname in 1:length(beta.names)){
        
        print(eachname)
        this.prey <-  beta.names[eachname]
        
        beta.list <-  pbeta.file %>% 
          filter(`prey name`==this.prey) %>% 
          pull(beta) %>% 
          list(a=.)
        
        beta.sets <- parameterSets(beta.list, permutations,method = c("latin")) %>% 
          mutate(`prey name`=this.prey, `predator name`=pred.name, index_num = 1:permutations) %>% 
          dplyr:: select(index_num,`predator name`,`prey name`,a) %>% tbl_df %>% 
          dplyr::rename(beta = a) %>% 
          inner_join(mode.table, by=c("predator name","prey name")) %>% 
          mutate(mode_ratio = beta/mode) %>% 
          mutate(mode_ratio=replace(mode_ratio, mode_ratio==Inf, 0))
        
        if(max(beta.sets$beta)!=0){
          
          beta.result[[eachname]] <- beta.sets
        }
      }
      
      beta.result.table[[thisfile]] <- beta.result %>% 
        rbindlist()
      
    }
    
    
  }
  
  beta.data.res <- beta.result.table %>% 
    Filter(. %>% is.null %>% `!`, .) %>% 
    rbindlist() %>% 
    tbl_df
  
  print("Done creating beta.result.table")
  # prey.data is tidy availability matrix
  
  juv.prey.data <- prey.data.stat %>% 
    filter(`predator type`!="adult") %>%  #this is data for juveniles, that will not be used
    dplyr::select(index,name,`prey name`,availability)
  
  adult.prey.data <- prey.data.stat %>% 
    filter(`predator type`=="adult")
  
  juv.adult.prey.data <- adult.prey.data %>%  
    filter(`prey type`=="adult") %>% 
    dplyr::select(index,name,predator,availability,`prey name`)
  
  ad.adult.prey.data <- adult.prey.data %>%
    filter(`prey type`=="juvenile") %>% 
    dplyr::select(index,name,predator,availability,`prey name`) %>% 
    left_join(juv.adult.prey.data,by=c("predator","prey name")) %>% 
    mutate(ja.avail.ratio = availability.x/availability.y) %>% # juvenile/adult ratio
    mutate(ja.avail.ratio=replace(ja.avail.ratio, ja.avail.ratio==Inf, 0)) %>% 
    mutate(ja.avail.ratio=replace(ja.avail.ratio, is.nan(ja.avail.ratio), 0)) %>% 
    dplyr::select(predator,`prey name`,ja.avail.ratio) %>% 
    right_join(adult.prey.data, by=c("predator","prey name"))
  
  beta.name.frame <- beta.data.res %>% 
    distinct(`predator name`,`prey name`)
  
  setwd(workpath)
  
  write_csv(beta.data.res, "beta_data.csv")
  write_csv(beta.name.frame, "beta_name_frame.csv")
  write_csv(ad.adult.prey.data, "ad_adult_prey_data.csv")
  write_csv(juv.prey.data, "juv_prey_data.csv")
  
  
  }


make_prm <- function(eachperm, headerfile, footerfile, beta.data, beta.name.frame, ad.adult.prey.data, juv.prey.data,prey.frame.names) {

    setwd(workpath)
    print(paste("Generating permutation: ",eachperm, "from ",permutations))
    
    new.file.name <- paste("GOM_PRM_2015_",eachperm,".prm",sep="")
    file.copy(from = file.path(workpath, headerfile), to = file.path(savepath, new.file.name), overwrite = TRUE)
    file.copy(from = file.path(workpath, footerfile), to = file.path(savepath, footerfile), overwrite = TRUE)
    
    print("sample i permutation using the index_num field")
    #sample i permutation using the index_num field
   
     beta.sample <- read_csv(beta.data) %>% 
      filter(index_num==eachperm)
    
     these.prey.frame.names <- read_csv(prey.frame.names, col_names= FALSE) %>% 
       pull(X1)
     
    # diet data is for adult predators, including both adult and juvenile prey, changing both equally and maintaining the ratio
    
    print("get adult data")
    #this are prey for which there are no beta values
    this.prey.data.non <- read_csv(ad.adult.prey.data) %>% 
      anti_join(beta.sample,by=c("predator name","prey name")) %>% 
      dplyr::select(index,name,`prey name`,availability) 
    
    this.juv.prey.data <- read_csv(juv.prey.data)
    
    print("data for which there are no beta values")
    print(head(this.prey.data.non))
    
    this.prey.data <- read_csv(ad.adult.prey.data) %>% 
      inner_join(beta.sample,by=c("predator name","prey name")) %>% 
      dplyr::select(name,`prey name`,availability, beta, mode_ratio, mean_av, max_av, min_av, mode, ja.avail.ratio, index) %>% 
      mutate(ja.avail.ratio=if_else(ja.avail.ratio==0,1,ja.avail.ratio)) %>% 
      mutate(new.av = if_else(availability==0 & beta!=0, (beta*mean_av), availability)) %>% 
      mutate(new.av = if_else(availability!=0 & beta !=0, ((availability*mode_ratio)/ja.avail.ratio), availability)) %>%
      mutate(new.av = if_else(is.na(new.av), 0, new.av)) %>%
      dplyr::select(index,name,`prey name`, new.av, max_av) %>% 
      mutate(new.av = if_else(new.av> max_av, max_av, new.av)) %>% 
      dplyr::select(index,name,`prey name`, new.av) %>% 
      setNames(c("index","name","prey name","availability")) %>%
      bind_rows(.,this.juv.prey.data,this.prey.data.non) %>% 
      as.data.frame %>% 
      tbl_df() %>% 
      arrange(index) %>% 
      data.table() %>% 
      #select(`prey name`,availability) %>% 
      dcast.data.table(.,index+name ~ `prey name`, value.var="availability") %>% 
      dplyr::select(-index) %>% 
      as.data.frame()
    #select(name, prey.frame.names)
    #spread(`prey name`,availability) 
    
    print("this.prey.data")
    print(head(this.prey.data))
    
    new.avail.frame <- this.prey.data[c("name",these.prey.frame.names)]
    
    
    for(eachline in 1:nrow(new.avail.frame)){
      
      this.name <- new.avail.frame[eachline,1] %>% as.character() %>% paste(.,"94")
      
      this.row <- new.avail.frame[eachline,2:ncol(new.avail.frame)] %>% 
        as.matrix() %>% toString() %>% gsub(",","",.)
      
      colnames(this.row) <- NULL
      
      setwd(savepath)
      cat(paste(this.name,sep=""), file=new.file.name, fill=TRUE, append=TRUE)
      cat(this.row, file=new.file.name, fill=TRUE, append=TRUE)
      
    }
    
    file.append(new.file.name,footerfile)
    
    
    #writeLines(prm.data, con = "GOM_PRM_2015.prm", sep = "\n", useBytes = FALSE)
    
    this.outfolder <- paste("output",eachperm,sep="_")
    
    bash.name <- paste("goc_run_",eachperm,".sh",sep="")
    file.create(bash.name)
    cat(paste("atlantisNew -i  GOM_CDF.nc 0 -o GOM_OUT.nc -r at_runGOM.prm -f at_force.prm -p at_physics_nutsB.prm -b ",new.file.name, " -h at_harvest.prm -e econ.prm -s GulfGroups.csv -q GOM_fisheries.csv -d ", this.outfolder, sep = ""), file=bash.name, fill=TRUE, append=TRUE)
    
  
  
  
  
}






