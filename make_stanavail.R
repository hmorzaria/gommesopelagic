#' @title Make prms with standard availability values
#' @description  Create prm files with pprey including standard availability values
#' @details INPUT: 1)  prm files, 2) availability values
#' @details OUTPUT: 1) prm files with updated pprey matrix
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date May 2020


make_stanavail <- function(this.mult,base.prey.guild,base.prey.guild.meso,mult.seq,workpath,savepath,headerfile, footerfile, prey.frame.names){
  
  this.seq <- mult.seq[this.mult]
  print(this.seq)
  
  these.prey.frame.names <- read_csv(prey.frame.names, col_names= FALSE) %>% 
    pull(X1)
  
  this.prey.data <- base.prey.guild.meso %>% 
    mutate(availability=this.seq) %>% 
    bind_rows(.,base.prey.guild) %>% 
    arrange(index) %>% 
    data.table() %>% 
    #select(`prey name`,availability) %>% 
    dcast.data.table(.,index+name ~ `prey name`, value.var="availability") %>% 
    dplyr::select(-index) %>% 
    as.data.frame()
  
  new.avail.frame <- this.prey.data[c("name",these.prey.frame.names)]
  
  setwd(workpath)
  print(paste("Generating availability: ",this.seq))
  
  new.file.name <- paste("GOM_PRM_2015_",this.mult,".prm",sep="")
  file.copy(from = file.path(workpath, headerfile), to = file.path(savepath, new.file.name), overwrite = TRUE)
  file.copy(from = file.path(workpath, footerfile), to = file.path(savepath, footerfile), overwrite = TRUE)
  
  for(eachline in 1:nrow(new.avail.frame)){
    
    this.name <- new.avail.frame[eachline,1] %>% as.character() %>% paste(.,"94")
    
    print(this.name)
    
    this.row <- new.avail.frame[eachline,2:ncol(new.avail.frame)] %>% 
      as.matrix() %>% toString() %>% gsub(",","",.)
    
    colnames(this.row) <- NULL
    
    setwd(savepath)
    cat(paste(this.name,sep=""), file=new.file.name, fill=TRUE, append=TRUE)
    cat(this.row, file=new.file.name, fill=TRUE, append=TRUE)
    
  }
  
  file.append(new.file.name,footerfile)
  
  this.outfolder <- paste("output",this.mult,sep="_")
  
  bash.name <- paste("goc_run_",this.mult,".sh",sep="")
  file.create(bash.name)
  cat(paste("atlantisNew -i  GOM_CDF.nc 0 -o GOM_OUT.nc -r at_runGOM.prm -f at_force.prm -p at_physics_nutsB.prm -b ",new.file.name, " -h at_harvest.prm -e econ.prm -s GulfGroups.csv -q GOM_fisheries.csv -d ", this.outfolder, sep = ""), file=bash.name, fill=TRUE, append=TRUE)
  
  setwd(workpath)
}