#' Code to manage and run Atlantis simulations using parallel processing and doAzure
#'  
#' @author Hem Nalini Morzaria Luna
#' @date June 2017, updated March 2019
#' # https://github.com/Azure/doAzureParallel
#wget http://www.omegahat.net/XMLSchema/XMLSchema_0.7-0.tar.gz
#wget http://www.omegahat.net/SSOAP/SSOAP_0.9-0.tar.gz
#install.packages("~/XMLSchema_0.7-0.tar.gz", repos = NULL, type = "source")
#install.packages("~/SSOAP_0.9-0.tar.gz", repos = NULL, type = "source")


# set locale to avoid multibyte errors
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

#install.packages(c("XMLSchema", "SSOAP"), repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"))

# List of packages for session
.packages = c("devtools", "dtplyr","stringi","data.table","tidyverse","stringr","R.utils",
              "magrittr","future","parallel","doSNOW","XMLSchema", "SSOAP")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#set paths
workpath <- "~/amps_runs"
savepath <- "Atlantis"

#give write read permission to whole amps_runs folder

system("sudo chmod -R a+rwx ~/amps_runs", wait = TRUE)
#
#
system("sudo apt-get update; sudo apt-get upgrade -y", wait = TRUE)
 system("sudo apt-get install subversion build-essential subversion flip autoconf libnetcdf-dev libxml2-dev libproj-dev -y", wait = TRUE)
#
# system("sudo apt-get update; sudo apt-get upgrade -y; sudo apt-get autoremove -y", wait = TRUE)
# system("sudo dpkg –configure -a -y; sudo apt-get install -f -y", wait = TRUE)
# system("sudo apt-get autoremove") 

# # Get code from CSIRO SVN, if need to change version use r -5468 for example
#  system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk --username isaac.kaplan.768 --password TrevorBarker84! --quiet", wait = TRUE)
# system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk -r 6496 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/trunk --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
# system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/AtlantisSalmon hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)

# system("svn --no-auth-cache co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/bec_dev -r 6177 --username  isaac.kaplan.768 --password TrevorBarker84! --quiet", wait = TRUE)

# # # Rebuild recompile via MAKE:
#  system("cd trunk/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough'; sudo make -d install; cd atlantismain", wait = TRUE)
 system("cd trunk/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough'; sudo make -d install; cd atlantismain", wait = TRUE)
# system("cd AtlantisSalmon; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough'; sudo make -d install; cd atlantismain", wait = TRUE)
# system("cd trunk_ICKpopecol/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-DACCEPT_USE_OF_DEPRECATED_PROJ_API_H -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough'; sudo make -d install; cd atlantismain", wait = TRUE)

#sudo apt-get install valgrind
#G_SLICE=always-malloc G_DEBUG=gc-friendly  valgrind -v --tool=memcheck --leak-check=full --num-callers=40 --log-file=valgrind.log ./trunk/atlantis/atlantismain/atlantisMerged

# # # copy Atlantis executable
#  system(paste("sudo cp -u trunk/atlantis/atlantismain/atlantisMerged ~/amps_runs"), wait = TRUE)

#this will create multiple folders each with all Atlantis run files

no.folders <- c(1147:1154)

folder.length <- 1:length(no.folders)

sh.file <- "amps_cal.sh" #original sh file

folder.paths <- paste(savepath,"_B",no.folders,sep="")

system("sudo chmod -R a+rwx ~/amps_runs", wait = TRUE)

#run multiple Atlantis simulations on local machine cores

NumberOfCluster <- detectCores()  #- 1

# Initiate cluster
cl <- makeCluster(NumberOfCluster)
registerDoSNOW(cl)

# Run this for loop for one call of model from each cluster, assuming cluster is already initiated. 
atlantis.scenarios <- foreach(this.index=folder.length, .verbose = TRUE) %dopar% {
  
  # List of packages for session
  .packages = c("stringi","data.table","tidyverse","stringr","R.utils")
  
  # Install CRAN packages (if not already installed)
  .inst <- .packages %in% installed.packages()
  if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  this.folder <- folder.paths[this.index]
  setwd(paste(workpath,"/",this.folder,sep=""))
  
  # run Atlantis scenario
  system(paste("sudo flip -uv *; sudo chmod +x ", sh.file,"; sudo sh ./", sh.file, sep=""), wait = TRUE)
  
  done <- as.data.frame("done")
}


stopCluster(cl)

system("az vm deallocate --name atlantisserver04 --no-wait --resource-group morzariacedogroup")

#file.remove(list.files(path = "~/amps_runs/Atlantis_B299/outputFolder", pattern = "*.pdf", full.names = TRUE))

