#' Run Atlantis simulations as cluster
#' @author Hem Nalini Morzaria Luna
#' @date November 2019
#' email: hmorzarialuna@gmail.com




run_atlantiscluster_meso <- foreach(eachperm = permutation.list) %dopar% 
{
  
  # List of packages for session
  .packages = c("tidyverse")
  
  # Install CRAN packages (if not already installed)
  .inst <- .packages %in% installed.packages()
  if (length(.packages[!.inst]) > 0)
    install.packages(.packages[!.inst], repos = "http://cran.us.r-project.org")
  
  # Load packages into session 
  lapply(.packages, require, character.only=TRUE)
  
  dir.create("atlantis")
  setwd("~/atlantis")
  
  this.prm <- paste("GOM_PRM_2015_",eachperm,".prm",sep="")
  this.sh <- paste("goc_run_",eachperm,".sh",sep="")
  
  system(paste("azcopy", "--source  https://morzariagroupdiag419.blob.core.windows.net/gomprms", "--destination ~/atlantis", "--source-key e9QJLNwR9eGf82eSLNC6Wm0NFJoVxT5UWZarn+xsMoDTpmN7ePXEBdD+JtNLQlsmwpC5wWUBFa2QErPX7M8plQ== ", paste("--include", this.prm,sep = " "), "--recursive", "--quiet", "--exclude-older",sep=" \\"), wait=TRUE)
  system(paste("azcopy", "--source  https://morzariagroupdiag419.blob.core.windows.net/gomprms", "--destination ~/atlantis", "--source-key e9QJLNwR9eGf82eSLNC6Wm0NFJoVxT5UWZarn+xsMoDTpmN7ePXEBdD+JtNLQlsmwpC5wWUBFa2QErPX7M8plQ== ", paste("--include", this.sh,sep = " "), "--recursive", "--quiet", "--exclude-older",sep=" \\"), wait=TRUE)
  
  
  system(paste("azcopy", "--source  https://morzariagroupdiag419.blob.core.windows.net/gommodeloil", "--destination ~/atlantis", "--source-key e9QJLNwR9eGf82eSLNC6Wm0NFJoVxT5UWZarn+xsMoDTpmN7ePXEBdD+JtNLQlsmwpC5wWUBFa2QErPX7M8plQ== ", "--recursive", "--quiet", "--exclude-older",sep=" \\"), wait=TRUE)
  
  
 # system("sudo apt-get update; sudo apt-get upgrade -y", wait = TRUE)
 # system("sudo apt-get install subversion gdebi-core gdal-bin libcairo2 libcairo2-dev libapparmor1 libhdf5-dev libnetcdf-dev libgdal-dev  libudunits2-dev libxml2-dev libproj-dev flip automake -y", wait = TRUE)
 # version 6177 was used in previous publication
  system("svn co https://svnserv.csiro.au/svn/ext/atlantis/Atlantis/branches/bec_dev -r 6177 --username hmorzarialuna.992 --password Bybevhg6 --quiet", wait = TRUE)
 
  #have to switch to gcc version 6 for make to work and compile
  system("apt-get install g++-6 gcc-6")
  system("update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-6 2 --slave /usr/bin/g++ g++ /usr/bin/g++-6")
  
  system("cd ~/atlantis/bec_dev/atlantis; aclocal; autoheader; autoconf; automake -a; ./configure; sudo make CFLAGS='-Wno-error -Wno-format-overflow -Wno-misleading-indentation -Wno-format -Wno-implicit-fallthrough'; sudo make -d install; cd atlantismain", wait = TRUE)
  
  # copy Atlantis executable
  system("sudo cp -u ~/atlantis/bec_dev/atlantis/atlantismain/atlantisNew ~/atlantis", wait = TRUE)
  
  # run Atlantis scenario
  system(paste("cd ~/atlantis","; sudo flip -uv *; sudo chmod +x ", this.sh,"; sudo sh ./", this.sh, sep=""), wait = TRUE)
  
  this.outfolder <- paste("output",eachperm,sep="_")
  
  system(paste("azcopy", paste("--source ~/atlantis/",this.outfolder,sep=""), paste("--destination https://morzariagroupdiag419.blob.core.windows.net/gomallmesores/",this.outfolder," ",sep=""),"--dest-key e9QJLNwR9eGf82eSLNC6Wm0NFJoVxT5UWZarn+xsMoDTpmN7ePXEBdD+JtNLQlsmwpC5wWUBFa2QErPX7M8plQ== ", "--recursive", "--quiet", "--exclude-older",sep=" \\"), wait=TRUE)
  
  
  }


