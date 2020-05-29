PlotDiets<- function()
{
  #---------------
  #   This plots Biomass (all groups) and reserve N and Nums (all Vertebrates) over time. 
  #   ** See/check/change the user defined block of parameters below. **
  #  INPUT:  Uses R ncdf package to read outputCAM.nc, or any Atlantis Biology output.nc file. 
  #   It expects three things in the working directory: 
  #    	FuncGroupNamesInPlotOrder.csv  , with 5 columns: 1)NetCDFName (like "Planktiv_S_Fish_N"); 2) CODE, like "FVO"; and 3) commonName, like "Migrating Bird",  4) Virgin Biomass/ Current Biomass, like 10, 5) isFished  TRUE or FALSE,   (WITH headers; Put "NA" if you want some subplots to be blank)
  #     VertGroupNamesInPlotOrder.csv , with 2 columns: 1) NetCDFVertName, like "Planktiv_S_Fish1_ResN",and 2) commonName, like "Small Planktivore" (No headers)
  #   	AssessOrSurveyData.csv, with 1st column as year, columns 2-max are biomass of each functional group in each year. NAs are ok. 
  # OUTPUT:  THE LIST OF VARIABLES from the Atlantis output.nc file IS SAVED AS ListOfVarsFromNC.csv. 
  #            This list should be used to check the input.csv files to make sure all variable in model are being plotted. 
  #
  #    Notes: this is just a wrapper around R ncdf packages functions: open.ncdf,  get.var.ncdf, and close.ncdf
  # 
  # ToDO:This should really use matplot() to make plots faster
  # Isaac Kaplan isaac.kaplan@noaa.gov 
  #---------
  
  
  #library(ncdf)
  #library(sm)  # for pause()
  
  
  
  
  #DietPlotNameString<-paste(pathToSavePlots, "BiomassPlot",".pdf",sep="") # create a string to use as the file name
  DietPlotNameString<-paste("DietPlot",".pdf",sep="") # create a string to use as the file name
  
  
  
  
  #FuncGroupsBigGroups <- read.table("FuncGroupsBigGroups.csv", as.is = TRUE, header = TRUE, sep = ",")
  dietsAll<-read.table("AMPS_OUTDietCheck.txt",as.is = TRUE,header=TRUE,sep=" ") # For the Biomass vs. Time plots here. Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
  
  
  ListOfSpeciesAndCohorts<-dietsAll[dietsAll$Time == 365,2:3 ]   # test columns are: Time Group Cohort Stock Updated FDP etc. till column 76 is the last species. 
  numDietsToPlot <-dim(ListOfSpeciesAndCohorts)[1]
  
  
  
  
  pdf(DietPlotNameString)
  #windows()
  
  #works for single plot per page: par(mfrow=(5,2),xpd = T, mar = par()$mar + c(0,0,0,10))
  
  
  #-------------
  
  species <-"FDP"
  
  #par(mfrow=c(5,2),xpd = T, mar = par()$mar + c(0,0,0,10))
  par(mfrow=c(5,2),xpd = T, mar = par()$mar + c(0,0,0,10))
  
  
  for (dietPlotNum in 1:numDietsToPlot) # loop over all diet plots to make, as listed in year 0 rows of the DietCheck.txt file. 
  {
    
    species <- ListOfSpeciesAndCohorts[dietPlotNum ,1 ]
    cohort <- ListOfSpeciesAndCohorts[dietPlotNum ,2  ]
    print(species)
    print('Cohort number: ')
    print(cohort)
    
    PlotDietsOneSpeciesOneCohort(species,cohort)
  }
  
  
  
  
  #---------------
  
  dev.off()
  
} # end function


PlotDietsOneSpeciesOneCohort <- function( species,cohort)
{
  
  
  #plot.new()
  DietPlotNameString<-paste("DietPlotOneSpeciesOneCohort",".pdf",sep="") # create a string to use as the file name
  
  FuncGroupNamesInPlotOrder <- read.table("../FuncGroupNamesInPlotOrder.csv", as.is = TRUE, header = TRUE, sep = ",")   #read.table("FuncGroupsBigGroups.csv", as.is = TRUE, header = TRUE, sep = ",")
  
  
  #------------------
  print('SET UP PAR MARGINS')
  par(mar=c(3.1, 4.1, 2.1, 7.1), xpd=TRUE)
  # A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1
  
  
  #----------------
  print('READ IN AND REFORMAT INPUT DATA')
  dietsAll<-read.table("AMPS_OUTDietCheck.txt",as.is = TRUE,header=TRUE,sep=" ") # For the Biomass vs. Time plots here. Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
  
  
  
  dietsThisSpecies <- dietsAll[ dietsAll$Predator==species,]
  test<-dietsThisSpecies[dietsThisSpecies$Cohort == cohort, ]   # test columns are: Time Group Cohort Stock Updated FDP etc. till column 76 is the last species. 
  
  
  myData <- as.matrix(t(test[ ,c(1,6:76)]))    # Extract only  Time and Species composition columns frm Test; transpose so these are on rows. 
  #-----
  #minTime<-0
  #maxTime<- 735
  #whichDays <- which((myData[1,]>=minTime)&(myData[1,]<=maxTime))
  #myData <- myData[whichDays,]
  #----
  timesInDays <- myData[1,]  # extract times for plotting later. 
  myData<-myData[-1,] # remove first row, which was times. 
  
  #------
  print('SET UP COLORS')
  colorsAllPrey <- rep(rainbow(10),9)
  colorsAllPrey <- colorsAllPrey[1:85]
  
  
  #-----------
  
  #dietDataArePresent <- (dim(myDataSubset)[1]>0)
  #if (dietDataArePresent)
  
  # CHECK TO SEE IF EACH PREY SPECIES TOTALS GREATER THAN 0.01 
  indexPreyAboveMinThresholdTemp <- (rowSums(myData)>0.01)
  print('Printing index of indexPreyAboveMinThresholdTemp')
  print(indexPreyAboveMinThresholdTemp)
  
  indexPreyAboveMinThresholdTempIndex <- which(rowSums(myData)>0.01,arr.ind=TRUE)
  #indexPreyAboveMinThresholdTempIndex<-indexPreyAboveMinThresholdTempIndex$ix
  
  # IF  AT LEAST ONE PREY SPECIES TOTALS GREATER THAN 0.01
  if (length(which(indexPreyAboveMinThresholdTemp))>0)  # should be indexPreyAboveMinThresholdTempIndex 
  {
    
    if(length(indexPreyAboveMinThresholdTempIndex)>1)
    {  # Use the index of rows specifying which prey contribute more than 0.01, but sort them from large to small
      sortOutput<-sort(rowSums(myData[indexPreyAboveMinThresholdTemp,]),decreasing=TRUE,index.return=TRUE)
      sortedOrder<-sortOutput$ix
    }
    else
    {
      sortedOrder<-1
    }
    # apply this sorting to the index of  rows specifying which prey contribute 
    indexPreyAboveMinThreshold<-indexPreyAboveMinThresholdTempIndex[sortedOrder]
    # subset the data using this new sorted index
    myDataSubset <- myData[indexPreyAboveMinThreshold, ] 
    
    colorsSubset <- colorsAllPrey[indexPreyAboveMinThreshold]
    
    print('indexPreyAboveMinThresholdTemp')
    print(indexPreyAboveMinThresholdTemp)  
    print('sortedOrder')
    print(sortedOrder)
    print('indexPreyAboveMinThreshold') 
    print(indexPreyAboveMinThreshold)  
    print(myDataSubset)
    print(timesInDays)
    
    
    print('MAKE BARPLOT')
    barplot( myDataSubset,col=colorsSubset,names.arg= timesInDays/365,cex.lab=1.3,cex.axis=1.3,xlab = "Year",ylab = "Diet fraction",ylim=c(0,1.0),main=paste(species, ": Cohort ",cohort),border=NA,mgp=c(1.7,0.5,0))
    #mgp:  the first two numbers can be thought of as the distance between (1) the axis titles and the axes and (2) the axis labels and the axes.
    
    print('ADD LEGEND')
    #legend("topright",inset=c(-0.25,0), fill=heat.colors(6), legend=rownames(myDataSubset))
    sppNamesSubset <- FuncGroupNamesInPlotOrder$Long.Name[ indexPreyAboveMinThreshold]
    #sppNamesSubset<-sppNamesSubset[2:length( sppNamesSubset )]
    ##legend("topright",inset=c(-0.25,0), fill= colorsSubset, legend=sppNamesSubset) 
    par(xpd = T, mar = par()$mar + c(0,0,0,7))
    #legend.outside(side=4, fill= colorsSubset, legend=sppNamesSubset,cex=0.5) 
    legend((length(timesInDays)*1.2),1.0, fill= colorsSubset, legend=sppNamesSubset,cex=0.5) 
    par(mar=c(3.1, 4.1, 2.1, 7.1))
    
    # (max(timesInDays)/365)+1
    
    #legend(3,1.0, fill= colorsSubset, legend=sppNamesSubset,cex=0.5) 
    #par(mar=c(5, 4, 4, 2) + 0.1)
  }
  else   # If there were no diet data for this cohort or species
  {
    barplot(1,main=paste(species, ": Cohort ",cohort, "NO DATA"))
  }  # end else statment
  
} #end function