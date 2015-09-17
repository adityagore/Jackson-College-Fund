.First <- function(){
  # Loading gRbase for generating combinations
  print("Loading packages")
  if(!require(gRbase)){
    source("http://bioconductor.org/biocLite.R")
    biocLite("gRbase")
    require(gRbase,quietly=TRUE)
  } else {
    require(gRbase,quietly=TRUE)
  }
  
  
  # Loading XML for scraping websites
  if(!require(XML)){
    install.packages("XML")
    require(XML,quietly=TRUE)
  } else {
    require(XML,quietly=TRUE)
  }
  
  
  # Loading parallel for parallel computations
  if(!require(parallel)){
    install.packages("parallel")
    require(parallel)
  } else {
    require(parallel)
  }
  
  # Loading plyr package for distribution
  if(!require(plyr)){
    install.packages("plyr")
    require(plyr,quietly=TRUE)
  } else {
    require(plyr,quietly=TRUE)
  }
  
  source("requiredFunctions.R")
    # source("startup.R")

}

.Last <- function(){
  cat("\nGoodbye at ", date(), "\n")
}