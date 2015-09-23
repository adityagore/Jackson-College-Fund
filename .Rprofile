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
  
  if(!require(ggplot2)){
    install.packages("ggplot2")
    require(ggplot2,quietly = TRUE)
  } else {
    require(ggplot2,quietly = TRUE)
  }
  
  if(!require(data.table)){
    install.packages("data.table")
    require(data.table, quietly = TRUE)
  } else {
    require(data.table,quietly = TRUE)
  }
  
  if(!require(reshape2)){
    install.packages("reshape2")
    require(reshape2, quietly = TRUE)
  } else {
    require(reshape2,quietly = TRUE)
  }
  
  source("requiredFunctions.R")
    # source("startup.R")

}

.Last <- function(){
  cat("\nGoodbye at ", date(), "\n")
}