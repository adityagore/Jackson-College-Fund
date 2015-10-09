print("Loading all the required functions")

# To find out how many unique tickets were formed
howManyTickets <- function(){
  print("Please either enter player names or just hit enter:")
  names <- scan(file="",what=character(),nmax=9,quiet=TRUE,sep="\n")
  names.len <- length(names)
  is.finalTickets <- "finalTickets" %in% ls(globalenv())
  if(!is.finalTickets) load("totaltickets.RData")
  finalTickets <<- finalTickets
  if(names.len == 0){
    return(length(getUniqueTickets(finalTickets)))
  } else {
    temp <- lapply(finalTickets,namedTickets,name=names)
    temp <- Filter(Negate(is.null),temp)
    return(length(getUniqueTickets(temp)))
  }
}

# To get all the unique tickets
getUniqueTickets <- function(x){
  names <- lapply(x,function(y) y$Name)
  return(unique(names))
}


# To write the tickets in the file.
getTickets <- function(x,filename){
  if(class(x)=="data.frame"){
    playerNames <- paste0(gsub("(\\w+\\s?\\.?\\s?\\w+\\s?\\.?\\s?\\w*\\s?\\w+\\.?)","\"\\1\"",x$Name),collapse=",")
  } else if (class(x)=="character"){
    playerNames <- paste0(gsub("(\\w+\\s?\\.?\\s?\\w+\\s?\\.?\\s?\\w*\\s?\\w+\\.?)","\"\\1\"",x),collapse=",")
  }
  write(playerNames,file=filename,append=TRUE)
}


# Salary check function
aboveSalary <- function(x,salary){
  ifelse(sum(x$Salary) >= salary, return(x), return(NULL))
}

# To find the tickets with given names
namedTickets <- function(ticket,name){
  if(sum(name %in% ticket$Name)==length(name)){
    return(ticket)
  } else {
    return(NULL)
  }
}

# To remove the tickets with given names
removeNamedTickets <- function(ticket,name,all=FALSE){
  if(sum(name %in% ticket$Name)==length(name) & all){
    return(NULL)
  } else if (sum(name %in% ticket$Name) > 0 & !all){
    return(NULL)
  } else {
    return(ticket)
  }
}

# To remove tickets with user provided names
removeTickets <- function(){
  is.finalTickets <- "finalTickets" %in% ls(globalenv())
  if(!is.finalTickets) load("totaltickets.RData")
  finalTickets <<- finalTickets
  names <- scan(file="",what=character(),nmax=9,quiet=TRUE,sep="\n")
  print("Remove tickets with:")
  print("1. All players in them.")
  print("2. At least one player in them.")
  print("3. Skip")
  n <- 0
  while(!(n %in% c(1L, 2L, 3L))){
    n <- readline("Please select a number: ")
  }
  ifelse(n==1,all<-FALSE,ifelse(n==2,all<-TRUE,all <- ""))
  if(all!=""){
    finalTickets <<- lapply(finalTickets,removeNamedTickets,names,all=all)
    finalTickets <<- Filter(Negate(is.null),finalTickets)
  }
}

playerDistribution <- function(){
  is.finalTickets <- "finalTickets" %in% ls(globalenv())
  if(!is.finalTickets) load("totaltickets.RData")
  finalTickets <<- finalTickets
  names <- scan(file="",what=character(),nmax=9,quiet=TRUE,sep="\n")
  if(length(names)>0){
    tempTickets <- lapply(finalTickets,namedTickets,names)
    tempTickets <- Filter(Negate(is.null),tempTickets)
  } else {
    tempTickets <- finalTickets
  }
  names.list <- unlist(getUniqueTickets(tempTickets))
  position.list <- unlist(lapply(tempTickets,function(x) x$Position))
  names.list <- cbind(position.list,names.list)
  totalCounts <- count(names.list)
  totalTickets <- length(getUniqueTickets(tempTickets))
  names(totalCounts) <- c("Position","Players","Tickets")
  totalCounts$Percentage <- round(totalCounts$Tickets*100/totalTickets,digits=2)
  totalCounts <- totalCounts[order(totalCounts$Percentage,decreasing=TRUE),]
  totalCounts$Percentage <- paste0(as.character(totalCounts$Percentage),"%")
  return(arrange(totalCounts,desc(Position),desc(Tickets)))
}

savetickets <- function(){
  if("finalTickets" %in% ls(globalenv())){
    print("Saving tickets for:")
    print("1. DraftKings")
    print("2. Yahoo")
    print("3. Fanduel")
    n <- 0
    while(!(n %in% c(1L, 2L, 3L))){
      n <- readline("Please select a number: ")
    }
    savefile <- ifelse(n==1,"dktickets.csv",ifelse(n==2,"yahootickets.csv","fandueltickets.csv"))
    save(finalTickets,file="totaltickets.RData")
    string1 <- paste0(gsub("(\\w+\\s+\\w+)","\"\\1\"",finalTickets[[1]]$Position),collapse=",")    
    writeTickets(header=string1,file=savefile)
  } else {
    print("Error: No tickets found.")
  }
}

writeTickets <- function(header,file){
  write(header,file)
  print("Writing the tickets to the file")
  playerNames <- lapply(getUniqueTickets(finalTickets),function(x) {
    paste0(gsub("(\\w+\\s?\\.?\\s?\\w+\\s?\\.?\\s?\\w*\\s?\\w+\\.?)","\"\\1\"",x),collapse=",")
  })
  lapply(playerNames,write,file=file,append=TRUE)
  print(paste0(length(finalTickets), " tickets written to the file \"", file,"\""))
}

salaryDistribution <- function(){
  is.finalTickets <- "finalTickets" %in% ls(globalenv())
  if(!is.finalTickets) load("totaltickets.RData")
  finalTickets <<- finalTickets
  tmp <- count(unlist(lapply(finalTickets, function(x) sum(x$Salary))))
  tmp <- tmp[order(tmp$freq,decreasing = TRUE),]
  names(tmp) <- c("Salary","Tickets")
  tmp$Percentage <- paste0(as.character(round(tmp$Tickets*100/length(finalTickets),digits = 2)),"%")
  return(tmp)
}

parseTable <- function(dataTable,contestNumber){
  dataTable[,ContestNumber:=contestNumber]
  setkeyv(dataTable,c("ContestNumber","EntryId","Rank","Lineup"))
  return(dataTable[,':=' (QB = gsub("QB (.*) RB.*RB.*WR.*","\\1",Lineup),
               RB1 = gsub("QB.*RB (.*) RB.*","\\1",Lineup),
               RB2 = gsub("QB.*RB.*RB (.*) WR.*WR.*WR.*","\\1",Lineup),
               WR1 = gsub("QB.*WR (.*) WR.*WR.*","\\1",Lineup),
               WR2 = gsub("QB.*WR.*WR (.*) WR.*","\\1",Lineup),
               WR3 = gsub("QB.*WR.*WR.*WR (.*) TE.*","\\1",Lineup),
               TE = gsub("QB.*TE (.*) FLEX.*","\\1",Lineup),
               FLEX = gsub("QB.*FLEX (.*) DST.*","\\1",Lineup),
               DST = gsub("QB.*DST (\\w+)\\s*$","\\1",Lineup))])
}

readParseTable <- function(filePath){
  dataTable <- fread(filePath,stringsAsFactors=FALSE)
  week <- gsub(".*(Week\\d+).*","\\1",filePath)
  dataTable[,Week:=week]
  contestNumber <- gsub("\\D*\\d*\\D*(\\d*).*","\\1",filePath)
  print(contestNumber)
  return(parseTable(dataTable,contestNumber))
}

factorToCharacter <- function(datatable){
  tmp <- lapply(datatable,function(x){
    if(is.factor(x)) return(as.character(x)) else return(x)
  })
  tmp <- data.table(tmp)
  return(tmp)
}

cleanFactors <- function(dataframe){
  classObject <- class(dataframe)
  tmp <- lapply(dataframe,function(x){
    if(is.factor(x)) return(factor(x)) else return(x)
  })
  ifelse("data.table"%in%classObject,return(data.table(data.frame(tmp))),return(data.frame(tmp)))
}

findStacks <- function(position,teams,stackNumber=0,exact=TRUE,na=FALSE){
  if((sum(is.na(teams))>0|"" %in% teams)&!na) {
    return(FALSE)
  } else if((sum(is.na(teams))>0|"" %in% teams)&na){
    return(TRUE)
  } else if(!na) {
    qbIndex <- position %in% "QB"
    team.qb <- teams[qbIndex]
    if(exact){
      ifelse(sum(teams[!qbIndex]%in%team.qb)==stackNumber,return(TRUE),return(FALSE))
    } else {
      ifelse(sum(teams[!qbIndex]%in%team.qb)>=stackNumber,return(TRUE),return(FALSE))
    }
  } else {
    return(FALSE)
  }
}

NoTeamInformation <- function(teams){
  if(sum(is.na(teams))>0|""%in%teams){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

combineSalaries <- function(){
  salary.datadk <- read.csv("DKSalaries.csv",stringsAsFactors = FALSE)
  salary.datafd <- read.csv("FDSalaries.csv",stringsAsFactors=FALSE)
  salary.datayahoo <- read.csv("YahooSalaries.csv",stringsAsFactors = FALSE)
  salary.datadk$Name <- gsub("(.+)\\s+$","\\1",salary.datadk$Name)
  salary.datayahoo$Name <- gsub("(.*)(IR|O|Q|P|D|DTD)$","\\1",salary.datayahoo$Name)
  salary.datafd$Name <- paste0(salary.datafd$First.Name," ",salary.datafd$Last.Name)
  salary.datafd[salary.datafd$Position=="D","Position"] <- "DST"
  salary.datayahoo[salary.datayahoo$Pos=="DEF","Pos"] <- "DST"
  salary.datadk <- salary.datadk[,c("Position","Name","Salary","teamAbbrev")]
  salary.datafd <- salary.datafd[,c("Position","Name","Salary","Team")]
  salary.datayahoo <- salary.datayahoo[,c("Pos","Name","Salary")]
  names <- unlist(sapply(salary.datadk$Name,function(x) grep(x,salary.datayahoo$Name,value=TRUE)))
  salary.datadk[salary.datadk$Name %in% names(names)&salary.datadk$Position=="DST","Name"] <- names[names(names)%in%salary.datadk[salary.datadk$Position=="DST","Name"]]
  salary.merge1 <- merge(salary.datadk,salary.datayahoo,by.x=c("Position",
                        "Name"),by.y = c("Pos","Name"),suffixes = c("DK","Yahoo"),all=TRUE)
  salary.data <- merge(salary.merge1,salary.datafd,by=c("Position","Name"),all=TRUE)
  salary.data <- salary.data[,c("Position","Name","SalaryDK","Salary","SalaryYahoo","Team")]
  names(salary.data) <- c("Position","Name","SalaryDK","SalaryFD","SalaryYahoo","Team")
  write.table(salary.data,"compareSalary.csv",sep=",",row.names = FALSE)
}

cleanFDSalaries <- function(){
  tmp <- read.csv("FDSalaries.csv",stringsAsFactors = FALSE)
  write.csv(tmp,file="FDSalaries.csv",row.names=FALSE)
  rm(tmp)
}
