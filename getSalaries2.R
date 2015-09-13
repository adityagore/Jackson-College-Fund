rm(list=ls())


draftkings <- 0L # If making tickets for draftkings set it to 1 else 0
yahoo <- 1L # If making tickets for yahoo set it to 1 else 0
fanduel <- 0L # If making tickets for fanduel set it to 1 else 0
flex <- "WR" # Flex position for all tickets
updatePlayerTeams <- 0L # Set to 1 if players moved to other team during this week. 

print(Sys.time())
print("Loading Packages")

# Loading gRbase for generating combinations
if(!require(gRbase)){
  source("http://bioconductor.org/biocLite.R")
  biocLite("gRbase")
  require(gRbase)
} else {
  require(gRbase)
}


# Loading XML for scraping websites
if(!require(XML)){
  install.packages("XML")
  require(XML)
} else {
  require(XML)
}


# Loading parallel for parallel computations
if(!require(parallel)){
  install.packages("parallel")
  require(parallel)
} else {
  require(parallel)
}


# Setting the numbers of players by position for each website
if(draftkings|yahoo){
  QB <- 1
  RB <- ifelse(flex=="RB",3,2)
  WR <- ifelse(flex=="WR",4,3)
  TE <- ifelse(flex=="TE",2,1)
  DST <- 1
  K <- 0  
} else {
  QB <- 1
  RB <- 2
  WR <- 3
  TE <- 1
  DST <- 1
  K <- 1 
}

# Setting up the number of cores for parallel computation
n.cores <- detectCores()
ifelse(n.cores==48,n.cores <- 40,ifelse(n.cores==8,n.cores <- 4,ifelse(n.cores==4,n.cores <- 3,n.cores <- detectCores())))
print(Sys.time())

print("Working on gathering playerData")

if(file.exists("playerData.RData") & !updatePlayerTeams){
  load("playerData.RData")
} else {
  print("Gathering player teams data from:")
  string1 <- "http://www.foxsports.com/nfl/players?season=2015&page="
  string2 <- "&position=0"
  print(string1)
  
  readPlayerTeams <- function(x = c(1:180),string1,string2){
    url <- paste0(string1,x,string2)
    return(readHTMLTable(url,which=1,stringsAsFactors=FALSE))
  }
  
  playerData <- data.frame()
  print(system.time(  
    for(i in seq(166)){
      url <- paste0(string1,i,string2)
      playerData <- rbind(playerData,
                          readHTMLTable(url,which=1,stringsAsFactors=FALSE))
    }
  ))
  playerData <- subset(playerData, Pos %in% c("QB","WR","RB","TE","K"))[,c("Player","Team","Pos")]
  
  playerData$Name <- gsub("(\\D+)\\r\\n\\t+(\\D+)","\\1 \\2",playerData$Player)
  
  playerData$Player <- NULL
  playerData$Pos <- NULL
  
  save(playerData,file="playerData.RData")
  
}
ifelse(draftkings,
defenseName <- list(
  "49ers"="SF",
  "Bears"="CHI",
  "Bengals"="CIN",
  "Bills"="BUF",
  "Broncos"= "DEN",
  "Browns"="CLE",
  "Buccaneers"="TB",
  "Cardinals"="ARI",
  "Chargers"="SD",
  "Chiefs"="KC",
  "Colts"="IND",
  "Cowboys"="DAL",
  "Dolphins"="MIA",
  "Eagles"="PHI",
  "Falcons"="ATL",
  "Giants"="NYG",
  "Jaguars"="JAX",
  "Jets"="NYJ",
  "Lions"="DET",
  "Packers"="GB",
  "Panthers"="CAR",
  "Raiders"="OAK",
  "Rams"="STL",
  "Ravens"="BAL",
  "Redskins"="WAS",
  "Saints"="NO",
  "Seahawks"="SEA",
  "Texans"="HOU",
  "Titans"="TEN",
  "Vikings"="MIN"
), ifelse(yahoo,
defenseName <- list(
  "San Francisco 49ers"="SF",
  "Chicago Bears"="CHI",
  "Cincinnati Bengals"="CIN",
  "Buffalo Bills"="BUF",
  "Denver Broncos"= "DEN",
  "Cleveland Browns"="CLE",
  "Tampa Bay Buccaneers"="TB",
  "Arizona Cardinals"="ARI",
  "San Diego Chargers"="SD",
  "Kansas City Chiefs"="KC",
  "Indianapolis Colts"="IND",
  "Dallas Cowboys"="DAL",
  "Miami Dolphins"="MIA",
  "Philadelphia Eagles"="PHI",
  "Atlanta Falcons"="ATL",
  "New York Giants"="NYG",
  "Jacksonville Jaguars"="JAX",
  "New York Jets"="NYJ",
  "Detroit Lions"="DET",
  "Green Bay Packers"="GB",
  "Carolina Panthers"="CAR",
  "Oakland Raiders"="OAK",
  "St. Louis Rams"="STL",
  "Baltimore Ravens"="BAL",
  "Washington Redskins"="WAS",
  "New Orleans Saints"="NO",
  "Seattle Seahawks"="SEA",
  "Houston Texans"="HOU",
  "Tennessee Titans"="TEN",
  "Minnesota Vikings"="MIN"
),defenseName <- list()))

defenseName <- stack(defenseName)
print("Gathered Player Data")
print(Sys.time())

print("Working on cleaning data")
print(Sys.time())

# Reading the salaries from the file
salaryFile <- ifelse(draftkings,"DKSalaries.csv",ifelse(yahoo,"YahooSalaries.csv","fanduelSalaries.csv"))
salary.data <- read.csv(file=salaryFile,header=TRUE,stringsAsFactors=FALSE)
if(draftkings){
  salary.data$Name <- gsub("(.+)\\s+$","\\1",salary.data$Name)
} else if(yahoo){
  salary.data$Name <- gsub("(.*)(IR|O|Q|P|D|DTD)$","\\1",salary.data$Name)
}

gameInfo <- ifelse(draftkings,"GameInfo",ifelse(yahoo,"Opponent",""))
pattern <- ifelse(draftkings,
                  "(\\w{2,3})@(\\w{2,3})\\s.*", ifelse(yahoo,
                                                       "(\\w{2,3})\\s*@\\s*(\\w{2,3})\\,.*",""))
teams <- strsplit(gsub(pattern,"\\1,\\2",salary.data[[gameInfo]]),split=",")
teams <- lapply(teams,toupper)
salary.data[,c("Home","Away")] <- do.call(rbind,teams)
# salary.data$preference <- 0


salary.data <- merge(salary.data,playerData,by="Name",all.x=TRUE)
salary.data <- salary.data[complete.cases(salary.data$Salary),]
salary.data[match(defenseName$ind,salary.data$Name),"Team"] <- defenseName$values

if(yahoo){
  names(salary.data) <- gsub("Pos","Position",names(salary.data))
  salary.data[salary.data$Position=="DEF","Position"] <- "DST"
}

template <- read.csv("template.txt",stringsAsFactors=FALSE,header=TRUE)
# template$K <- NULL
# template$Position <- NULL

# template[template$PC %in% subset(salary.data,Position=="WR")$Name,"Position"] <- "WR"
# template[template$PC %in% subset(salary.data,Position=="TE")$Name, "Position"] <- "TE"
# template[is.na(template$Position),"Position"] = ""

returnData <- function(x,template,position){
  return(subset(x, Name %in% unlist(as.list(template)) & Position==position))
}

# qb.salary <- returnData(salary.data,template,"QB")
# wr.salary <- returnData(salary.data,template,"WR")
# te.salary <- returnData(salary.data,template,"TE")
# rb.salary <- returnData(salary.data,template,"RB")
# dst.salary <- returnData(salary.data,template,"DST")

playerpoolfile <- ifelse(draftkings,"playerpool.csv",ifelse(yahoo,"yahooplayerpool.csv",""))

playerpool <- read.csv(playerpoolfile,header=TRUE,stringsAsFactors=FALSE)

if(yahoo){
  names(playerpool) <- gsub("Pos","Position",names(playerpool))
  playerpool$Name <- gsub("(.*)(IR|O|Q|P|D)$","\\1",playerpool$Name)
  playerpool[playerpool$Position=="DEF","Position"] <- "DST"
}
# wr.salary <- rbind(wr.salary,subset(salary.data,Name %in% playerpool$Name & Position=="WR"))
# rb.salary <- rbind(rb.salary,subset(salary.data,Name %in% playerpool$Name & Position=="RB"))
# te.salary <- rbind(te.salary,subset(salary.data, Name %in% playerpool$Name & Position=="TE"))

wr.salary <- subset(salary.data,Name %in% playerpool$Name & Position=="WR")
rb.salary <- subset(salary.data,Name %in% playerpool$Name & Position=="RB")
te.salary <- subset(salary.data,Name %in% playerpool$Name & Position=="TE")
dst.salary <- subset(salary.data,Name %in% playerpool$Name & Position=="DST")




print("Cleaned all the data")
Sys.time()

newOrder <- c("QB","RB","WR","TE","DST")
maxSalary <- ifelse(draftkings,50000,ifelse(yahoo,200,0))

reorderData <- function(x,order){
  return(x[order(factor(x$Position,levels=order)),])
}

finalOrderData <- function(x,template){
  new.x <- x[match(template,x$Name),]
  new.x[!complete.cases(new.x),]   <- subset(x,!(Name %in% template))
  return(new.x)
}

combForm <- function(x,useData,pos,n,flex="WR",totalRemaining){
  salary.taken <- sum(x$Salary)
  salary.left <- maxSalary - salary.taken
  qb.name <- subset(x,Position=="QB")$Name
  ifelse((totalRemaining - n) == 0, exactSalary<-TRUE,exactSalary<-FALSE)
  if(pos=="WR"){
    index <- combnPrim(nrow(useData),n)
    good_rs <- with(useData,apply(index,2,function(y){
      ifelse(exactSalary,sum(Salary[y]) == salary.left,sum(Salary[y]) <= salary.left) &
        sum(Team[y] %in% subset(x,Position=="RB")$Team) == 0 & # No WR and RB from same team
        sum(Team[y] %in% subset(x,Position=="TE")$Team) == 0 & # No WR and TE from same team
        sum(Team[y] %in% subset(x,Position=="DST")$Team) == 0 # No WR and DST from same team
    }))
    if(sum(good_rs)>0){
      if(n>1 & sum(good_rs)>1){
        wrList <- do.call(list,apply(index[,good_rs],2,function(y){
          rbind(x, useData[y,])
        }))} else {
          wrList <- do.call(list,sapply(index[,good_rs],function(y){
            rbind(x, useData[y,])
          },simplify=FALSE))
        }} else {wrList <- NULL}
    ifelse(is.null(wrList),return(list(NULL)),return(wrList))
  } else if(pos=="RB"){
    index <- combnPrim(nrow(useData),n)
    good_rs <- with(useData,apply(index,2,function(y){
      ifelse(exactSalary,sum(Salary[y]) == salary.left,sum(Salary[y]) <= salary.left) &
        sum(Team[y] %in% subset(x,Position=="QB")$Team) == 0 & # No QB and RB from same team
        sum(Team[y] %in% subset(x,Position=="WR")$Team) == 0 & # No RB and WR from same team
        sum(Team[y] %in% subset(x,Position=="TE")$Team) == 0 & # No RB and TE from same team
        sum(Team[y] %in% subset(x,Position=="DST")$Home) == 0 & # No RB and Home Defense
        sum(Team[y] %in% subset(x,Position=="DST")$Away) == 0 & # No RB and Away Defense
        sum(Team[y] %in% subset(x,Position=="K")$Home) == 0 & # No RB and K from home team
        sum(Team[y] %in% subset(x,Position=="K")$Away) == 0 # No RB and TE from away team
    }))
    if(sum(good_rs)>0){
      if(n>1 & sum(good_rs)>1){
        rbList <- do.call(list,apply(index[,good_rs],2,function(y){
          rbind(x,useData[y,])
        }))} else {
          rbList <- do.call(list,sapply(index[,good_rs],function(y){
            rbind(x,useData[y,])
          },simplify=FALSE))
        }} else {rbList <- NULL}
    ifelse(is.null(rbList),return(list(NULL)),return(rbList))
  } else if(pos=="TE"){
    index <- combnPrim(nrow(useData),n)
    good_rs <- with(useData,apply(index,2,function(y){
      ifelse(exactSalary,sum(Salary[y]) == salary.left,sum(Salary[y]) <= salary.left) &
        sum(Team[y] %in% subset(x,Position=="RB")$Team) == 0 & # No RB and TE of same team
        sum(Team[y] %in% subset(x,Position=="WR")$Team) == 0 & # No RB and TE of same team
        sum(Team[y] %in% subset(x,Position=="DST")$Team) == 0 # No RB and TE of same team
    }))
    if(sum(good_rs)>0){
      if(n>1 & sum(good_rs)>1){
        teList <- do.call(list,apply(index[,good_rs],2,function(y){
          rbind(x,useData[y,])
        }))} else {
          teList <- do.call(list,sapply(index[,good_rs],function(y){
            rbind(x,useData[y,])
          },simplify=FALSE))
        }} else {teList <- NULL}
    ifelse(is.null(teList),return(list(NULL)),return(teList))  
  } else if(pos=="DST"){
    index <- combnPrim(nrow(useData),n)
    good_rs <- with(useData,apply(index,2,function(y){
      ifelse(exactSalary,sum(Salary[y]) == salary.left,sum(Salary[y]) <= salary.left) &
        sum(Team[y] %in% subset(x,Position=="QB")$Team) == 0 & #No QB and DST of same team
        sum(Team[y] %in% subset(x,Position=="TE")$Team) == 0 & #No TE and DST of same team
        sum(Team[y] %in% subset(x,Position=="WR")$Team) == 0 & #No WR and DST of same team
        sum(Team[y] %in% subset(x,Position=="RB")$Home) == 0 & #No RB and DST of home team
        sum(Team[y] %in% subset(x,Position=="RB")$Away) == 0 #No RB and DST of away team
    }))
    if(sum(good_rs)>0){
      dstList <- do.call(list,sapply(index[,good_rs],function(y){
        rbind(x,useData[y,])
      },simplify=FALSE))} else{ dstList <- NULL}
    ifelse(is.null(dstList),return(list(NULL)),return(dstList))
  } else if(pos=="FLEX"){
    return(combForm(x,pos=flex,n=n))
  }
}



comboFormPerTemplate <- function(template,qb.num=1,wr.num=4,rb.num=2,te.num=1,dst.num=1,k.num=0,envir=ls()){
  template.data <- subset(salary.data,Name %in% template)
  qb.num <- qb.num - sum(template.data$Position=="QB")
  wr.num <- wr.num - sum(template.data$Position=="WR")
  rb.num <- rb.num - sum(template.data$Position=="RB")
  te.num <- te.num - sum(template.data$Position=="TE")
  dst.num <- dst.num - sum(template.data$Position=="DST")
  team.qb <- subset(template.data,Position=="QB")$Team
  team.wr <- subset(template.data,Position=="WR")$Team
  team.rb <- subset(template.data,Position=="RB")$Team
  team.te <- subset(template.data,Position=="TE")$Team
  team.dst <- subset(template.data,Position=="DST")$Team
  team.k <- subset(template.data,Position=="K")$Team
  dst.home <- subset(template.data,Position=="DST")$Home
  dst.away <- subset(template.data,Position=="DST")$Away
  k.home <- subset(template.data,Position=="K")$Home
  k.away <- subset(template.data,Position=="K")$Away
  rb.home <- subset(template.data,Position=="RB")$Home
  rb.away <- subset(template.data,Position=="RB")$Away
  print(paste0("Working on template with QB: ",template["QB"]))
  total.left <- qb.num + wr.num + rb.num + te.num + dst.num
  print(paste0("Numbers of players to add: ",total.left ))
  if(qb.num == 1){
    firstComb <- parallel.lapply(list(template.data),combForm,useData = qb.salary,pos="QB",n=qb.num,envir=envir)
  } else {
    firstComb <- list(template.data)
    firstComb <- Filter(Negate(is.null),firstComb)
  }
  print("Working on WRs")
  if(wr.num > 0){
    passData <- subset(wr.salary,
                    !(Team %in% team.rb |
                      Name %in% template.data$Name |
                      Team %in% team.te |
                      Team %in% team.dst)
                    )
    print(system.time(secondComb <- unlist(parallel.lapply(firstComb,FUN=combForm,useData = passData,pos="WR",n=wr.num,totalRemaining=total.left,envir=envir),
                         recursive=FALSE)))
    secondComb <- Filter(Negate(is.null),secondComb)
    print(total.left <- total.left - wr.num)
    print("Done with WR combinations")
    print(paste0("Number of tickets formed: ",length(secondComb)))
  } else {
    print(system.time(secondComb <- firstComb))
    print("Done with WR combinations")
    print(paste0("Number of tickets formed: ",length(secondComb)))
  }
  print("Working on RBs")
  if(rb.num > 0){
    passData <- subset(rb.salary,
                       !(Team %in% team.qb |
                           Name %in% template.data$Name |
                           Team %in% team.te |
                           Team %in% dst.home |
                           Team %in% dst.away |
                           Team %in% k.home |
                           Team %in% k.away |
                           Team %in% rb.home |
                           Team %in% rb.away
                           )
    )
    print(system.time(thirdComb <- unlist(parallel.lapply(secondComb,combForm,useData=passData,
                        pos="RB",n=rb.num,totalRemaining=total.left,envir=envir),recursive=FALSE)))
    thirdComb <- Filter(Negate(is.null),thirdComb)
    print(total.left <- total.left - rb.num)
    print("Done with RB combinations")
    print(paste0("Number of tickets formed: ",length(thirdComb)))
  } else {
    print(system.time(thirdComb <- secondComb))
    print("Done with RB combinations")
    print(paste0("Number of tickets formed: ",length(thirdComb)))
  }
  print("Working on TEs")
  if(te.num > 0){
    passData <- subset(te.salary,
                       !(Team %in% team.rb |
                           Name %in% template.data$Name |
                           Team %in% team.wr |
                           Team %in% team.dst
                       )
    )
    print(system.time(fourthComb <- unlist(parallel.lapply(thirdComb,combForm,useData=passData,
                         pos="TE",n=te.num,totalRemaining=total.left,
                         envir=envir),recursive=FALSE)))
    fourthComb <- Filter(Negate(is.null),fourthComb)
    print(total.left <- total.left - te.num)
    print("Done with TE combinations")
    print(paste0("Number of tickets formed: ",length(fourthComb)))
  } else {
    print(system.time(fourthComb <- thirdComb))
    print("Done with TE combinations")
    print(paste0("Number of tickets formed: ",length(fourthComb)))
  }
  print("Working on DST")
  if(dst.num > 0){
    passData <- subset(dst.salary,
                       !(Team %in% team.qb |
                           Name %in% template.data$Name |
                           Team %in% rb.home |
                           Team %in% rb.away |
                           Team %in% team.wr |
                           Team %in% team.te
                       )
    )
    print(system.time(lastComb <- unlist(parallel.lapply(fourthComb,combForm,useData=passData,
                       pos="DST",n=dst.num,totalRemaining=total.left,
                       envir=envir),recursive=FALSE)))
    lastComb <- Filter(Negate(is.null),lastComb)
    print(total.left <- total.left - dst.num)
    print("Done with ticket formations")
    print(paste0("Number of tickets formed: ",length(lastComb)))
  } else {
    print(system.time(lastComb <- fourthComb))
    print("Done with ticket formations")
    print(paste0("Number of tickets formed: ",length(lastComb)))
  }
  newOrder <- get("newOrder",envir)
  lastComb <- lapply(lastComb,reorderData,newOrder)
  print("Ordering tables")
  n.cores <- get("n.cores",envir)
  print(paste0("Number of cores used: ",n.cores))
  cluster <- makePSOCKcluster(names=n.cores)
  print(system.time(lastComb <- parLapply(cl=cluster,X=lastComb,fun=finalOrderData,template=as.character(template))))
  stopCluster(cl=cluster)
  return(lastComb)
}

parallel.apply <- function(x,index,fun,envir=ls(),...){
  if(.Platform$OS.type == "windows"){
    n.cores <- get("n.cores",envir)
    cluster <- makePSOCKcluster(names=n.cores)
    clusterEvalQ(cl=cluster, expr=require(gRbase))
    clusterEvalQ(cl=cluster,expr=require(XML))
    clusterExport(cl=cluster,get("var",envir))
    print(system.time(temp <- parApply(cl=cluster,X=x,MARGIN=index,FUN=fun,...)))
    stopCluster(cl=cluster)
    return(temp)
  } else {
    n.cores <- get("n.cores",envir)
    if(index==1){
      print(system.time(temp <- mclapply(1:nrow(x),function(i,fun,...) fun(x[i,],...,mc.cores=n.cores))))
    } else {
      print(system.time(temp <- mclapply(1:ncol(x),function(i,fun,...) fun(x[,i],...,mc.cores=n.cores))))
    }
    return(temp)
  }
}

parallel.lapply <- function(x,FUN,envir=ls(),...){
  if(.Platform$OS.type == "windows"){
    n.cores <- get("n.cores",envir)
    print(paste0("Number of cores used: ",n.cores))
    cluster <- makePSOCKcluster(names=n.cores)
    clusterEvalQ(cl=cluster, expr=require(gRbase))
    clusterExport(cl=cluster,get("var",envir))
    print(system.time(temp <- parLapply(cl=cluster,X=x,fun=FUN,...)))
    stopCluster(cl=cluster)
    return(temp)
  } else {
    n.cores <- get("n.cores",envir)
    print(paste0("Number of cores used: ",n.cores))
    print(system.time(temp <- mclapply(x,FUN=FUN,...,mc.cores=n.cores)))
    return(temp)
  }
}

my_env <- new.env()
my_env$var <- ls()
my_env$n.cores <- n.cores

print(system.time(finalTickets <- unlist(apply(template,1,comboFormPerTemplate,envir=my_env,qb.num=QB,
                                               rb.num=RB, wr.num=WR, te.num=TE, dst.num=DST),recursive=FALSE)))

aboveSalary <- function(x,salary){
  ifelse(sum(x$Salary) >= salary, return(x), return(NULL))
}

namedTickets <- function(ticket,name){
  if(sum(name %in% ticket$Name)==length(name)){
    return(ticket)
  } else {
    return(NULL)
  }
}

removeNamedTickets <- function(ticket,name,all=FALSE){
  if(sum(name %in% ticket$Name)==length(name) & all){
    return(NULL)
  } else if (sum(name %in% ticket$Name) > 0 & !all){
    return(NULL)
  } else {
    return(ticket)
  }
}


removeTickets <- function(){
  if("finalTickets" %in% ls(globalenv())){
    finalTickets <- get("finalTickets",globalenv())
    names <- scan(file="",what=character(),nmax=9,quiet=TRUE,sep="\n")
    finalTickets <<- lapply(finalTickets,removeNamedTickets,names,all=FALSE)
    finalTickets <<- Filter(Negate(is.null),finalTickets)
  }
} 

print(system.time(rightTickets <- lapply(finalTickets,aboveSalary,salary=maxSalary)))
print(system.time(rightTickets <- Filter(Negate(is.null),rightTickets)))


# print(system.time(rightTickets <- parallel.lapply(rightTickets,reorderData,envir=my_env,newOrder)))
print(paste0("Number of qualified tickets: ",length(rightTickets)))

string1 <- paste0(gsub("(\\w+\\s+\\w+)","\"\\1\"",rightTickets[[1]]$Position),collapse=",")

write(string1,file="tickets.csv")

getTickets <- function(x,filename){
  if(class(x)=="data.frame"){
    playerNames <- paste0(gsub("(\\w+\\s?\\.?\\s?\\w+\\s?\\.?\\s?\\w*\\s?\\w+\\.?)","\"\\1\"",x$Name),collapse=",")
  } else if (class(x)=="character"){
    playerNames <- paste0(gsub("(\\w+\\s?\\.?\\s?\\w+\\s?\\.?\\s?\\w*\\s?\\w+\\.?)","\"\\1\"",x),collapse=",")
  }
  write(playerNames,file=filename,append=TRUE)
}

getUniqueTickets <- function(x){
  names <- lapply(x,function(y) y$Name)
  return(unique(names))
}

print("Writing the tickets to the file")

lapply(getUniqueTickets(rightTickets),getTickets,filename="tickets.csv")

save(finalTickets,rightTickets,file="totaltickets.RData")

findTickets <- function(x){
  load("totaltickets.RData")
  index <- unlist(lapply(finalTickets,function(y){
    sum(x %in% y$Name) == length(x)
  }))
  return(finalTickets[index])
}

howManyTickets <- function(){
  names <- scan(file="",what=character(),nmax=9,quiet=TRUE,sep="\n")
  names.len <- length(names)
  if("finalTickets" %in% ls(globalenv()) & names.len == 0){
    return(length(getUniqueTickets(get("finalTickets",globalenv()))))
  } else {
    finalTickets <- get("finalTickets",globalenv())
    temp <- lapply(finalTickets,namedTickets,name=names)
    temp <- Filter(Negate(is.null),temp)
    return(length(getUniqueTickets(temp)))
  }
}
