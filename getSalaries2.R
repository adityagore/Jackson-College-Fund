print("For which site are you making the tickets?")
print("1. DraftKings")
print("2. Yahoo")
print("3. FanDuel")
n <- 0
while(!(n %in% c(1L, 2L, 3L))){
  n <- readline("Please select a number: ")
}

ifelse(n==1,draftkings <- 1L,draftkings <- 0L) # If making tickets for draftkings set it to 1 else 0
ifelse(n==2,yahoo <- 1L,yahoo <- 0L) # If making tickets for yahoo set it to 1 else 0
ifelse(n==3,fanduel <- 1L,fanduel <- 0L)  # If making tickets for fanduel set it to 1 else 0

n <- 0
# Flex position for all tickets
if(draftkings|yahoo){
  print("What's your flex position?")
  print("1. WR")
  print("2. RB")
  print("3. TE")
  while(!(n %in% c(1L, 2L, 3L))){
    n <- readline("Please select a number: ")
  }
  flex <- ifelse(n==1,"WR",ifelse(n==2,"RB","TE"))
}
n <- 0
print("Do you want to update team players belong to?")
print("1. No")
print("2. Yes")
while(!(n %in% c(1L, 2L))){
  n <- readline("Please select a number: ")
}

 
updatePlayerTeams <- ifelse(n==1,0L,1L) # Set to 1 if players moved to other team during this week. 

n <- 0
print(Sys.time())


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

#Loading the teams of players from Fox Sports website
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
  playerData <- subset(playerData, Pos %in% c("QB","WR","RB","TE","K"))[,c("Player","Team","Pos")] # Just getting the teams of selected players
  
  playerData$Name <- gsub("(\\D+)\\r\\n\\t+(\\D+)","\\1 \\2",playerData$Player) # Getting appropriate names
  
  playerData$Player <- NULL
  playerData$Position <- playerData$Pos
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
  "Patriots"="NE",
  "Raiders"="OAK",
  "Rams"="STL",
  "Ravens"="BAL",
  "Redskins"="WAS",
  "Saints"="NO",
  "Seahawks"="SEA",
  "Steelers"="PIT",
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
  "Minnesota Vikings"="MIN",
  "Pittsburgh Steelers"="PIT",
  "New England Patriots"="NE"
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
  salary.data$Name <- gsub("(.+)\\s+$","\\1",salary.data$Name) #Removing extra space from the end of the name in dragtkings salary
} else if(yahoo){
  salary.data$Name <- gsub("(.*)(IR|O|Q|P|D|DTD)$","\\1",salary.data$Name) #Removing the player out,questionable indices from end
}

gameInfo <- ifelse(draftkings,"GameInfo",ifelse(yahoo,"Opponent","")) # Getting away and home team information
pattern <- ifelse(draftkings,
                  "(\\w{2,3})@(\\w{2,3})\\s.*", ifelse(yahoo,
                                                       "(\\w{2,3})\\s*@\\s*(\\w{2,3})\\,.*",""))
teams <- strsplit(gsub(pattern,"\\1,\\2",salary.data[[gameInfo]]),split=",")
teams <- lapply(teams,toupper)
salary.data[,c("Home","Away")] <- do.call(rbind,teams)
if(yahoo){
  names(salary.data) <- gsub("Pos","Position",names(salary.data))
  salary.data[salary.data$Position=="DEF","Position"] <- "DST"
}

if(draftkings){
  salary.data$Team <- toupper(salary.data$teamAbbrev)
} else {
  salary.data <- merge(salary.data,playerData,by=c("Name","Position"),all.x=TRUE)
  salary.data <- salary.data[complete.cases(salary.data$Salary),]
  matchedIndex <- match(defenseName$ind,salary.data$Name)
  matchedIndex <- matchedIndex[!(is.na(matchedIndex))]
  matchedValues <- match(salary.data$Name,defenseName$ind)
  matchedValues <- matchedValues[!(is.na(matchedValues))]
  salary.data[matchedIndex,"Team"] <- defenseName$values[matchedValues]
}


# Reading the template file
templatefile <- ifelse(draftkings,"dktemplate.txt",ifelse(yahoo,"yahootemplate.txt",""))

template <- read.csv(templatefile,stringsAsFactors=FALSE,header=TRUE)

playerpoolfile <- ifelse(draftkings,"playerpool.csv",ifelse(yahoo,"yahooplayerpool.csv",""))

playerpool <- read.csv(playerpoolfile,header=TRUE,stringsAsFactors=FALSE)

if(yahoo){
  names(playerpool) <- gsub("Pos","Position",names(playerpool))
  playerpool$Name <- gsub("(.*)(IR|O|Q|P|D)$","\\1",playerpool$Name)
  playerpool[playerpool$Position=="DEF","Position"] <- "DST"
}

wr.salary <- subset(salary.data,Name %in% playerpool$Name & Position=="WR")
rb.salary <- subset(salary.data,Name %in% playerpool$Name & Position=="RB")
te.salary <- subset(salary.data,Name %in% playerpool$Name & Position=="TE")
dst.salary <- subset(salary.data,Name %in% playerpool$Name & Position=="DST")




print("Cleaned all the data")
Sys.time()

newOrder <- c("QB","RB","WR","TE","DST")


maxSalary <- ifelse(draftkings,50000,ifelse(yahoo,200,0))
print("Please enter the minimum value of tickets to be made!")
print("E.g. 49700 or 50000 for Draftkings, 196 or 200 for yahoo")
minSalary <- 51000
while(maxSalary < minSalary){
  minSalary <- readline("Please enter the amount: ")
  minSalary <- as.integer(minSalary)
}



reorderData <- function(x,order){
  return(x[order(factor(x$Position,levels=order)),])
}

finalOrderData <- function(x,template){
  new.x <- x[match(template,x$Name),]
  new.x[!complete.cases(new.x),]   <- subset(x,!(Name %in% template))
  return(new.x)
}


# Form combinations for one template one position
combForm <- function(x,useData,pos,n,flex="WR",totalRemaining){
  salary.taken <- sum(x$Salary)
  salary.left <- maxSalary - salary.taken
  salary.right <- minSalary - salary.taken
  qb.name <- subset(x,Position=="QB")$Name
  ifelse((totalRemaining - n) == 0, exactSalary<-TRUE,exactSalary<-FALSE)
  if(pos=="WR"){
    index <- combnPrim(nrow(useData),n)
    good_rs <- with(useData,apply(index,2,function(y){
      sum(Salary[y]) <= salary.left & ifelse(exactSalary,sum(Salary[y]) >= salary.right,TRUE) &
        sum(Team[y] %in% subset(x,Position=="RB")$Team) == 0 & # No WR and RB from same team
        sum(Team[y] %in% subset(x,Position=="TE")$Team) == 0 & # No WR and TE from same team
        sum(Team[y] %in% subset(x,Position=="DST")$Team) == 0 # No WR and DST from same team
    }))
    if(sum(good_rs)>0){
      if(n>1 & sum(good_rs)>1){
        wrList <- do.call(list,apply(index[,good_rs],2,function(y){
          rbind(x, useData[y,])
        }))
      } else { #n==1|sum(good_rs)==1
        wrList <- do.call(list,sapply(index[,good_rs],function(y){
          rbind(x, useData[y,])
        },simplify=FALSE))
      }
    } else {wrList <- NULL}
    ifelse(is.null(wrList),return(list(NULL)),return(wrList))
  } else if(pos=="RB"){
    index <- combnPrim(nrow(useData),n)
    good_rs <- with(useData,apply(index,2,function(y){
      sum(Salary[y]) <= salary.left & ifelse(exactSalary,sum(Salary[y]) >= salary.right,TRUE) &
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
      sum(Salary[y]) <= salary.left & ifelse(exactSalary,sum(Salary[y]) >= salary.right,TRUE) &
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
      sum(Salary[y]) <= salary.left & ifelse(exactSalary,sum(Salary[y]) >= salary.right,TRUE) &
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
                           Team %in% team.wr|
                           Team %in% team.qb |
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
                           Team %in% team.dst |
                           Team %in% team.te
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
  }
  newOrder <- get("newOrder",envir)
  lastComb <- lapply(lastComb,function(x){
    if(nrow(x)==9){
      return(x)
    } else {
      return(NULL)
    }
  })
  lastComb <- Filter(Negate(is.null),lastComb)
  print("Done with ticket formations")
  print(paste0("Number of tickets formed: ",length(lastComb)))
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

print(system.time(finalTickets <- unlist(apply(template,1,comboFormPerTemplate,qb.num=QB,envir=my_env,
                                               rb.num=RB, wr.num=WR, te.num=TE, dst.num=DST),recursive=FALSE)))


# print(system.time(rightTickets <- lapply(finalTickets,aboveSalary,salary=maxSalary)))
# print(system.time(rightTickets <- Filter(Negate(is.null),rightTickets)))


# print(system.time(rightTickets <- parallel.lapply(rightTickets,reorderData,envir=my_env,newOrder)))
print(paste0("Number of qualified tickets: ",length(finalTickets)))

ticketsfile <- ifelse(draftkings,"dktickets.csv",ifelse(yahoo,"yahootickets.csv","fandueltickets.csv"))

string1 <- paste0(gsub("(\\w+\\s+\\w+)","\"\\1\"",rightTickets[[1]]$Position),collapse=",")
writeTickets(header=string1,file=ticketsfile)

save(finalTickets,file="totaltickets.RData")
