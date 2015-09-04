# if(!require(httr)){
#   install.packages("httr")
#   require(httr)
# } else {
#   require(httr)
# }
# 
# if(!require(RJSONIO)){
#   install.packages("RJSONIO")
#   require(RJSONIO)
# } else {
#   require(RJSONIO)
# }
# 
# if(!require(RSelenium)){
#   install.packages("RSelenium")
#   require(RSelenium)
# } else {
#   require(RSelenium)
# }

start.time <- Sys.time()

setwd(path.expand("~/nflProject/"))
print(getwd())

print(Sys.time())
print("Loading Packages")

if(!require(gRbase)){
  source("http://bioconductor.org/biocLite.R")
  biocLite("gRbase")
  require(gRbase)
} else {
  require(gRbase)
}

if(!require(XML)){
  install.packages("XML")
  require(XML)
} else {
  require(XML)
}
print(Sys.time())
print("Working on gathering playerData")

if(file.exists("playerData.RData")){
  load("playerData.RData")
} else {

string1 <- "http://www.foxsports.com/nfl/players?season=2015&page="
string2 <- "&position=0"

playerData <- data.frame()

for(i in seq(166)){
  url <- paste0(string1,i,string2)
  playerData <- rbind(playerData,
                      readHTMLTable(url,which=1,stringsAsFactors=FALSE))
}

playerData <- subset(playerData, Pos %in% c("QB","WR","RB","TE"))[,c("Player","Team","Pos")]

playerData$Name <- gsub("(\\D+)\\r\\n\\t+(\\D+)","\\1 \\2",playerData$Player)

playerData$Player <- NULL
playerData$Pos <- NULL

save(playerData,file="playerData.RData")

}
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
  )

defenseName <- stack(defenseName)
print("Gathered Player Data")
print(Sys.time())

print("Working on cleaning data")
print(Sys.time())
salary.data <- read.csv("DKSalaries.csv",header=TRUE,stringsAsFactors=FALSE)
# write.csv(salary.data[with(salary.data,order(Position,Salary,Name)),c("Name","Position","Salary")],file="playerpool.csv",row.names=FALSE)
template <- read.csv("template.csv",stringsAsFactors=FALSE,header=TRUE)
template$K <- NULL
# load("playerpool.RData")
# salary.data <- subset(salary.data,Name %in% playerpool$Name)
teams <- strsplit(gsub("(\\w{2,3})@(\\w{2,3})\\s.*","\\1,\\2",salary.data$GameInfo),split=",")
teams <- lapply(teams,toupper)
salary.data[,c("Home","Away")] <- do.call(rbind,teams)
# salary.data$preference <- 0
salary.data$Name <- gsub("(.+)\\s+$","\\1",salary.data$Name)
salary.data <- merge(salary.data,playerData,by="Name",all.x=TRUE)
salary.data <- salary.data[complete.cases(salary.data$Salary),]
salary.data[salary.data$Name %in% defenseName$ind,"Team"] <- defenseName$values

template$Position <- NA

template[template$PC %in% subset(salary.data,Position=="WR")$Name,"Position"] <- "WR"
template[template$PC %in% subset(salary.data,Position=="TE")$Name, "Position"] <- "TE"
template[is.na(template$Position),"Position"] = ""

qb.salary <- subset(salary.data,Name %in% template$QB)
wr.salary <- subset(salary.data, Name %in% template$PC & Position=="WR")
te.salary <- subset(salary.data, Name %in% template$PC & Position == "TE")
rb.salary <- subset(salary.data, Name %in% template$RB)
dst.salary <- subset(salary.data, Name %in% template$DEF)



playerpool <- read.csv("playerpool.csv",header=TRUE,stringsAsFactors=FALSE)
wr.salary <- rbind(wr.salary,subset(salary.data,Name %in% playerpool$Name & Position=="WR"))
rb.salary <- rbind(rb.salary,subset(salary.data,Name %in% playerpool$Name & Position=="RB"))
te.salary <- rbind(te.salary,subset(salary.data, Name %in% playerpool$Name & Position=="TE"))


# qb.salary <- subset(salary.data,Position=="QB" & Salary > 3000)
# wr.salary <- subset(salary.data,Position=="WR" & Salary > 3000)
# te.salary <- subset(salary.data,Position=="TE" & Salary > 3000)
# dst.salary <- subset(salary.data,Position=="DST")
# rb.salary <- subset(salary.data,Position=="RB" & Salary > 3000)
set.seed(1920)

print("Cleaned all the data")
Sys.time()

# qb.salary$preference <- rep(c(90,9.99999,0.00001),c(5,15,nrow(qb.salary)-20))
# wr.salary$preference <- rep(c(60,20,9.99999,0.00001),c(15,20,10,nrow(wr.salary)-45))
# rb.salary$preference <- rep(c(60,20,9.99999,0.00001),c(15,30,20,nrow(rb.salary)-65))
# te.salary$preference <- rep(c(90,9.99999,0.00001),c(4,6,nrow(te.salary)-10))
# dst.salary$preference <- rep(c(90,9.99999,0.00001),c(4,6,nrow(dst.salary)-10))
# 
# qb.salary$preference <- qb.salary$preference/sum(qb.salary$preference)
# wr.salary$preference <- wr.salary$preference/sum(wr.salary$preference)
# rb.salary$preference <- rb.salary$preference/sum(rb.salary$preference)
# te.salary$preference <- te.salary$preference/sum(te.salary$preference)
# dst.salary$preference <- dst.salary$preference/sum(dst.salary$preference)

# index <- combnPrim(39,9)
# 
# pickIndex <- function(x,n){
#   return(sample(1:nrow(x),n,prob=x$preference))
# }
# 
# qb.pick <- pickIndex(qb.salary,4)
# rb.pick <- pickIndex(rb.salary,12)
# wr.pick <- pickIndex(wr.salary,15)
# te.pick <- pickIndex(te.salary,4)
# dst.pick <- pickIndex(dst.salary,4)
# 
# player.pool <- rbind(qb.salary[qb.pick,],
#                      rb.salary[rb.pick,],
#                      wr.salary[wr.pick,],
#                      te.salary[te.pick,],
#                      dst.salary[dst.pick,])

# good_rs <- with(player.pool,apply(index,2,function(x){
#   sum(Salary[x]) <= 50000 &
#     sum(Salary[x]) >= 49000 &
#     sum(Position[x]=="QB") ==1 &
#     sum(Position[x] == "DST") ==1 &
#     sum(Position[x]=="RB") >= 2 &
#     sum(Position[x]=="WR") >= 3 &
#     sum(Position[x]=="TE") >= 1 &
#     sum(Position[x] != "QB"&Position[x] != "DST") <= 7
# }))
# 
# save(good_rs,file="playerpool.RData")

#Function to form multiple combinations.

combForm <- function(x,pos,n,flex="WR"){
  salary.taken <- sum(x$Salary)
  salary.left <- 50000 - salary.taken
  qb.name <- subset(x,Position=="QB")$Name
  if(pos=="WR"){
    index <- combnPrim(nrow(wr.salary),n)
    good_rs <- with(wr.salary,apply(index,2,function(y){
      sum(Salary[y]) <= salary.left &
        (sum(Name[y] %in% subset(template,Position=="WR"& QB==qb.name)$PC) > 0 |
           sum(wr.salary$Name %in% subset(template,Position=="WR"& QB==qb.name)$PC) == 0) &
        sum(Team[y] %in% subset(x,Position=="RB")$Team) == 0 & # No WR and RB from same team
        sum(Team[y] %in% subset(x,Position=="TE")$Team) == 0 & # No WR and TE from same team
        sum(Team[y] %in% subset(x,Position=="DST")$Team) == 0 # No WR and DST from same team
    }))
    if(sum(good_rs)>0){
      if(n>1){
        wrList <- do.call(list,apply(index[,good_rs],2,function(y){
          rbind(x, wr.salary[y,])
        }))} else {
          wrList <- do.call(list,sapply(index[,good_rs],function(y){
            rbind(x, wr.salary[y,])
          },simplify=FALSE))
        }} else {wrList <- NULL}
    print(paste0(Sys.time(),":Working on forming WR combinations for ",qb.name))
    ifelse(is.null(wrList),return(list(NULL)),return(wrList))
  } else if(pos=="RB"){
    index <- combnPrim(nrow(rb.salary),n)
    good_rs <- with(rb.salary,apply(index,2,function(y){
      sum(Salary[y]) <= salary.left &
        sum(Team[y] %in% subset(x,Position=="QB")$Team) == 0 & # No QB and RB from same team
        sum(Team[y] %in% subset(x,Position=="WR")$Team) == 0 & # No RB and WR from same team
        sum(Team[y] %in% subset(x,Position=="TE")$Team) == 0 & # No RB and TE from same team
        sum(Team[y] %in% subset(x,Position=="DST")$Home) == 0 & # No RB and Home Defense
        sum(Team[y] %in% subset(x,Position=="DST")$Away) == 0 & # No RB and Away Defense
        sum(Team[y] %in% subset(x,Position=="K")$Home) == 0 & # No RB and K from home team
        sum(Team[y] %in% subset(x,Position=="K")$Away) == 0 & # No RB and TE from away team
        (sum(Name[y] %in% subset(template,QB==qb.name)$RB) > 0 |
           sum(rb.salary$Name %in% subset(template,QB==qb.name)$RB) == 0)
    }))
    if(sum(good_rs)>0){
      if(n>1){
        rbList <- do.call(list,apply(index[,good_rs],2,function(y){
          rbind(x,rb.salary[y,])
        }))} else {
          rbList <- do.call(list,sapply(index[,good_rs],function(y){
            rbind(x,rb.salary[y,])
          },simplify=FALSE))
        }} else {rbList <- NULL}
    print(paste0(Sys.time(),":Working on forming RB combinations for ",qb.name))
    ifelse(is.null(rbList),return(list(NULL)),return(rbList))
  } else if(pos=="TE"){
    index <- combnPrim(nrow(te.salary),n)
    good_rs <- with(te.salary,apply(index,2,function(y){
      sum(Salary[y]) <= salary.left &
        sum(Team[y] %in% subset(x,Position=="RB")$Team) == 0 & # No RB and TE of same team
        sum(Team[y] %in% subset(x,Position=="WR")$Team) == 0 & # No RB and TE of same team
        sum(Team[y] %in% subset(x,Position=="DST")$Team) == 0 & # No RB and TE of same team
        (sum(Name[y] %in% subset(template,Position=="TE"& QB==qb.name)$PC) > 0 |
           sum(te.salary$Name %in% subset(template,Position=="TE"& QB==qb.name)$PC) == 0)
    }))
    if(sum(good_rs)>0){
      if(n>1){
        teList <- do.call(list,apply(index[,good_rs],2,function(y){
          rbind(x,te.salary[y,])
        }))} else {
          teList <- do.call(list,sapply(index[,good_rs],function(y){
            rbind(x,te.salary[y,])
          },simplify=FALSE))
        }} else {teList <- NULL}
    print(paste0(Sys.time(),":Working on forming TE combinations for ",qb.name))
    ifelse(is.null(teList),return(list(NULL)),return(teList))  
  } else if(pos=="DST"){
    index <- combnPrim(nrow(dst.salary),n)
    good_rs <- with(dst.salary,apply(index,2,function(y){
      sum(Salary[y]) <= salary.left &
        sum(Team[y] %in% subset(x,Position=="QB")$Team) == 0 & #No QB and DST of same team
        sum(Team[y] %in% subset(x,Position=="TE")$Team) == 0 & #No TE and DST of same team
        sum(Team[y] %in% subset(x,Position=="WR")$Team) == 0 & #No WR and DST of same team
        sum(Team[y] %in% subset(x,Position=="RB")$Home) == 0 & #No RB and DST of home team
        sum(Team[y] %in% subset(x,Position=="RB")$Away) == 0 & #No RB and DST of away team
        (sum(Name[y] %in% subset(template,QB==qb.name)$DEF) > 0 |
           sum(dst.salary$Name %in% subset(template,QB==qb.name)$DEF)==0)
    }))
    if(sum(good_rs)>0){
      dstList <- do.call(list,sapply(index[,good_rs],function(y){
        rbind(x,dst.salary[y,])
      },simplify=FALSE))} else{ dstList <- NULL}
    print(paste0(Sys.time(),":Working on forming DST combinations for ",qb.name))
    ifelse(is.null(dstList),return(list(NULL)),return(dstList))
  } else if(pos=="FLEX"){
    return(combForm(x,pos=flex,n=n))
  }
}

# Function to join multiple lists together

# appendMultipleList <- function(list1,list2,...){
#   args <- list(...)
#   if(length(args)==0){
#     finalList <- append(list1,list2)
#     print("Players Added")
#     return(finalList)
#   } else {
#     list1 <- append(list1,list2)
#     finalList <- appendMultipleList(list1,...)
#     print("Adding Players")
#     return(finalList)
#   }
# }

print("Working on forming quarterback combinations")
print(Sys.time())
qbPlayerList <- list()
for(i in 1:nrow(qb.salary)){
  qbPlayerList <- append(qbPlayerList,list(qb.salary[i,]))
}
print("Working on forming wide receivers combinations")
print(Sys.time())


secondLevel <- unlist(lapply(qbPlayerList,combForm,pos="WR",n=3),recursive=FALSE)
print(length(secondLevel))
thirdLevel <- unlist(lapply(secondLevel,combForm,pos="FLEX",n=1),recursive=FALSE)
print(length(thirdLevel))
fourthLevel <- unlist(lapply(thirdLevel,combForm,pos="RB",n=2),recursive=FALSE)
print(length(fourthLevel))
fifthLevel <- unlist(lapply(fourthLevel,combForm,pos="TE",n=1),recursive=FALSE)
print(length(fifthLevel))
finalPlayerList <- unlist(lapply(fifthLevel,combForm,pos="DST",n=1),recursive=FALSE)
print(length(finalPlayerList))

save(finalPlayerList,file="finalTickets.RData")

stop.time <- Sys.time()
print("Time taken:")
print(stop.time-start.time)


# playerList <- list()
# 
# for(i in seq(1:length(qbList))){
#   salary.taken <- qbList[[i]]$Salary
#   salary.left <- 50000-salary.taken
#   index <- combnPrim(nrow(wr.salary),3)
#   good_rs <- with(wr.salary,apply(index,2,function(x){
#     sum(Salary[x]) <= salary.left &
#       (sum(Name[x] %in% subset(template,Position=="WR"& QB==qbList[[i]]$Name)$PC) > 0 |
#          (sum(Name[x] %in% subset(template,Position=="WR"& QB==qbList[[i]]$Name)$PC) == 0 &
#             sum(wr.salary$Name %in% subset(template,Position=="WR"& QB==qbList[[i]]$Name)$PC) == 0))
#   }))
#   wrList <- do.call(list,apply(index[,good_rs],2,function(x){
#     rbind(qbList[[i]], wr.salary[x,])
#   }))
#   print(Sys.time())
#   print("WR")
#   playerList <- append(playerList,wrList) 
# }
# 
# print("Working on forming running back combinations")
# print(Sys.time())
# newPlayerList <- list()
# for(i in seq(1:length(playerList))){
#   salary.taken <- sum(playerList[[i]]$Salary)
#   salary.left <- 50000-salary.taken
#   index <- combnPrim(nrow(rb.salary),2)
#   good_rs <- with(rb.salary,apply(index,2,function(x){
#     sum(Salary[x]) <= salary.left &
#       sum(Team[x] %in% subset(playerList[[i]],Position=="QB")$Team) < 1 & # No QB and RB from same team
#       sum(Team[x] %in% subset(playerList[[i]],Position=="WR")$Team) < 1 &# No RB and WR from same team
#       (sum(Name[x] %in% subset(template,QB==playerList[[i]]$Name[1])$RB) > 0 |
#          (sum(Name[x] %in% subset(template,QB==playerList[[i]]$Name[1])$RB) == 0 &
#             sum(rb.salary$Name %in% subset(template,QB==playerList[[i]]$Name[1])$RB) == 0))
#   }))
#   rbList <- do.call(list,apply(index[,good_rs],2,function(x){
#     rbind(playerList[[i]],rb.salary[x,])
#   }))
#   print(Sys.time())
#   print("RB")
#   newPlayerList <- append(newPlayerList,rbList)
# }
# 
# newPlayerList <- lapply(newPlayerList,function(x){
#   if(nrow(x)==6){
#     return(x)
#   }
# })
# 
# newPlayerList <- Filter(Negate(is.null),newPlayerList)
# print("Working on forming tight end combinations")
# print(Sys.time())
# 
# tePlayerList <- list()
# for(i in seq(1:length(newPlayerList))){
#   salary.taken <- sum(newPlayerList[[i]]$Salary)
#   salary.left <- 5000-salary.taken
#   index <- combnPrim(nrow(te.salary),1)
#   good_rs <- with(te.salary,apply(index,2,function(x){
#     sum(Salary[x]) <= salary.left &
#       sum(Team[x] %in% subset(newPlayerList[[i]],Position=="RB")$Team) < 1 & # No RB and TE of same team
#       (sum(Name[x] %in% subset(template,Position=="TE"& QB==newPlayerList[[i]]$Name[1])$PC) > 0 |
#          (sum(Name[x] %in% subset(template,Position=="TE"& QB==newPlayerList[[i]]$Name[1])$PC) == 0 &
#             sum(te.salary$Name %in% subset(template,Position=="TE"& QB==newPlayerList[[i]]$Name[1])$PC) == 0))
#   }))
#   teList <- list()
#   if(sum(good_rs)>0){
#   teList <- do.call(list,apply(index[,good_rs],2,function(x){
#     rbind(newPlayerList[[i]],te.salary[x,])
#     
#   }
#   ))}
#   print(Sys.time())
#   print("TE")
#   tePlayerList <- append(tePlayerList,teList)
# }
# 
# tePlayerList <- lapply(tePlayerList,function(x){
#   if(nrow(x)==7){
#     return(x)
#   }
# })
# 
# tePlayerList <- Filter(Negate(is.null),tePlayerList)
# 
# print("Working on forming defense combinations")
# print(Sys.time())
# 
# dstPlayerList <- list()
# 
# for(i in seq(1:length(tePlayerList))){
#   salary.taken <- sum(tePlayerList[[i]]$Salary)
#   salary.left <- 5000-salary.taken
#   index <- combnPrim(nrow(dst.salary),1)
#   good_rs <- with(dst.salary,apply(index,2,function(x){
#     sum(Salary[x]) <= salary.left &
#       sum(Team[x] %in% subset(tePlayerList[[i]],Position=="QB")$Team) < 1 & #No QB and DST of same team
#       sum(Team[x] %in% subset(tePlayerList[[i]],Position=="RB")$Team) < 1 & #No RB and DST of same team
#       sum(Team[x] %in% subset(tePlayerList[[i]],Position=="RB")$Home) < 1 & #No RB and DST of home team
#       sum(Team[x] %in% subset(tePlayerList[[i]],Position=="RB")$Away) < 1 & #No RB and DST of away team
#       (sum(Team[x] %in% subset(template,QB==tePlayerList[[i]]$Name[1])$DEF) > 0 |
#          (sum(Team[x] %in% subset(template,QB==tePlayerList[[i]]$Name[1])$DEF) == 0 &
#             sum(dst.player$Name %in% subset(template,QB==tePlayerList[[i]]$Name[1])$DEF)==0))
#   }))
#   dstList <- do.call(list,apply(index[,good_rs],2,function(x){
#     rbind(tePlayerList[[i]],dst.salary[x,])
#   }))
#   print(Sys.time())
#   print("DEF")
#   dstPlayerList <- append(dstPlayerList,dstList)
# }
# 
# dstPlayerList <- lapply(dstPlayerList,function(x){
#   if(nrow(x)==8){
#     return(x)
#   }
# })
# 
# dstPlayerList <- Filter(Negate(is.null),dstPlayerList)
# print("Working on forming flex combinations")
# print(Sys.time())
# 
# finalPlayerList <- list()
# 
# for(i in seq(1:length(dstPlayerList))){
#   salary.taken <- sum(dstPlayerList[[i]]$Salary)
#   salary.left <- 5000-salary.taken
#   wr.subset <- subset(wr.salary,!(Name %in% subset(dstPlayerList[[i]],Position=="WR")$Name))
#   index <- combnPrim(nrow(wr.subset),1)
#   good_rs <- with(wr.subset,apply(index,2,function(x){
#     sum(Salary[x]) <= salary.left & salary.taken > 49900
#       sum(Team[x] %in% subset(dstPlayerList[[i]],Position=="RB")$Team) < 1
#   }))
#   finalList <- do.call(list,apply(index[,good_rs],2,function(x){
#     rbind(dstPlayerList[[i]],wr.subset[x,])
#   }))
#   print(Sys.time())
#   print("Flex")
#   finalPlayerList <- append(finalPlayerList,finalList)
# }
# 
# finalPlayerList <- lapply(finalPlayerList,function(x){
#   if(nrow(x)==9){
#     return(x)
#   }
# })
# 
# finalPlayerList <- Filter(Negate(is.null),finalPlayerList)
# 
# print("Formed all the possible teams in finalPlayerList")
# print(Sys.time())
# save(finalPlayerList,file="finalPlayerList")
# 
# 
# 
# 
# # url <- "http://www.pro-football-reference.com/players/C/CassMa00/gamelog/"
# 
# # study <- htmlParse(url)
# 
# 
# 
