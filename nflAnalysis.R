currentPath <- getwd()

folderPath <- currentPath
folderPath <- paste0(folderPath,"/Data/")

weeks <- list.files(path=folderPath)
weeks <- weeks[grep("\\w+[^1]$",weeks)]

fileLists <- list()
filePaths <- list()
salaryfiles <- list()

for(week in weeks){
  filePaths[[week]] <- paste0(paste0(folderPath,week,"/"))
  fileLists[[week]] <- paste0(filePaths[[week]],grep(".*csv",list.files(path=filePaths[[week]]),value=TRUE))
  salaryfiles[[week]] <- fileLists[[week]][grep("DKSalaries",fileLists[[week]])]
  fileLists[[week]] <- fileLists[[week]][-grep("Salaries",fileLists[[week]])]
}

n <- readline()

system.time(ContestData <- lapply(as.list(unlist(fileLists)),
                                               readParseTable))
ContestData <- rbindlist(ContestData)
ContestData[,Lineup:=NULL]

ContestData <- data.table(ContestData)
setkeyv(ContestData,c("Points","QB","RB1","RB2","WR1","WR2","WR3",
                      "FLEX","TE","DST"))

ContestData <- ContestData %>% filter(Points!=0) # &
#                         !(QB %in% "LOCKED") &
#                         !(RB1 %in% "LOCKED") &
#                         !(RB2 %in% "LOCKED") &
#                         !(WR1 %in% "LOCKED") &
#                         !(WR2 %in% "LOCKED") &
#                         !(WR3 %in% "LOCKED") &
#                         !(TE %in% "LOCKED") &
#                         !(FLEX %in% "LOCKED") &
#                         !(DST %in% "LOCKED"))
ContestData[,DST:=gsub("(.+)\\s+$","\\1",DST)]

salary.data <- do.call(rbind.data.frame,lapply(salaryfiles,read.csv,stringsAsFactors=FALSE))
salary.data[,"Week"] <- gsub("(.*)\\.\\d*","\\1",rownames(salary.data))
salary.data <- data.table(salary.data)
salary.data[,c("Salary","GameInfo","AvgPointsPerGame"):=NULL]
salary.data[,Name:=gsub("(.+)\\s+$","\\1",Name)]

setwd(currentPath)



ContestData2 <- melt(ContestData,measure=c("QB","RB1","RB2","WR1","WR2","WR3","TE","FLEX","DST"),id=c("ContestNumber","EntryId","Week"))
setnames(ContestData2,c("variable","value"),c("Position","Name"))
ContestData2[,Position := gsub("(\\D+)(\\d*)","\\1 \\2",Position)]
ContestData2 <- ContestData2 %>% separate(Position,into=c("Position","Order"),sep=" ")



# 
# salary.data <- read.csv("SalaryHistory.csv",stringsAsFactors = FALSE)
# salary.data <- data.table(salary.data)
# salary.data[,c("GameInfo","Salary","AvgPointsPerGame"):=NULL]
# salary.data <- salary.data[TeamAbbr!=""]
salary.data <- unique(salary.data)
qbNames <- unique(subset(salary.data,Position=="QB")$Name)
wrNames <- unique(subset(salary.data,Position=="WR")$Name)
rbNames <- unique(subset(salary.data,Position=="RB")$Name)
teNames <- unique(subset(salary.data,Position=="TE")$Name)
dstNames <- unique(subset(salary.data,Position=="DST")$Name)
dstNames <- gsub("(\\S+)\\s*","\\1",dstNames)

# save(ContestData,ContestData2,file="ContestData.rda")
# setkeyv(ContestData2,c("ContestNumber","EntryId","variable"))
# load("ContestData.rda")
# PlayerCount <- ddply(ContestData2,.(ContestNumber,variable),summarise,Player=count(value)$x,Tickets=count(value)$freq)
# ContestData2 <- ContestData2 %>% separate(variable,into = c("Number","Position"),sep="\\D+")
# PlayerCount <- ContestData2[,.(Player=as.character(count(value)$x),Tickets=count(value)$freq),by=.(ContestNumber,variable)]

# totalTickets <- ContestData2 %>% group_by(ContestNumber) %>% summarise(totaltickets=length(unique(EntryId)))
# qbNumbers <- ContestData2 %>% filter(Position%in%"QB") %>% group_by(ContestNumber,Name) %>% summarise(totaltickets=n())
# qbNumbers <- qbNumbers[order(ContestNumber,-totaltickets)]
# qbNumbers[,Position:="QB"]
# wrNumbers <- ContestData2 %>% filter(Position%in%c("WR","FLEX")&Name%in%c(wrNames,"")) %>% group_by(ContestNumber,Name) %>% summarise(totaltickets=n())
# wrNumbers <- wrNumbers[order(ContestNumber,-totaltickets)]
# wrNumbers[,Position:="WR"]
# rbNumbers <- ContestData2 %>% filter(Position%in%c("RB","FLEX")&Name%in%c(rbNames,"")) %>% group_by(ContestNumber,Name) %>% summarise(totaltickets=n())
# rbNumbers <- rbNumbers[order(ContestNumber,-totaltickets)]
# rbNumbers[,Position:="RB"]
# teNumbers <- ContestData2 %>% filter(Position%in%c("TE","FLEX")&Name%in%c(teNames,"")) %>% group_by(ContestNumber,Name) %>% summarise(totaltickets=n())
# teNumbers <- teNumbers[order(ContestNumber,-totaltickets)]
# teNumbers[,Position:="TE"]
# dstNumbers <- ContestData2 %>% filter(Position%in%c("DST")) %>% group_by(ContestNumber,Name) %>% summarise(totaltickets=n())
# dstNumbers <- dstNumbers[order(ContestNumber,-totaltickets)]
# dstNumbers[,Position:="DST"]
setkeyv(ContestData2,c("ContestNumber","Week","Position","Order"))
totalTickets <- ContestData2[,.(totaltickets=length(unique(EntryId))),by=.(ContestNumber,Week)]
qbNumbers <- ContestData2[Position=="QB",.(Tickets=.N,Position="QB"),by=.(ContestNumber,Week,Name)]
wrNumbers <- ContestData2[Position%in%c("WR","FLEX")&Name%in%c(wrNames,""),.(Tickets=.N,Position="WR"),by=.(ContestNumber,Week,Name)]
rbNumbers <- ContestData2[Position%in%c("RB","FLEX")&Name%in%c(rbNames,""),.(Tickets=.N,Position="RB"),by=.(ContestNumber,Week,Name)]
teNumbers <- ContestData2[Position%in%c("TE","FLEX")&Name%in%c(teNames,""),.(Tickets=.N,Position="TE"),by=.(ContestNumber,Week,Name)]
dstNumbers <- ContestData2[Position=="DST",.(Tickets=.N,Position="DST"),by=.(ContestNumber,Week,Name)]
totalNumbers <- rbindlist(list(qbNumbers,wrNumbers,rbNumbers,teNumbers,dstNumbers))
setkey(totalNumbers,ContestNumber)
setkey(totalTickets,ContestNumber)
totalNumbers[,ContestTickets:=totalTickets[ContestNumber][["totaltickets"]]]
totalNumbers[,PercentOwned:=100*Tickets/ContestTickets]

totalPctOwned <- totalNumbers %>% mutate(Contest=paste0(ContestNumber,":",Week)) %>% select(Contest,Position,Name,PercentOwned) %>% spread(Contest,PercentOwned)
setkeyv(totalPctOwned,c("Position","Name"))
totalPctOwned$AvgPct <- rowMeans(totalPctOwned[,c(3,4,5),with=FALSE],na.rm=TRUE)
totalPctOwned <- totalPctOwned[order(Position,AvgPct,decreasing = TRUE)]
write.csv(totalPctOwned,file="PercentOwned.csv",row.names = FALSE)


playerData <- merge(totalNumbers,salary.data,by=c("Position","Name","Week"),all.x=TRUE)
playerData <- playerData[order(ContestNumber,Position,PercentOwned,decreasing = TRUE)]
setnames(playerData,"teamAbbrev","Team")
playerData[,Team:=toupper(Team)]
setnames(salary.data,"teamAbbrev","Team")
ContestData2[,NewPosition:=Position]
ContestData2[NewPosition=="FLEX"&Name%in%wrNames,NewPosition:="WR"]
ContestData2[NewPosition=="FLEX"&Name%in%rbNames,NewPosition:="RB"]
ContestData2[NewPosition=="FLEX"&Name%in%teNames,NewPosition:="TE"]

ContestFullData <- merge(ContestData2,salary.data,by=c("Name","Week","Position"),all.x=TRUE)

ContestFullData[,':='(Stack0=findStacks(Position,Team,0),
                      Stack1=findStacks(Position,Team,1),
                      Stack2=findStacks(Position,Team,2),
                      Stack3=findStacks(Position,Team,3),
                      Stack4=findStacks(Position,Team,4,exact=FALSE),
                      Garbage=findStacks(Position,Team,na=TRUE)),
                by=.(ContestNumber,EntryId,Week)]

StackNumbers <- ContestFullData[,.(Stack0=sum(Stack0)/9,
                                   Stack1=sum(Stack1)/9,
                                   Stack2=sum(Stack2)/9,Stack3=sum(Stack3)/9,
                                   Stack4=sum(Stack4)/9,
                                   Garbage=sum(Garbage)/9),by=.(ContestNumber,Week)]

StackNumbers[,Total:=Stack0+Stack1+Stack2+Stack3+Stack4+Garbage]

StackPercent <- StackNumbers[,.(Stack0=100*Stack0/Total,
                                Stack1=100*Stack1/Total,
                                Stack2=100*Stack2/Total,
                                Stack3=100*Stack3/Total,
                                Stack4=100*Stack4/Total,
                                Garbage=100*Garbage/Total),by=.(ContestNumber,Week)]

ContestFullData[Stack1==TRUE&Position=="QB",Team1:=Team]
ContestFullData[Stack2==TRUE&Position=="QB",Team2:=Team]
ContestFullData[Stack3==TRUE&Position=="QB",Team3:=Team]
ContestFullData[Stack4==TRUE&Position=="QB",Team4:=Team]


# TeamStack <- ContestFullData %>% gather(Stack,Value,Stack1,Stack2,Stack3,Stack4)
TeamStack <- ContestFullData[Position=="QB"] %>% select(-Name,-NewPosition,-Position,-Order,-Stack0,-Garbage) %>% gather(Stack,Value,Stack1,Stack2,Stack3,Stack4)
Stack1Info <- ContestFullData[Stack1==TRUE&Position=="QB",.(Count=.N,Stack="Stack1"),by=.(ContestNumber,Name,Team1)]
Stack2Info <- ContestFullData[Stack2==TRUE&Position=="QB",.(Count=.N,Stack="Stack2"),by=.(ContestNumber,Name,Team2)]
Stack3Info <- ContestFullData[Stack3==TRUE&Position=="QB",.(Count=.N,Stack="Stack3"),by=.(ContestNumber,Name,Team3)]
Stack4Info <- ContestFullData[Stack4==TRUE&Position=="QB",.(Count=.N,Stack="Stack4"),by=.(ContestNumber,Name,Team4)]
Stac1Info <- data.table(Stack1Info)
Stac2Info <- data.table(Stack2Info)
Stac3Info <- data.table(Stack3Info)
Stac4Info <- data.table(Stack4Info)

setnames(Stack1Info,"Team1","Team")
setnames(Stack2Info,"Team2","Team")
setnames(Stack3Info,"Team3","Team")
setnames(Stack4Info,"Team4","Team")

StackInfo <- rbindlist(list(Stack1Info,Stack2Info,Stack3Info,Stack4Info))

StackInfo <- StackInfo[order(ContestNumber,desc(Stack),Count,decreasing = TRUE)]
StackInfo[,Total:=sum(Count),by=.(ContestNumber,Stack)]
StackInfo[,stackPercent:=Count*100/Total]
StackInfo[,totalPercent:=Count*100/totalTickets[ContestNumber][["totaltickets"]]]

# PlayerCount <- ContestData2 %>% group_by(ContestNumber,variable,value) %>%
#   summarise(freq=n())
# 
# PlayerCount <- data.table(PlayerCount)
# setkeyv(PlayerCount,c("ContestNumber","Player","variable"))
# qbCount <- PlayerCount[Player %in% qbNames & variable %in% "QB"]
# totalCount <- qbCount[,.(TotalTickets=sum(Tickets)),by=.(ContestNumber)]
# qbCount <- merge(qbCount,totalCount,by="ContestNumber",all.x=TRUE)
# wrCount <- PlayerCount[Player%in%wrNames&(variable%in%"WR1"|
#                                             variable%in%"WR2"|
#                                             variable%in%"WR3"|
#                                             variable%in%"FLEX")]
# wrCount <- wrCount[,.(Tickets=sum(Tickets)),by=.(ContestNumber,Player)]
# wrCount <- merge(wrCount,totalCount,by="ContestNumber",all.x=TRUE)
# teCount <- PlayerCount[Player%in%teNames&(variable%in%"TE"|
#                                             variable%in%"FLEX")]
# teCount <- teCount[,.(Tickets=sum(Tickets)),by=.(ContestNumber,Player)]
# teCount <- merge(teCount,totalCount,by="ContestNumber",all.x=TRUE)
# rbCount <- PlayerCount[Player%in%rbNames&(variable%in%"RB1"|
#                                             variable%in%"RB2"|
#                                             variable%in%"FLEX")]
# rbCount <- rbCount[,.(Tickets=sum(Tickets)),by=.(ContestNumber,Player)]
# rbCount <- merge(rbCount,totalCount,by="ContestNumber",all.x=TRUE)
# dstCount <- PlayerCount[Player%in%dstNames & variable%in%"DST"]
# dstCount <- dstCount[,.(Tickets=sum(Tickets)),by=.(ContestNumber,Player)]
# dstCount <- merge(dstCount,totalCount,by="ContestNumber",all.x=TRUE)
# 
# 
# 
# qbCount$"OwnedPct" <- round(qbCount$Tickets*100/qbCount$TotalTickets,digits=4)
# rbCount$"OwnedPct" <- round(rbCount$Tickets*100/rbCount$TotalTickets,digits=4)
# wrCount$"OwnedPct" <- round(wrCount$Tickets*100/wrCount$TotalTickets,digits=4)
# teCount$"OwnedPct" <- round(teCount$Tickets*100/teCount$TotalTickets,digits=4)
# dstCount$"OwnedPct" <- round(dstCount$Tickets*100/dstCount$TotalTickets,digits=4)
# 
# qbCount <- qbCount[order(rank(ContestNumber),-rank(Tickets))]
# rbCount <- rbCount[order(rank(ContestNumber),-rank(Tickets))]
# wrCount <- wrCount[order(rank(ContestNumber),-rank(Tickets))]
# teCount <- teCount[order(rank(ContestNumber),-rank(Tickets))]
# dstCount <- dstCount[order(rank(ContestNumber),-rank(Tickets))]
# 
# qbPlot <- ggplot(qbCount[OwnedPct<6&OwnedPct>4],aes(x=ContestNumber,y=OwnedPct,group=Player))
# qbPlot + geom_bar(stat="identity",position = "dodge",aes(fill=ContestNumber))+facet_grid(.~Player)+
#   theme_bw()
# 
# dstPlot <- ggplot(dstCount,aes(x=Player,y=Tickets,group=ContestNumber))
# dstPlot + geom_bar(stat = "identity",position="dodge",aes(fill=ContestNumber))+theme_bw()
# ContestNumbers <- unique(PlayerCount[,.(ContestNumber)])[[1]]
# for(contestid in ContestNumbers){
#   tmp <- data.frame(qbCount[ContestNumber==contestid])
#   write.csv(x=tmp,file = paste0(contestid,"_QB_owned.csv"),row.names = FALSE)
#   tmp <- data.frame(rbCount[ContestNumber==contestid])
#   write.csv(x=tmp,file = paste0(contestid,"_RB_owned.csv"),row.names = FALSE)
#   tmp <- data.frame(wrCount[ContestNumber==contestid])
#   write.csv(x=tmp,file = paste0(contestid,"_WR_owned.csv"),row.names = FALSE)
#   tmp <- data.frame(teCount[ContestNumber==contestid])
#   write.csv(x=tmp,file = paste0(contestid,"_TE_owned.csv"),row.names = FALSE)
#   tmp <- data.frame(dstCount[ContestNumber==contestid])
#   write.csv(x=tmp,file = paste0(contestid,"_DST_owned.csv"),row.names = FALSE)
# }
# 
# setkey(dk.table2,EntryId)
# 
# 
# #Stack Information
# totalTickets <- finalMerge[,.(totaltickets=length(EntryId)),by=.(ContestNumber)]
# stackNumbers <- finalMerge[,.(Stack0=findStacks(Position,TeamAbbr,0),
#                               Stack1=findStacks(Position,TeamAbbr,1),
#                               Stack2=findStacks(Position,TeamAbbr,2),
#                               Stack3=findStacks(Position,TeamAbbr,3),
#                               Stack4=findStacks(Position,TeamAbbr,4),
#                               Stack5plus=findStacks(Position,TeamAbbr,5,exact=FALSE)),
#                            by=.(ContestNumber,EntryId)]
# 
# totalStacks <- stackNumbers[,.(Stack0=sum(Stack0),
#                                Stack1=sum(Stack1),
#                                Stack2=sum(Stack2),
#                                Stack3=sum(Stack3),
#                                Stack4=sum(Stack4),
#                                Stack5plus=sum(Stack5plus)),
#                             by=.(ContestNumber)]
# totalStacks <- merge(totalStacks,totalTickets,by="ContestNumber",all=TRUE)
# totalStacks[,c("Stack0Pct","Stack1Pct","Stack2Pct","Stack3Pct","Stack4Pct","Stack5plusPct"):=list(
#   round(100*Stack0/totaltickets,2),
#   round(100*Stack1/totaltickets,2),
#   round(100*Stack2/totaltickets,2),
#   round(100*Stack3/totaltickets,2),
#   round(100*Stack4/totaltickets,2),
#   round(100*Stack5plus/totaltickets,2)
# )]
# 
# 
