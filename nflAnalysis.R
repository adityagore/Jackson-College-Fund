currentPath <- getwd()

folderPath <- currentPath
folderPath <- paste0(folderPath,"/Data/")

weeks <- list.files(path=folderPath)

fileLists <- list()
filePaths <- list()

for(week in weeks){
  filePaths[[week]] <- paste0(paste0(folderPath,week,"/"))
  fileLists[[week]] <- paste0(filePaths[[week]],grep(".*csv",list.files(path=filePaths[[week]]),value=TRUE))
}


system.time(ContestData <- lapply(as.list(unlist(fileLists)),
                                               readParseTable))
ContestData <- rbindlist(ContestData)

ContestData <- data.table(ContestData)
setkeyv(ContestData,c("Points","QB","RB1","RB2","WR1","WR2","WR3",
                      "FLEX","TE","DST"))

ContestData <- subset(ContestData,Points!=0 &
                        !(QB %in% "LOCKED") &
                        !(RB1 %in% "LOCKED") &
                        !(RB2 %in% "LOCKED") &
                        !(WR1 %in% "LOCKED") &
                        !(WR2 %in% "LOCKED") &
                        !(WR3 %in% "LOCKED") &
                        !(TE %in% "LOCKED") &
                        !(FLEX %in% "LOCKED") &
                        !(DST %in% "LOCKED"))





setwd(currentPath)



ContestData2 <- melt(ContestData,measure=c("QB","RB1","RB2","WR1","WR2","WR3","TE","FLEX","DST"),id=c("ContestNumber","EntryId"))

salary.data <- read.csv("SalaryHistory.csv",stringsAsFactors = FALSE)
salary.data <- data.table(salary.data)
salary.data$Salary <- NULL
salary.data$GameInfo <- NULL
salary.data <- unique(salary.data)
qbNames <- unique(subset(salary.data,Position=="QB")$Name)
wrNames <- unique(subset(salary.data,Position=="WR")$Name)
rbNames <- unique(subset(salary.data,Position=="RB")$Name)
teNames <- unique(subset(salary.data,Position=="TE")$Name)
dstNames <- unique(subset(salary.data,Position=="DST")$Name)
dstNames <- gsub("(\\S+)\\s*","\\1",dstNames)

save(ContestData,ContestData2,file="ContestData.rda")
setkeyv(ContestData2,c("ContestNumber","EntryId","variable"))
load("ContestData.rda")
# PlayerCount <- ddply(ContestData2,.(ContestNumber,variable),summarise,Player=count(value)$x,Tickets=count(value)$freq)
PlayerCount <- ContestData2[,.(Player=as.character(count(value)$x),Tickets=count(value)$freq),by=.(ContestNumber,variable)]

PlayerCount <- data.table(PlayerCount)
setkeyv(PlayerCount,c("ContestNumber","Player","variable"))
qbCount <- PlayerCount[Player %in% qbNames & variable %in% "QB"]
totalCount <- qbCount[,.(TotalTickets=sum(Tickets)),by=.(ContestNumber)]
qbCount <- merge(qbCount,totalCount,by="ContestNumber",all.x=TRUE)
wrCount <- PlayerCount[Player%in%wrNames&(variable%in%"WR1"|
                                            variable%in%"WR2"|
                                            variable%in%"WR3"|
                                            variable%in%"FLEX")]
wrCount <- wrCount[,.(Tickets=sum(Tickets)),by=.(ContestNumber,Player)]
wrCount <- merge(wrCount,totalCount,by="ContestNumber",all.x=TRUE)
teCount <- PlayerCount[Player%in%teNames&(variable%in%"TE"|
                                            variable%in%"FLEX")]
teCount <- teCount[,.(Tickets=sum(Tickets)),by=.(ContestNumber,Player)]
teCount <- merge(teCount,totalCount,by="ContestNumber",all.x=TRUE)
rbCount <- PlayerCount[Player%in%rbNames&(variable%in%"RB1"|
                                            variable%in%"RB2"|
                                            variable%in%"FLEX")]
rbCount <- rbCount[,.(Tickets=sum(Tickets)),by=.(ContestNumber,Player)]
rbCount <- merge(rbCount,totalCount,by="ContestNumber",all.x=TRUE)
dstCount <- PlayerCount[Player%in%dstNames & variable%in%"DST"]
dstCount <- dstCount[,.(Tickets=sum(Tickets)),by=.(ContestNumber,Player)]
dstCount <- merge(dstCount,totalCount,by="ContestNumber",all.x=TRUE)



qbCount$"OwnedPct" <- round(qbCount$Tickets*100/qbCount$TotalTickets,digits=4)
rbCount$"OwnedPct" <- round(rbCount$Tickets*100/rbCount$TotalTickets,digits=4)
wrCount$"OwnedPct" <- round(wrCount$Tickets*100/wrCount$TotalTickets,digits=4)
teCount$"OwnedPct" <- round(teCount$Tickets*100/teCount$TotalTickets,digits=4)
dstCount$"OwnedPct" <- round(dstCount$Tickets*100/dstCount$TotalTickets,digits=4)

qbCount <- qbCount[order(rank(ContestNumber),-rank(Tickets))]
rbCount <- rbCount[order(rank(ContestNumber),-rank(Tickets))]
wrCount <- wrCount[order(rank(ContestNumber),-rank(Tickets))]
teCount <- teCount[order(rank(ContestNumber),-rank(Tickets))]
dstCount <- dstCount[order(rank(ContestNumber),-rank(Tickets))]

qbPlot <- ggplot(qbCount[OwnedPct<6&OwnedPct>4],aes(x=ContestNumber,y=OwnedPct,group=Player))
qbPlot + geom_bar(stat="identity",position = "dodge",aes(fill=ContestNumber))+facet_grid(.~Player)+
  theme_bw()

dstPlot <- ggplot(dstCount,aes(x=Player,y=Tickets,group=ContestNumber))
dstPlot + geom_bar(stat = "identity",position="dodge",aes(fill=ContestNumber))+theme_bw()
ContestNumbers <- unique(PlayerCount[,.(ContestNumber)])[[1]]
for(contestid in ContestNumbers){
  tmp <- data.frame(qbCount[ContestNumber==contestid])
  write.csv(x=tmp,file = paste0(contestid,"_QB_owned.csv"),row.names = FALSE)
  tmp <- data.frame(rbCount[ContestNumber==contestid])
  write.csv(x=tmp,file = paste0(contestid,"_RB_owned.csv"),row.names = FALSE)
  tmp <- data.frame(wrCount[ContestNumber==contestid])
  write.csv(x=tmp,file = paste0(contestid,"_WR_owned.csv"),row.names = FALSE)
  tmp <- data.frame(teCount[ContestNumber==contestid])
  write.csv(x=tmp,file = paste0(contestid,"_TE_owned.csv"),row.names = FALSE)
  tmp <- data.frame(dstCount[ContestNumber==contestid])
  write.csv(x=tmp,file = paste0(contestid,"_DST_owned.csv"),row.names = FALSE)
}

setkey(dk.table2,EntryId)

