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
save(ContestData,ContestData2,file="ContestData.rda")

load("ContestData.rda")
PlayerCount <- ddply(ContestData2,.(ContestNumber,EntryId,variable),summarise,Player=count(value)$x,Tickets=count(value)$freq)

qbCount <- subset(PlayerCount,variable=="QB")
rbCount <- count(subset(dk.table2,value%in%rbNames)$value)
wrCount <- count(subset(dk.table2,value%in%wrNames)$value)
teCount <- count(subset(dk.table2,value%in%teNames)$value)
dstCount <- count(subset(dk.table2,value%in%dstNames)$value)

totalTickets <- sum(qbCount$freq)

qbCount$"%Owned" <- round(qbCount$freq*100/totalTickets,digits=4)
rbCount$"%Owned" <- round(rbCount$freq*100/totalTickets,digits=4)
wrCount$"%Owned" <- round(wrCount$freq*100/totalTickets,digits=4)
teCount$"%Owned" <- round(teCount$freq*100/totalTickets,digits=4)
dstCount$"%Owned" <- round(dstCount$freq*100/totalTickets,digits=4)

qbCount <- qbCount[order(qbCount$freq,decreasing = TRUE),]
rbCount <- rbCount[order(rbCount$freq,decreasing = TRUE),]
wrCount <- wrCount[order(wrCount$freq,decreasing = TRUE),]
teCount <- teCount[order(teCount$freq,decreasing = TRUE),]
dstCount <- dstCount[order(dstCount$freq,decreasing = TRUE),]

write.csv(qbCount,file = "QB_owned.csv",row.names = FALSE)
write.csv(rbCount,file = "RB_owned.csv",row.names = FALSE)
write.csv(wrCount,file = "WR_owned.csv",row.names = FALSE)
write.csv(teCount,file = "TE_owned.csv",row.names = FALSE)
write.csv(dstCount,file = "DST_owned.csv",row.names = FALSE)

setkey(dk.table2,EntryId)

