require(RSelenium)

string1 <- "http://www.nfl.com/players/search?category=position&filter="
string2 <- "&conferenceAbbr=null&playerType=current&conference=ALL"
positions <- c("quarterback","runningback","widereceiver","tightend")
playerTeams <- data.frame("Player Name"=numeric(),"Team"=numeric())
names(playerTeams) = c("Player Name","Team")

for(pos in positions){
  url <- paste0(string1,pos,string2)
  t1 <- readHTMLTable(url,stringsAsFactors=FALSE,which=4)
  playerTeams <- rbind(playerTeams,t1[,c("Player Name","Team")])
}


string1 <- "http://www.foxsports.com/nfl/players?season=2015&page="
string2 <- "&position=0"

playerData <- data.frame()

for(i in seq(50)){
  url <- paste0(string1,i,string2)
  playerData <- rbind(playerData,
                      readHTMLTable(url,which=1,stringsAsFactors=FALSE))
}

playerData <- subset(playerData, Pos %in% c("QB","WR","RB","TE"))[,c("Player","Team","Pos")]

playerData$Name <- gsub("(\\D+)\\r\\n\\t+(\\D+)","\\1 \\2",playerData$Player)

playerData$Player = NULL
