if(!require(XML)){
  install.packages("XML")
  require(XML)
} else {
  require(XML)
}
string1 <- "http://www.pro-football-reference.com"
string2 <- "/years/"
string3 <- "/games.htm"
setwd(path.expand("~/nflProject/gamestats/"))

for(i in c(1978:1999)){
  filename <- paste0("nflstats",i,".csv")
  url <- paste0(string1,string2,i,string3)
  print(url)
  firstPage <- htmlParse(url)
  links <- xpathSApply(firstPage,"//a/@href")
  links <- links[grep("boxscores",links)]
  links <- paste0(string1,links)
  at.symbol <- xpathSApply(firstPage,"/html/body/div[1]/div[3]/div[3]/table/tbody/tr/td[6]/text()")
  firstPage.data <- readHTMLTable(url,stringsAsFactors=FALSE)
  firstPage.data <- firstPage.data$games[grep("\\d+",firstPage.data$games$Week),]  
  dates <- as.Date(paste0(firstPage.data$Date,",",i), "%B %d,%Y")
  offense.teams <- firstPage.data$"Winner/tie"
  defense.teams <- firstPage.data$"Loser/tie"
  offense.scores <- as.numeric(firstPage.data$PtsW)
  defense.scores <- as.numeric(firstPage.data$PtsL)
  table <- data.frame(as.numeric(),as.numeric(),as.numeric(),
                      as.numeric(),as.numeric(),as.numeric(),
                      as.numeric(),as.numeric(),as.numeric(),
                      as.numeric(),as.numeric(),as.numeric(),
                      as.numeric(),as.numeric(),as.numeric(),
                      as.numeric(),as.numeric(),as.numeric(),
                      as.numeric(),as.numeric(),as.numeric(),
                      as.numeric(),as.numeric(),as.numeric(),
                      as.numeric())
  title <- c("Date","TeamName","ScoreOff","FirstDownOff", "RushAttOff",
             "RushYdsOff","PassAttOff","PassCompOff",
             "PassYdsOff","PassIntOff","FumblesOff","SackYdsOff",
             "PenYdsOff","Opponent","ScoreDef",
             "FirstDownDef","RushAttDef","RushYdsDef","PassAttDef",
             "PassCompDef","PassYdsDef","PassIntDef","FumblesDef","SackYdsDef",
             "PenYdsDef")
  
  names(table) <- title
  n <- nrow(firstPage.data)+3
  for(index in 3:n){
    table.current <- readHTMLTable(links[index],stringsAsFactors=FALSE)
    firstdownoff <- as.numeric(table.current$team_stats[1,2])
    firstdowndef <- as.numeric(table.current$team_stats[1,3])
    rushattoff <- as.numeric(strsplit(table.current$team_stats[2,2],split="-")[[1]][1])
    rushattdef <- as.numeric(strsplit(table.current$team_stats[2,3],split="-")[[1]][1])
    rushydsoff <- as.numeric(strsplit(table.current$team_stats[2,2],split="-")[[1]][2])
    rushydsdef <- as.numeric(strsplit(table.current$team_stats[2,3],split="-")[[1]][2])
    passattoff <- as.numeric(strsplit(table.current$team_stats[3,2],split="-")[[1]][2])
    passattdef <- as.numeric(strsplit(table.current$team_stats[3,3],split="-")[[1]][2])
    passcompoff <- as.numeric(strsplit(table.current$team_stats[3,2],split="-")[[1]][1])
    passcompdef <- as.numeric(strsplit(table.current$team_stats[3,3],split="-")[[1]][1])
    passydsoff <- as.numeric(strsplit(table.current$team_stats[3,2],split="-")[[1]][3])
    passydsdef <- as.numeric(strsplit(table.current$team_stats[3,3],split="-")[[1]][3])
    passintoff <- as.numeric(strsplit(table.current$team_stats[3,2],split="-")[[1]][5])
    passintdef <- as.numeric(strsplit(table.current$team_stats[3,3],split="-")[[1]][5])
    fumblesoff <- as.numeric(strsplit(table.current$team_stats[7,2],split="-")[[1]][1])
    fumblesdef <- as.numeric(strsplit(table.current$team_stats[7,3],split="-")[[1]][1])
    sackydsoff <- as.numeric(strsplit(table.current$team_stats[4,2],split="-")[[1]][2])
    sackydsdef <- as.numeric(strsplit(table.current$team_stats[4,3],split="-")[[1]][2])
    penydsoff <- as.numeric(strsplit(table.current$team_stats[9,2],split="-")[[1]][2])
    penydsdef <- as.numeric(strsplit(table.current$team_stats[9,3],split="-")[[1]][2])
    table.add <- as.data.frame(list(
      "Date" = dates[index-2],"TeamName" = offense.teams[index-2],"ScoreOff"=offense.scores[index-2],
      "FirstDownOff"=firstdownoff, "RushAttOff"=rushattoff,
      "RushYdsOff"=rushydsoff,"PassAttOff"=passattoff,"PassCompOff"=passcompoff,
      "PassYdsOff"=passydsoff,"PassIntOff"=passintoff,"FumblesOff"=fumblesoff,"SackYdsOff"=sackydsoff,
      "PenYdsOff"=penydsoff,"Opponent" = defense.teams[index-2],"ScoreDef" = defense.scores[index-2],
      "FirstDownDef"=firstdowndef,"RushAttDef"=rushattdef,"RushYdsDef"=rushydsdef,"PassAttDef"=passattdef,
      "PassCompDef"=passcompdef,"PassYdsDef"=passydsdef,"PassIntDef"=passintdef,"FumblesDef"=fumblesdef,
      "SackYdsDef"=sackydsdef,
      "PenYdsDef"=penydsdef
      ))
    table <- rbind(table,table.add)
    print(paste0(length(links)-index,links[index]))
  }
  write.csv(table,row.names=FALSE,file=filename)  
  
}