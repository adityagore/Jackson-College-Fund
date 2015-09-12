if(!require(RSelenium)){
  install.packages("RSelenium")
  require(RSelenium)
} else {
  require(RSelenium)
}

checkForServer()
startServer()
checkForServer()

url <- "https://sports.yahoo.com/dailyfantasy/"

cprof <- getChromeProfile("C:\\Users\\AGORE\\AppData\\Local\\Google\\Chrome\\User Data","Default")

remDr <- remoteDriver(browser="chrome" ,extraCapabilities=cprof)

system("java -jar selenium-server-standalone-2.47.1.jar")

remDr$open()

remDr$navigate(url)

webElem <- remDr$findElement(using="xpath","//*[@id='content']/div/div[2]/div/div[2]/div[1]/div/ul/li[2]/a/span/span[2]")
webElem$clickElement()
webElem <- remDr$findElement(using="xpath","//*[@id='content']/div/div[2]/div/div[3]/div[2]/table/tbody/tr[1]/td[6]/a")
webElem$clickElement()
webElem <- remDr$findElement(using="xpath","//*[@id='playerSearch']")
for(i in 1:100){
  webElem$sendKeysToElement(list(key="page_down"))
}
getHTMLFile <- htmlParse(remDr$getPageSource(),asText=TRUE)
playerTable <- readHTMLTable(getHTMLFile,stringsAsFactors=FALSE,which=3)
playerTable <- playerTable[,c(1,3,4,6)]
names(playerTable) <- c("Pos","Name","Opponent","Salary")
playerTable$Salary <- gsub("\\$(.*)","\\1",playerTable$Salary)
playerTable$Salary <- as.integer(playerTable$Salary)
write.csv(playerTable,file="YahooSalaries.csv",row.names=FALSE)
remDr$close()
