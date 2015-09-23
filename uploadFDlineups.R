if(!require(RSelenium)){
  install.packages("RSelenium")
  require(RSelenium)
} else {
  require(RSelenium)
}

load("totaltotaltickets.RData")
enterTickets <- sample(1:length(finalTickets),400,replace=FALSE)
finalTickets <- finalTickets[enterTickets]
save(finalTickets,file="enterTicketsFD.RData")
load("enterTicketsFD.RData")
dkTickets <- getUniqueTickets(finalTickets)

checkForServer()
startServer()
checkForServer()

url <- "https://www.fanduel.com/"

# system("java -jar selenium-server-standalone-2.47.1.jar")

cprof <- getChromeProfile("C:\\Users\\AGORE\\AppData\\Local\\Google\\Chrome\\User Data","Default")

remDr <- remoteDriver(browser="chrome" ,extraCapabilities=cprof)

remDr$open()

remDr$navigate(url)

# webElem <- remDr$findElement(using="css selector",".navbar-right > li:nth-child(3) > a:nth-child(1)")
# webElem$clickElement()
# userName <- "rhapsodygreat"
# password <- "amcfitchburg19"
# 
# webElem <- remDr$findElement(using="css selector","#fancybox-content > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > form:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(1) > input:nth-child(1)")
# webElem$sendKeysToElement(list(userName,key="tab",password,key="enter"))
# webElem <- remDr$findElement(using="css selector",".sports-nav > div:nth-child(1) > ul:nth-child(1) > li:nth-child(2) > a:nth-child(1)")
# webElem$clickElement()
webElem <- remDr$findElement(using="css selector","body > header > div.sports-nav > div > ul > li:nth-child(2) > a") #NFL
webElem$clickElement()
# webElem <- remDr$findElement(using="css selector","div.ui-widget-content:nth-child(5) > div:nth-child(7) > a:nth-child(1) > span:nth-child(1)")
# webElem$clickElement()
# htmlFile <- htmlParse(remDr$getPageSource(),asText = TRUE)
# 
# xpath1 <- "//*[@id='lobby-grid']/div[5]/div/div"
# xpath2 <- "/div[2]/a"
# xpath3 <- "/div[7]/a"
# contestNames <- getNodeSet(htmlFile,paste0(xpath1,xpath2,"/text()"))
# print(contestNames)
# contestenter <- readline("Please enter which contest to enter: ")
# webElem <- remDr$findElement(using="xpath",paste0(xpath1,"[",contestenter,"]",xpath3))
# url <- webElem$getElementAttribute("href")
# remDr$navigate(url[[1]])


webElem <- remDr$findElement(using="xpath","//*[@id='lobby-grid']/div[5]/div/div[13]/div[7]/a")
webElem$clickElement()

webElem <- remDr$findElement(using="xpath","//*[@id='enter-wrap']/table/tbody/tr[1]/td[2]/a")
webElem$clickElement()


system.time(for(j in 4:length(dkTickets)){
  print(j)
  playerNames <- gsub("(\\w*\\.*\\w*\\.*\\w*\\s*\\w*).*\\.?$","\\1",dkTickets[[j]])
  print(playerNames)
  for(i in 1:length(playerNames)){
    webElem <- remDr$findElement(using="id","playerSearch")
    webElem$clickElement()
    # print(name)
    webElem$sendKeysToElement(list(playerNames[i],key="enter"))
    #     remDr$setImplicitWaitTimeout(2000)
    #     webElem$sendKeysToElement(list(key="enter"))
    # remDr$setImplicitWaitTimeout(5000)
    Sys.sleep(1)
    webElem <- remDr$findElement(using="xpath","//*[@id='ui-skeleton']/div/section/div[2]/div[4]/div[1]/section[2]/div[2]/table/tbody/tr/td[7]/button/i[1]")
    webElem$clickElement()
  }
  Sys.sleep(1)
  webElem <- remDr$findElement(using="id","enterButton")
  webElem$clickElement()
  Sys.sleep(10)
  remDr$setImplicitWaitTimeout(2000)
  webElem <- remDr$findElement(using="xpath","//*[@id='whatnext']/div[2]/a")
  webElem$clickElement()
  Sys.sleep(8)
  remDr$setImplicitWaitTimeout(2000)
  webElem <- remDr$findElement(using="xpath","//*[@id='ui-skeleton']/div/section/div[2]/div[4]/div[2]/section/section/div/button[2]/small")
  webElem$clickElement()
  remDr$acceptAlert()
  Sys.sleep(1)
})



webElem <- remDr$findElement(using="xpath","//*[@id='body-bg']/header/div/div/nav/ul[1]/li[1]/a")
webElem$clickElement()
## End here

contestLocation <- getNodePosition(contestNodes)
webElem <- remDr$findElement(using="xpath","//*[@id='lobby-grid']/div[5]/div/div[10]/div[7]/a")
webElem$clickElement()
webElem <- remDr$findElement(using="css selector","#fancybox-close")
webElem$clickElement()


# For Drafting
webElem <- remDr$findElement(using="xpath","//*[@id='draft-panel']/div/div[1]/div[6]/ul/li[7]")
webElem$clickElement()

webElem <- remDr$findElement(using="xpath","//*[@id='draft-panel']/div/div[1]/div[4]/div/input")
webElem$clickElement()
webElem$sendKeysToElement(list("Aaron Rodgers",key="enter"))
webElem <- remDr$findElement(using="xpath","//*[contains(@id,'player-picker')]/div[5]/div/div/div[7]/a")
webElem$clickElement()
webElem <- remDr$findElement(using="xpath","//*[@id='draft-panel']/div/div[1]/div[4]/div/input")
webElem$clickElement()
webElem$sendKeysToElement(list("Martellus Bennett",key="enter"))
webElem <- remDr$findElement(using="xpath","//*[contains(@id,'player-picker')]/div[5]/div/div/div[7]/a")
webElem$clickElement()


#Submit Ticket:
webElem <- remDr$findElement(using="xpath","/html/body/div[2]/div/div[3]/div[4]/div[2]/a[4]")
webElem$clickElement()


remDr$close()
remDr$quit()
