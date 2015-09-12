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
webElem$clickElement()
webElem$sendKeysToElement(list("Aaron Rodgers",key="enter"))
webElem <- remDr$findElement(using="xpath","//*[@id='lineupSelection']/div/div/table/tbody/tr/td[7]/a/span")
webElem$clickElement()
webElem <- remDr$findElement(using="xpath","//*[@id='playerSearch']")
webElem$clickElement()
webElem$sendKeysToElement(list("Martellus Bennett",key="enter"))
webElem <- remDr$findElement(using="xpath","//*[@id='lineupSelection']/div/div/table/tbody/tr/td[7]/a/span")
webElem$clickElement()


webElem <- remDr$findElement(using="xpath","//*[@id='content']/div/div[2]/div[2]/div/div/div/div[2]/div[2]/button")
webElem$isElementEnabled()
if(webElem$isElementEnabled()){
  webElem$clickElement()
}

remDr$close()
remDr$quit()
