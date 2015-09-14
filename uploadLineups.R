if(!require(RSelenium)){
  install.packages("RSelenium")
  require(RSelenium)
} else {
  require(RSelenium)
}

checkForServer()
startServer()
checkForServer()

url <- "https://www.draftkings.com/"

system("java -jar selenium-server-standalone-2.47.1.jar")

cprof <- getChromeProfile("C:\\Users\\AGORE\\AppData\\Local\\Google\\Chrome\\User Data","Default")

remDr <- remoteDriver(browser="chrome" ,extraCapabilities=cprof)

remDr$open()

remDr$navigate(url)

webElem <- remDr$findElement(using="css selector",".navbar-right > li:nth-child(3) > a:nth-child(1)")
webElem$clickElement()
userName <- "rhapsodygreat"
password <- "amcfitchburg19"

webElem <- remDr$findElement(using="css selector","#fancybox-content > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > form:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(1) > input:nth-child(1)")
webElem$sendKeysToElement(list(userName,key="tab",password,key="enter"))
# webElem <- remDr$findElement(using="css selector",".sports-nav > div:nth-child(1) > ul:nth-child(1) > li:nth-child(2) > a:nth-child(1)")
# webElem$clickElement()
webElem <- remDr$findElement(using="css selector","body > header > div.sports-nav > div > ul > li:nth-child(2) > a") #NFL
webElem$clickElement()
# webElem <- remDr$findElement(using="css selector","div.ui-widget-content:nth-child(5) > div:nth-child(7) > a:nth-child(1) > span:nth-child(1)")
# webElem$clickElement()
webElem <- remDr$findElement(using="xpath","//*[@id='lobby-grid']/div[5]/div/div[10]/div[7]/a")
webElem$clickElement()
webElem <- remDr$findElement(using="css selector","#fancybox-close")
webElem$clickElement()
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
remDr$close()
remDr$quit()
