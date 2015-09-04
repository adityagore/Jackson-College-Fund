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

fprof <- getFirefoxProfile("firefoxprofile",useBase=TRUE)

remDr <- remoteDriver(browser="firefox",extraCapabilities=fprof)

remDr$open()

remDr$navigate(url)

webElem <- remDr$findElement(using="css selector",".navbar-right > li:nth-child(3) > a:nth-child(1)")
webElem$clickElement()
userName <- "rhapsodygreat"
password <- "amcfitchburg19"

webElem <- remDr$findElement(using="css selector","#fancybox-content > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > form:nth-child(1) > div:nth-child(2) > div:nth-child(2) > div:nth-child(1) > input:nth-child(1)")
webElem$sendKeysToElement(list(userName,key="tab",password,key="enter"))
webElem <- remDr$findElement(using="css selector",".sports-nav > div:nth-child(1) > ul:nth-child(1) > li:nth-child(2) > a:nth-child(1)")
webElem$clickElement()
webElem <- remDr$findElement(using="css selector","div.ui-widget-content:nth-child(5) > div:nth-child(7) > a:nth-child(1) > span:nth-child(1)")
webElem$clickElement()
