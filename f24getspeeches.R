library(Sentida)
library(RSelenium)
library(rvest)
library(spacyr)
library(tidyverse)
library(ggraph)
library(igraph)
library(tif)

# selenium
f_opts <- list(
  binary = "/Applications/Firefox.app/Contents/MacOS/firefox"
)

# Set capabilities for RSelenium
capabilities <- list(
  `moz:firefoxOptions` = f_opts
)
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4444L,
                                 extraCapabilities = capabilities,
                                 browserName = "firefox")
remDr$open()
url="https://www.dansketaler.dk/soeg?tag=1.+maj-tale"
urlp="https://www.dansketaler.dk/soeg?q=pr%C3%A6diken&tag=Pr%C3%A6diken"
remDr$navigate(urlp)
# Navigate to the page
# Find all link elements within the search results
link_elements <- remDr$findElements(using = "class name", "special")
#link_elements <- remDr$findElements(using = "css selector", ".speech-hit")
speeches <- list()
speeches2 <- list()
speechespreach <- list()
links <- sapply(link_elements, function(x) x$getElementAttribute("href")[[1]] )
links <- links[!is.na(links)]  # Remove NA values that represent failed extractions
tlink=links[[1]]

# Iterate over each link and scrape the content
for (link in links) {
  remDr$navigate(link)
  Sys.sleep(3)
  # Assuming you want to scrape the speech text, identify the correct selector for the speech text
  speech_text <- remDr$findElement(using = "css selector", ".speech-article-content")
  speech_content <- speech_text$getElementText()[[1]]
  # Store the scraped content in a list or data frame
  speechespreach[[link]] <- speech_content
  
  # Optionally, pause between requests to reduce the load on the server
  Sys.sleep(1)
}

wh = remDr$getWindowHandles()
remDr$switchToWindow(wh[[1]])  # Assuming main is the first one
remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")

dfallsp <- data.frame(
  name = names(speeches2),
  value = unlist(speeches2)
)
dfallsppreach <- data.frame(
  name = names(speechespreach),
  value = unlist(speechespreach)
)
dfallsp[3,]

dfallsp['length']=lapply(dfallsp['value'],function(x) nchar(x))

rownames(dfallsp)=NULL


