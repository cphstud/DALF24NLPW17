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




#Spacy
spacy_initialize(model = "da_core_news_sm")
#Anders fog mangler
anderF=readRDS("fogigen.rds")
anderF=anderF %>% rowwise() %>% mutate(taler=getSpeaker(link))
anderF <- anderF %>% mutate(year=str_extract(link,"[0-9]{4}"))
anderF <- anderF %>% mutate(taler=str_remove(taler,"\x2d"))
anderF <- anderF %>% mutate(lastname=str_extract(taler," [:alpha:]+[\\s]?$"))
anderF <- anderF %>% rename(text=content)
anderF <- anderF %>% rename(url=link)
anderF <- anderF %>% mutate(year=paste0(year,"-01-01"))
anderF <- anderF %>% mutate(year2=as.Date(year))
anderF <- anderF %>% mutate(text=gsub("\\s+"," ",text))
anderF <- anderF %>% rowwise() %>% mutate(sentiment=sentida(text,output = "mean"))
anderF <- anderF %>% mutate(size=nchar(text))
anderF <- anderF %>% mutate(title=str_extract(url,"[:alpha:]+-[0-9]{4}"))
for(i in (1:nrow(anderF))) {
  anderF[i,"doc_id"]=i
}
#Mette F mangler
metteF=readRDS("metteF.rds")
metteF=metteF %>% rowwise() %>% mutate(taler=getSpeaker(link))
metteF <- metteF %>% mutate(year=str_extract(link,"[0-9]{4}"))
metteF <- metteF %>% mutate(taler=str_remove(taler,"\x2d"))
metteF <- metteF %>% mutate(lastname=str_extract(taler," [:alpha:]+[\\s]?$"))
metteF <- metteF %>% rename(text=content)
metteF <- metteF %>% rename(url=link)
metteF <- metteF %>% mutate(year=paste0(year,"-01-01"))
metteF <- metteF %>% mutate(year2=as.Date(year))
metteF <- metteF %>% mutate(text=gsub("\\s+"," ",text))
metteF <- metteF %>% rowwise() %>% mutate(sentiment=sentida(text,output = "mean"))
metteF <- metteF %>% mutate(size=nchar(text))
metteF <- metteF %>% mutate(title=str_extract(url,"[:alpha:]+-[0-9]{4}"))
for(i in (1:nrow(metteF))) {
  metteF[i,"doc_id"]=i+nrow(anderF)
}
metteF2=metteF
metteF=metteF[1:9,] 

#Poul S mangler
poul=readRDS("poulscl.rds")
poul=poul %>% rowwise() %>% mutate(taler=getSpeaker(link))
poul <- poul %>% mutate(year=str_extract(link,"[0-9]{4}"))
poul <- poul %>% mutate(taler=str_remove(taler,"\x2d"))
poul <- poul %>% mutate(lastname=str_extract(taler," [:alpha:]+[\\s]?$"))
poul <- poul %>% rename(text=content)
poul <- poul %>% rename(url=link)
poul <- poul %>% mutate(year=paste0(year,"-01-01"))
poul <- poul %>% mutate(year2=as.Date(year))
poul <- poul %>% mutate(text=gsub("\\s+"," ",text))
poul <- poul %>% rowwise() %>% mutate(sentiment=sentida(text,output = "mean"))
poul <- poul %>% mutate(size=nchar(text))
poul <- poul %>% mutate(title=str_extract(url,"[:alpha:]+-[0-9]{4}"))
for(i in (1:nrow(poul))) {
  poul[i,"doc_id"]=i+nrow(anderF)+nrow(metteF)
}
poul2=poul

# find locations som nævnes

# now plot
speechesMFAF = rbind(metteF,anderF)

# time to plot
  ggplot(speechesMFAF,aes(x=year2,y=sentiment,color=lastname))+
  geom_bar(stat="identity",width=0.9)+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




##############
speeches=readRDS("alletaler.rds")
speeches$doc_id <- 1:nrow(speeches) 
speeches <- speeches %>% rename(text=content)
# clean
speeches2 <- speeches %>% mutate(url=str_remove(url,'tale_permalink="|"$'))
speeches2 <- speeches2 %>% mutate(url=str_remove(url,'taler_permalink="|"$'))

#META DATA: hvem og hvornår
spMeta=speeches %>% select(url,title,doc_id)
# clean meta
spMeta <- spMeta %>% mutate(url=str_remove(url,'"|^tale_permalink="|^taler_permalink="|"$'),
                            url=str_remove(url,'"'))
# get year from url
spMeta <- spMeta %>% mutate(year=str_extract(url,"[0-9]{4}"))
# get speaker via url and scrape
url=spMeta[1,"url"]
tag=".single-tale__single-info.single-tale__single-info__taler-navn"

# test single row
page=rvest::read_html(url$url)
getSp <- page %>% html_nodes("a") %>% html_attr("title")
speaker <- page %>% html_elements(tag) %>% html_nodes("a") %>% html_attr("title")
spMet2=spMeta %>% slice(-114)
spMet2=spMet2 %>% slice(-147)
spMet2 <- spMet2 %>% mutate(year=str_extract(url,"[0-9]{4}"))

# do for all
spMet2 <- spMet2 %>% rowwise() %>% mutate(taler=getSpeaker(url))
spMet2 <- spMet2 %>% mutate(taler=str_remove(taler,"\x2d"))
spMet2 <- spMet2 %>% mutate(lastname=str_extract(taler," [:alpha:]+[\\s]?$"))

# now filter

#UTIL
# find liste over danske statsministre
dkSM=read_html("/Users/thor/Downloads/statsministre i Danmark - Oversigt fra 1848 til nu - lex.dk.mhtml")
dkSMListe=dkSM %>% html_table()
dkSMListeF=dkSMListe[[5]]
# alternativ
dktag=".ct-section-inner-wrap"
itag=".ct-new-columns"
dkPage=read_html("https://leksikongen.dk/statsminister-i-danmark/")
stuf=dkPage %>% html_elements(dktag) 
dkStatsMins <- stuf[[5]] %>% html_elements(itag) %>% html_text() %>% as.data.frame()
dkStatsMins <- dkStatsMins %>% mutate(year=str_extract(text,"[0-9]{4}"))
dkStatsMins <- dkStatsMins %>% mutate(minister=str_remove_all(text,"[0-9]"))
#dkStatsMins <- dkStatsMins %>% mutate(minister=str_remove_all(text,"[^a-zA-ZæøåÆØÅ]"))
dkStatsMins <- dkStatsMins %>% mutate(minister=str_remove_all(minister,"\\(.*\\)"))
dkStatsMins <- dkStatsMins %>% mutate(minister=str_remove_all(minister,"\\(.*"))
dkStatsMins <- dkStatsMins %>% mutate(minister=str_remove_all(minister,"\xe2\x80\x91|\x2d"))
dkStatsMins <- dkStatsMins %>% mutate(lastname=str_extract(minister," [:alpha:]+\\s?$"))

# filtrer talerlisten ud fra listen over statsministre
oklist=paste(dkStatsMins$lastname,collapse = "|")
spMet2f <- spMet2 %>% filter(grepl(oklist,lastname)) %>% arrange(year)
spMet3 <- spMet2 %>% select(doc_id,year)


# TIF format
spTif <- speeches %>% select(doc_id,text)
spTifT <- spTif %>% slice(67)
spTifTPars <- spacy_parse(spTifT, 
                          pos = TRUE,
                          tag = TRUE,
                          lemma = TRUE,
                          entity = TRUE,
                          dependency = TRUE,
                          nounphrase = TRUE,
                          multithread = TRUE)

nchar(spTif$text)
summary(spTif$text)
spTif$text[[47]]
nchar(spTifT$text)


es=entity_extract(spTifTPars, type = "all")

# Mette F mangler
sp2=getSpeech(speeches7[83,"url"]) %>% as.data.frame()
colnames(sp2)="text"
sp2merged_speech <- aggregate(text ~ 1, data = sp2, paste, collapse = " ")
speeches7[83,"text"]=sp2merged_speech

speeches5=inner_join(speeches4,spMet3,by="doc_id")
#Mette F mangler

# cast to date
speeches5 = speeches5 %>% select(-year.x)
speeches5 = speeches5 %>% rename(year=year.y,title=title.y,url=url.y)
speeches5 <- speeches5 %>% mutate(year=paste0(year,"-01-01"))
speeches5 <- speeches5 %>% mutate(year2=as.Date(year))
speeches5 <- speeches5 %>% mutate(size=nchar(text))
speeches5 <- speeches5 %>% mutate(text=gsub("\\s+"," ",text))

speeches6 <- speeches5 %>% slice(-(151:152))
speeches6 <- speeches6 %>% rowwise() %>% mutate(sentiment=sentida(text,output = "mean"))
speeches7 <- speeches6 %>% filter(grepl(lastname,oklist)) %>% arrange(year)
speeches8 <- speeches7 %>% mutate(text=gsub("\\s+"," ",text))
speeches8 <- speeches8 %>% mutate(size=nchar(text))
speeches8 <- speeches8 %>% rowwise() %>% mutate(sentiment=sentida(text,output = "mean"))
speeches9 = rbind(speeches8,anderF)

# time to plot
speeches9 %>% filter(year2 > "1965-01-01") %>% 
  ggplot(aes(x=year2,y=sentiment,color=lastname))+
  geom_bar(stat="identity",width=0.9)+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



getSpeech <- function(x) {
  Sys.sleep(1)
  #x=spMet2[147,"url"]
  tag=".single-tale__version"
  tag=".single-tale__content"
  tag=".col-12.col-smtab-7.col-smdesk-5"
  ctag=".single-tale__content"
  speech <- tryCatch({
    page=rvest::read_html(x$url)
    #speech <- page %>% html_nodes(ctag) %>% html_text()
    #speech <- page %>% html_nodes(tag) %>% html_text()
    speech <- page %>% html_nodes("p") %>% html_text()
  },
  error = function(e) {return("Failed")}
  )
  return(speech)
}

getSpeaker <- function(x) {
  Sys.sleep(1)
  tag=".single-tale__single-info.single-tale__single-info__taler-navn"
  speaker <- tryCatch({
    page=rvest::read_html(x)
    getSp <- page %>% html_nodes("a") %>% html_attr("title")
    speaker <- page %>% html_elements(tag) %>% html_nodes("a") %>% html_attr("title")},
    error = function(e) {return("Failed")}
  )
  return(speaker)
}

