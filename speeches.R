library(ggwordcloud)
library(wordcloud2)
library(Sentida)
library(RSelenium)
library(rvest)
library(spacyr)
library(tidyverse)
library(ggraph)
library(igraph)
library(tif)

# selenium
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "firefox")
remDr$open()

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


speechesMFAF = rbind(metteF,anderF)
speechesMFAFPS = rbind(speechesMFAF,poul)

spMetaMFAFPS=speechesMFAFPS %>% select(-c(text,url,year,sentiment,size,title,lastname)) 

# time to plot
ggplot(speechesMFAFPS,aes(x=year2,y=sentiment,color=lastname))+
  geom_bar(stat="identity",width=0.9)+
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# find locations som nævnes
# TIF format
spTif <- speechesMFAFPS %>% select(doc_id,text)
spTifPars <- spacy_parse(spTif, 
                       pos = TRUE,
                       tag = TRUE,
                       lemma = TRUE,
                       entity = TRUE,
                       dependency = TRUE,
                       nounphrase = TRUE,
                       multithread = TRUE)

# NOUNS
#find them
spNouns <- spTifPars %>% filter(pos=="NOUN") %>% select(doc_id,lemma) %>% mutate(doc_id=as.numeric(doc_id))
spNounsInfo <- inner_join(spNouns,spMetaMFAFPS,by="doc_id")
spNounsInfo <- spNounsInfo %>% mutate(lemma=gsub("\xe2\x80\x93",NA,lemma))
afCountNouns <- spNounsInfo %>% count(taler,lemma,sort = T)
afCountNouns <- afCountNouns %>% filter(!is.na(lemma))

# plot them
afCountNouns %>% filter(n>50) %>% 
  ggplot(aes(x=reorder(lemma,n),y=n, fill=taler))+
  geom_bar(stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip() + 
  theme(legend.position='none',
        plot.title = element_text(size =10),
        axis.text = element_text(size=10)) + 
  facet_wrap(~ taler)

#Places
spLocs <- spTifPars %>% filter(nchar(entity)>0) %>% select(doc_id,lemma) %>% mutate(doc_id=as.numeric(doc_id))
spLocs <- inner_join(spLocs,spMetaMFAFPS,by="doc_id")
#spLocs <- spLocs %>% mutate(lemma=gsub("\xe2\x80\x93",NA,lemma))
spLocsCount <- spLocs %>% count(taler,lemma,sort = T)
spLocsCount <- spLocsCount %>% filter(nchar(lemma)>1)
# plot them
spLocsCount %>% filter(n>5) %>% 
  ggplot(aes(x=reorder(lemma,n),y=n, fill=taler))+
  geom_bar(stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip() + 
  theme(legend.position='none',
        plot.title = element_text(size =10),
        axis.text = element_text(size=10)) + 
  facet_wrap(~ taler)

forPlot=spLocsCount %>% filter(n>5) %>% filter(grepl("Mette Frederiksen",taler))
forPlot=spLocsCount %>% filter(n>5)
ggplot(spLocsCountMF,aes(label = word, 
                   size = freq
                   #color = factor(sample.int(10, nrow(forPlot), replace = TRUE))
                   )) +
geom_text_wordcloud() +
  scale_size_area(max_size = 13) +
theme_minimal() 
    #facet_wrap(~ taler)

spLocsCountMF <- spLocsCount %>% filter(n>5) %>% filter(taler==" Mette Frederiksen") %>% select(lemma,n)
spLocsCountMF <- spLocsCountMF %>% rename(word=lemma) %>% rename(freq=n)
wordcloud2(spLocsCountMF)
##BIGRAMS
library(quanteda)  
options(width = 110)
my_corpus <- corpus(metteF,text_field = "text")
summary(my_corpus, 5)
toks <- tokens(my_corpus, remove_punct = TRUE)
toks_ngram <- tokens_ngrams(toks, n = 2:4)
head(toks_ngram[[1]], 30) 
tail(toks_ngram[[1]], 30)
toks_neg_bigram <- tokens_compound(toks, pattern = phrase("ikke *"))
toks_neg_bigram_select <- tokens_select(toks_neg_bigram, pattern = phrase("ikke_*"))
tail(toks_neg_bigram_select[[1]], 30) 

  
##############

#UTIL
# find liste over danske statsministre
dktag=".ct-section-inner-wrap"
dkPage=read_html("https://leksikongen.dk/statsminister-i-danmark/")
stuf=dkPage %>% html_elements(dktag) 
dkStatsMins <- stuf[[5]] %>% html_elements(itag) %>% html_text() %>% as.data.frame()
colnames(dkStatsMins)="text"
dkSM=dkStatsMins
dkStatsMins <- dkStatsMins %>% mutate(year=str_extract(text,"[0-9]{4}"))
dkStatsMins <- dkStatsMins %>% mutate(minister=str_remove_all(text,"[0-9]"))
dkStatsMins <- dkStatsMins %>% mutate(parti=str_extract(minister,"\\(.*\\)"))
dkStatsMins <- dkStatsMins %>% mutate(minister=str_remove_all(minister,"\\(.*\\)"))
dkStatsMins <- dkStatsMins %>% mutate(parti=str_remove_all(parti,"[\\(\\)]"))
dkStatsMins <- dkStatsMins %>% mutate(minister=str_remove_all(minister,"\xe2\x80\x91|\x2d"))
dkStatsMins <- dkStatsMins %>% mutate(lastname=str_extract(minister," [:alpha:]+\\s?$"))

# filtrer talerlisten ud fra listen over statsministre
oklist=paste(dkStatsMins$lastname,collapse = "|")
spMet2f <- spMet2 %>% filter(grepl(oklist,lastname)) %>% arrange(year)

getSpeech <- function(x) {
  Sys.sleep(1)
  speech <- tryCatch({
    page=rvest::read_html(x$url)
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
