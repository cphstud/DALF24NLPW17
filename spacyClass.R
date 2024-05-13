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
# find en liste over danske statsministre
url="https://leksikongen.dk/statsminister-i-danmark"
dkSM = readRDS("fogigen.rds")

#find årstal
# A.W. Moltke (embedsmandsregering med konservative og nationalliberale)1848‑1851

dkSM <- dkSM %>% mutate(year=str_extract(text,"[0-9]{4}"))
dkSM <- dkSM %>% mutate(fullname=str_extract(text,"^[^(]*"))
dkSM <- dkSM %>% mutate(parti=str_extract(text,"\\(.*\\)"))
dkSM <- dkSM %>% mutate(lastname=str_extract(fullname,"[:alpha:]$"))

# indlæs Mette F's taler
#andersF <- getCleanedSpeech(rdsfil,docidoffset)
andersF <- getCleanedSpeech("fogigen.rds",(nrow(metteF)),"Anders Fog")
poulSchl <- getCleanedSpeech("poulscl.rds",(nrow(andersF)+nrow(metteF)),"Poul Schlüter")
#mettFCl <- getCleanedSpeech("metteF.rds",(nrow(andersF)+nrow(metteF)),"Mette Frederiksen")
poulnFCl <- getCleanedSpeech("poulnyrup.rds",(nrow(andersF)+nrow(metteF)),"Poul Nyrup")

totAFPSPN <- rbind(andersF,poulSchl,poulnFCl) %>% select(taler,year,score)

ggplot(totAFPSPN,aes(x=year,y=score,fill=taler))+geom_bar(stat="identity")+
  geom_line()+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90))
  

# plot senitment-score
# taler, score, year



#subset til spacy
dfMetSP <- metteFred %>% select(doc_id,text)
dfASP <- andersF %>% select(doc_id,text)
dfPoulSP <- poulSchl %>% select(doc_id,text)

#spacy i luften
spacy_finalize()
spacy_initialize(model = "da_core_news_md")

andersFParsed <- spacy_parse(dfASP,
            pos=T,
            tag=T,
            lemma=T,
            entity=T,
            dependency=T,
            nounphrase =T,
            multithread=T)
poulSFParsed <- spacy_parse(dfPoulSP,
            pos=T,
            tag=T,
            lemma=T,
            entity=T,
            dependency=T,
            nounphrase =T,
            multithread=T)


metteFParsed <- spacy_parse(dfMetSP,
            pos=T,
            tag=T,
            lemma=T,
            entity=T,
            dependency=T,
            nounphrase =T,
            multithread=T)

# filter on NOUNS and group by doc_id,word
andersFParsedNouns <- getTokenByPOS(andersFParsed,"NOUN") %>% mutate(taler="Anders Fog")
poulSParsedNouns <- getTokenByPOS(poulSFParsed,"NOUN") %>% mutate(taler="Poul Schluter")
metteSParsedNouns <- getTokenByPOS(metteFParsed,"NOUN") %>% mutate(taler="Mette Frederiksen")

# now plot dataframes
# prepare df for plot - get names
totsp=rbind(andersFParsedNouns,poulSParsedNouns)
totsp=rbind(metteSParsedNouns,totsp)

# how to find dem som 

plotdf=totsp %>% filter(n>75)
ggplot(plotdf,aes(x=lemma,y=n), color=taler)+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~taler)


# find alle de ord der går igen i alle taler
metteFParsedNounsYear = metteFParsedNounsYear %>% select(-c(size,link))
metteFParsedNounsAllYear = metteFParsedNounsYear %>% group_by(lemma) %>% filter(n_distinct(year)==7) %>% 
  select(year,lemma,n)

#metteFParsedNouns <- metteFParsed %>% 
#  filter(pos=="NOUN") %>% 
#  select(lemma) %>% 
#  count(lemma,sort = T)
# fix to create dodge-plot
metteFParsedNounsAllYear <- metteFParsedNounsAllYear %>% 
  mutate(year = as.factor(format(year, "%Y")))

metteFParsedNounsAllYear %>% 
  ggplot(aes(x=lemma,y=n,fill=year))+
  geom_bar(stat="identity",position = position_dodge(width=0.9),width = 0.7)+
  theme(axis.text.x = element_text( angle=90,hjust=1 ))+
  coord_flip()

# ENTITETER
metteFParsedClassEnt <- metteFParsed %>% filter(nchar(entity)>0) %>% select(token,doc_id)
mFPCEntCount <- metteFParsedClassEnt %>% count(token,sort = T) %>% filter(nchar(token)>1)
mFPCEntCount <- metteFParsedClassEnt %>% count(doc_id,token,sort = T) %>% filter(nchar(token)>1) %>% 
  mutate(doc_id=as.numeric(doc_id))
mFPCEntCountInfo <- inner_join(mFPCEntCount,metaMetteF,by="doc_id")

mFPCEntCountInfo %>% filter(n>3) %>% 
  ggplot(aes(x=token,y=n)) + geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~year)


### NOW ENTITIES
# filter on entities and group by doc_id,word
metteFParsedEntities <- metteFParsed %>% 
  filter(nchar(entity)>0) %>% 
  select(doc_id,lemma) %>% 
  mutate(doc_id=as.numeric(doc_id)) %>% 
  filter(nchar(lemma)>1)
  #count(doc_id,lemma,sort = T)
  #count(lemma,sort = T)

metteFParsedEntitiesAll <- metteFParsedEntities %>% count(lemma,sort=T)

metteFParsedEntitiesInfo=inner_join(metteFParsedEntities,metaMetteF,by="doc_id") %>% select(-c(size,link))
metteFParsedEntitiesInfoCount <- metteFParsedEntitiesInfo %>% count(year,lemma,sort=T)

metteFParsedEntitiesInfoCount %>% filter(n >0)  %>% 
  ggplot(aes(label=lemma,size=n))+
  scale_size_area(max_size = 5)+
  geom_text_wordcloud()+
  facet_wrap(~year)

#metteFParsedNounsYear %>% filter(n>10) %>% 
#  ggplot(aes(x=reorder(lemma,n),n))+
#  geom_bar(stat="identity")+
#  theme(axis.text.x = element_text(
#    angle=90,hjust=1 ))+
#  coord_flip()
library(ggwordcloud)
metteFParsedNouns %>% filter(n>10) %>% 
  ggplot(aes(label=lemma,size=n))+
  geom_text_wordcloud()


metteFred %>% select(-content) %>% 
  ggplot(aes(x=year,y=size))+geom_bar(stat="identity")


# lav en fin dataframe



getCleanedSpeech <- function(f,o,n) {
  spretval = readRDS(f)
  spretval <- spretval %>% mutate(year=str_extract(link,"[0-9]{4}"))
  spretval <- spretval %>% mutate(size=nchar(content))
  spretval <- spretval %>% mutate(taler=n)
  spretval <- spretval %>% mutate(score=sentida(content,output = "mean"))
  spretval <- spretval %>% mutate(year=as.Date(paste0(year,"-01-01")))
  spretval <- spretval %>% rename(text=content)
  
  for (i in (1:nrow(spretval))) {
    spretval[i,"doc_id"]=i+o
  }
  return(spretval)
}

getTokenByPOS <- function(dfSP,postag) {
  retValPos <- dfSP %>% 
    filter(pos==postag) %>% 
    select(doc_id,lemma) %>% 
    mutate(doc_id=as.numeric(doc_id)) %>% 
    #count(doc_id,lemma,sort = T) 
    count(lemma,sort = T) 
  return(retValPos)
}

# now get metainfo from metaMette