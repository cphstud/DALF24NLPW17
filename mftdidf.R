library(Sentida)
library(tm)
library(spacyr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(ggraph)
library(igraph)

# load speeches
allSP=readRDS("allspeeches.rds")

# get speaker and year
allSP = allSP %>% mutate(speaker=str_extract(name, "(?<=/)[^/]+$"))
allSP = allSP %>% mutate(year=str_extract(speaker, "[0-9]+$"))
allSP = allSP %>% mutate(taler=str_extract(speaker, "([a-zøæåA-ZÆØÅ-]+)(?=-1)")) 
allSP = allSP %>% mutate(taler2=str_replace(taler,"s$",""))
allSP['index'] = 1:nrow(allSP)
str(allSP)

# zipf's lov på MetteF
mftalerord = allSP %>% filter(str_detect(taler,"mette")) %>% 
  unnest_tokens(input=value, output = mfword,token = "words") %>% 
    count(index,mfword, sort=T) %>% 
    ungroup()
jstalerord = allSP %>% filter(str_detect(taler,"johanne")) %>% 
  unnest_tokens(input=value, output = mfword,token = "words") %>% 
    count(index,mfword, sort=T) %>% 
    ungroup()
  
mftotal = mftalerord %>% 
  group_by(index) %>% summarise(total=sum(n))
jstotal = jstalerord %>% 
  group_by(index) %>% summarise(total=sum(n))

dk=stopwords("da")
mftalerord = left_join(mftalerord,mftotal, by="index") 
jstalerord = left_join(jstalerord,jstotal, by="index") 
mftalerord = mftalerord %>% mutate(tf=n/total)
jstalerord = jstalerord %>% mutate(tf=n/total)
mftalerord = mftalerord %>% group_by(index) %>% mutate(rank=row_number()) %>% ungroup()
jstalerord = jstalerord %>% group_by(index) %>% mutate(rank=row_number()) %>% ungroup()

mftaleprrank=mftalerord %>% group_by(index) %>% mutate(rank=row_number(),tf=n.x/n.y)

mftalerord %>% 
ggplot(aes(rank, tf, color=as.factor(index)))+geom_line()+scale_x_log10()+scale_y_log10()+
  labs(title = "Mette F Zip")+theme(legend.position="none")
jstalerord %>% 
ggplot(aes(rank, tf, color=as.factor(index)))+geom_line()+scale_x_log10()+scale_y_log10()


# plot words
dk=c(dk,"vores","så","kan","ved","år","ny","første","mere")
mftalerordStop=mftalerord %>% filter(!mfword %in%dk)
summary(mftalerordStop$tf)
mftalerordStop %>% filter(tf>0.0050) %>% 
  ggplot(aes(x=mfword,y=tf))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~index)


  






  

