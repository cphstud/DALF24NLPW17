library(dplyr)
library(tm)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# filter
enstop=stopwords(kind="en")
pnlist=list("he","she","He","She")

enstop=enstop[!enstop %in% pnlist ]
austen_bigrams <- austen_bigrams %>%  separate(bigram,c("w1","w2"),sep=" ")
head(austen_bigramsc)

austen_bigramsst <- austen_bigrams %>% filter(!w1 %in% enstop) %>% filter(!w2 %in% enstop)
# remove all non-characterso
austen_bigrams <- austen_bigrams %>% filter(str_detect(w1,"[a-zA-Z\\s-]")) %>% 
  filter(str_detect(w2,"[a-zA-Z\\s-]"))

# NOW TF
austen_bigrams <- austen_bigrams %>% unite(col = bigram,c("w1","w2"), sep = " ", remove = F)
austen_bigramsc <- austen_bigrams %>% count(book,bigram, sort = T)
austen_bigramsc <- austen_bigramsc %>% group_by(book) %>% mutate(tot=sum(n)) %>% ungroup()
head(austen_bigramsc)
austen_bigramsc <- austen_bigramsc %>% group_by(book, bigram) %>% mutate(tf=n/tot) %>% ungroup()

# NOW IDF - i hvor mange docs forekommer de resepktive bigrams?
bbi=list()
books=austen_bigrams %>% select(book) %>% unique()
allbooksdf = split(austen_bigramsc,austen_bigramsc$book)

df=allbooksdf$`Sense & Sensibility`
dfpp=inner_join(allbooksdf$`Sense & Sensibility`, allbooksdf$`Pride & Prejudice`, by="bigram")
dfmp=inner_join(allbooksdf$`Sense & Sensibility`, allbooksdf$`Mansfield Park`, by="bigram")
dfe=inner_join(allbooksdf$`Sense & Sensibility`, allbooksdf$Emma,by="bigram")
dfna=inner_join(allbooksdf$`Sense & Sensibility`, allbooksdf$`Northanger Abbey`, by="bigram")
dfp=inner_join(allbooksdf$`Sense & Sensibility`, allbooksdf$Persuasionby="bigram")
              
df=allbooksdf$`Sense & Sensibility`
df=allbooksdf$`Pride & Prejudice`

nl=1:6
nldf=as.data.frame(combn(nl,2))
for (i in nldf) {
  df = anti_join(nldf[[i]][1],nldf[[i]][2], by="bigram")
}

# plot tf
austen_bigramsc %>%  filter(n>50) %>% 
#ggplot(aes(x=bigram, y=n)) + geom_bar(stat="identity")+facet_wrap(~book)+coord_flip()
ggplot(aes(x=bigram, y=n, fill=as.factor(book))) + geom_bar(stat="identity", position = "dodge")+coord_flip()

# HE SHE?
pnlist=list("he","she","He","She")
austen_bigramsGender <- austen_bigrams %>% filter(w1 %in% pnlist)
austen_bigramsGenderVerbs <- austen_bigramsGender %>% group_by(w1,w2) %>% count(w2, sort=T) %>% ungroup()
dfm=austen_bigramsGenderVerbs %>% filter(w1 %in% c("he","He"))
dff=austen_bigramsGenderVerbs %>% filter(w1 %in% c("she","She"))
commonw=inner_join(dfm,dff,by="w2")
commonw=commonw %>% mutate(hebyher=(n.x/n.y)-1)
commonw %>% filter(n.x>40) %>% mutate(gender=ifelse(hebyher>0,"F","M")) %>% 
ggplot(aes(x=reorder(w2,hebyher),y=hebyher, fill=gender))+geom_bar(stat="identity",position="dodge")+coord_flip()+
  labs(title="Common words by gender", x="weight according to gender", y="common word")

#build a dictionary of JA-verbs
library(spacyr)
spacy_initialize(model = "en_core_web_sm")
condapath="/usr/local/anaconda3"
spacy_initialize(model= "en_core_web_sm", condaenv = "base")

#spacy_finalize()


austen_bigramsGenderVerbs %>% filter(n>30) %>% 
  ggplot(aes(x=w2,y=n, fill=as.factor(w1)))+geom_bar(stat="identity",position="dodge")+coord_flip()

# NOW GRAPH
library(ggraph)
jaforgr= austen_bigramsc %>%  separate(bigram,c("w1","w2"),sep=" ")
jaforgrcl <- jaforgr %>% filter(!w1 %in% enstop) %>% filter(!w2 %in% enstop)
edges <- jaforgrcl %>% filter(n>30) %>% 
  select(book,w1, w2, n) %>%
  rename(weight = n)
edges <- jaforgrcl %>% filter(n>30) %>% 
  select(book,w1, w2, n) %>%
  rename(weight = n)

g <- graph_from_data_frame(d = edges, directed = TRUE)
plot(g)
g
degree(g)
V(g)
ggraph(g,layout = "fr")+geom_edge_link()+geom_node_point()+geom_node_text(aes(label=name))

# graph on gender
cwgender=commonw %>% filter(n.x>40) %>% mutate(gender=ifelse(hebyher>0,"F","M")) 
cwgender=cwgender %>% mutate(cw="it")
cwedges <- cwgender %>% filter(n.x>10) %>% 
  mutate(weight=hebyher+10) %>% 
  select(cw,w2,gender,weight) %>% 
  mutate(gender=as.factor(gender))
cwg <- graph_from_data_frame(d = cwedges, directed = TRUE)
plot(cwg)
V(cwg)$color = "red"
V(cwg)$color <- ifelse(V(cwg)$gender == "F", "lightpink", "lightblue")
plot(cwg)

# TRY TO EXTRACT relational information
austeks <- austen_books() %>% group_by(book) %>% filter(str_detect(book,"Sense"))
austeks['doc_id']=1:nrow(austeks)
sense_sentences <- austeks %>% 
  unnest_tokens(sentence, text, token = "sentences")
sense_sentences['doc_id']=1:nrow(sense_sentences)
sense_sentences <- sense_sentences %>% ungroup()
sense_sentences <- sense_sentences %>% select(-book)
sense_sentences <- sense_sentences %>% rename('text'='sentence')


ausPOS=spacy_parse(austeks)
ausPOSparse <- spacy_parse(sense_sentences, pos = TRUE, entity = TRUE, dependency = TRUE)

ausPOSEx = ausPOS %>% filter(str_detect(entity,"PERSON") |  pos=="VERB")
ausPOSExP = ausPOSEx %>% filter(str_detect(entity,"PERSON")) %>% group_by(doc_id) %>%
  summarise(pc=n()) %>% ungroup()

ausPOSEx = left_join(ausPOSEx,ausPOSExP, by="doc_id")
ausPOSExSub=ausPOSEx %>% filter(pc>2)
ausPOSExSubPlot=ausPOSExSub %>% filter(pos=="PROPN") %>% group_by(doc_id) 
ausPOSExSubPlot = ausPOSExSubPlot %>% mutate(token=tolower(token))
ausPOSExSubPlot = ausPOSExSubPlot %>% filter(!token %in% c("mrs","miss","lady","mr")) %>% 
  filter(token != "c.")
ausPOSExSubPlotSub = ausPOSExSubPlot %>% select(doc_id,token) %>% unique()
wideJA= ausPOSExSubPlotSub %>% group_by(doc_id) %>% mutate(row=row_number()) %>% 
  pivot_wider(names_from = row,values_from = token,names_prefix = "doc_id_w")

wideJASlim=wideJA %>% rowwise() %>% mutate(w1=doc_id_w1, w2=
                               ifelse(nchar(doc_id_w4) > 1,doc_id_w4,
                                       ifelse(nchar(doc_id_w3)>1,doc_id_w3,
                                              ifelse(nchar(doc_id_w2 > 1,doc_id_w2,NA )))))

wideJASlim=wideJA %>% rowwise() %>% mutate(w1=doc_id_w1, 
                                           w2= if(!is.na(doc_id_w4)) {
                                             doc_id_w4 
                                               } else if(!is.na(doc_id_w3)) {
                                                 doc_id_w3
                                               } else  {
                                                 doc_id_w2 } )

wideJASlim = wideJASlim %>% filter(!is.na(w2)) %>% mutate(weight=1)
wideJASlim = wideJASlim %>% ungroup()
edges <- wideJASlim %>% 
  select(w1, w2,weight) 
g <- graph_from_data_frame(d = edges, directed = TRUE)
plot(g)
ggraph(g,layout = "fr")+geom_edge_link()+geom_node_point()+geom_node_text(aes(label=name))
