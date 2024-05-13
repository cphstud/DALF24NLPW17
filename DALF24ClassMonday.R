library(dplyr)
library(tm)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

testb=austen_bigrams$bigram

testspacy=spacy_parse(testb)
# prepare the stopwords - keep the pronouns for later
enstop=stopwords(kind="en")
pnlist=list("he","she","He","She")
enstop=enstop[!enstop %in% pnlist ]

# split the bigram in twos for stopwords
austen_bigrams <- austen_bigrams %>%  separate(bigram,c("w1","w2"),sep=" ")

# remove stopwords
enstop=c(enstop,"would","be","to","have","do","mrs","miss","mr","i","a")
austen_bigramsst <- austen_bigrams %>% filter(!w1 %in% enstop) %>% filter(!w2 %in% enstop)
austen_bigrams <- austen_bigrams %>% filter(!w1 %in% enstop) %>% filter(!w2 %in% enstop)

# remove all non-characters
austen_bigrams <- austen_bigrams %>% filter(str_detect(w1,"[a-zA-Z\\s-]")) %>% 
  filter(str_detect(w2,"[a-zA-Z\\s-]"))

# NOW TF
# re-create the bigram for counting the freq pr book 
austen_bigrams <- austen_bigrams %>% unite(col = bigram,c("w1","w2"), sep = " ", remove = F)
austen_bigramsc <- austen_bigrams %>% count(book,bigram, sort = T)
austen_bigramsc <- austen_bigramsc %>% group_by(book) %>% mutate(tot=sum(n)) %>% ungroup()
austen_bigramsc <- austen_bigramsc %>% group_by(book, bigram) %>% mutate(tf=n/tot) %>% ungroup()

# NOW IDF - i hvor mange docs forekommer de resepktive bigrams?
# not possible manually
allbooksdf = split(austen_bigramsc,austen_bigramsc$book)


# plot tf
austen_bigramsc %>%  filter(n>40) %>% 
  #ggplot(aes(x=bigram, y=n)) + geom_bar(stat="identity")+facet_wrap(~book)+coord_flip()
  ggplot(aes(x=bigram, y=n, fill=as.factor(book))) + geom_bar(stat="identity") + facet_wrap(~book)+coord_flip()

# HE SHE?
pnlist=list("he","she","He","She")
austen_bigramsGender <- austen_bigrams %>% filter(w1 %in% pnlist)
austen_bigramsGenderVerbs <- austen_bigramsGender %>% group_by(w1,w2) %>% count(w2, sort=T) %>% ungroup()
dfm=austen_bigramsGenderVerbs %>% filter(w1 %in% c("he","He"))
dff=austen_bigramsGenderVerbs %>% filter(w1 %in% c("she","She"))
commonw=inner_join(dfm,dff,by="w2")
commonw=commonw %>% mutate(hebyher=(n.x/n.y)-1)
commonw %>% filter(n.x>20) %>% mutate(gender=ifelse(hebyher>0,"F","M")) %>% 
  ggplot(aes(x=reorder(w2,hebyher),y=hebyher, fill=gender))+geom_bar(stat="identity",position="dodge")+coord_flip()+
  labs(title="Common words by gender", x="weight according to gender", y="common word")

#SPACY
#build a dictionary of JA-verbs to filter w2 down to verbs
library(spacyr)
spacy_initialize(model = "en_core_web_sm")
condapath="/usr/local/anaconda3"
spacy_initialize(model= "en_core_web_sm", condaenv = "base")

dftest=spacy_parse("Ron kissed Herimoni yesterday at Hogwarths")

# Get the total text of all JA books
austen_textSS <- austen_books() %>%
  group_by(book) %>% 
  filter(book=="Sense & Sensibility") %>% ungroup()
austen_textSS = austen_textSS %>% mutate(doc_id=row_number()) %>% select(-book)
JASS=spacy_parse(austen_textSS)
JASS_verbs=JASS %>% filter(pos=="ADJ") %>% select(token) %>% unique() %>% rename('w2'='token')

commonwVerbs=inner_join(commonw,JASS_verbs,by="w2")
  
# TRY TO EXTRACT relational information
austeks <- austen_books() %>% group_by(book) %>% filter(str_detect(book,"Sense")) %>% ungroup()
sense_sentences <- austeks %>% 
  unnest_tokens(sentence, text, token = "sentences")
sense_sentences['doc_id']=1:nrow(sense_sentences)
sense_sentences <- sense_sentences %>% rename('text'='sentence')
ausPOSparse <- spacy_parse(sense_sentences, pos = TRUE, entity = TRUE, dependency = TRUE)
ausPOSEx = ausPOS %>% filter(str_detect(entity,"PERSON") |  pos=="VERB")
ausPOSExName = ausPOS %>% filter(str_detect(entity,"PERSON"))

dfnames=as.data.frame(table(ausPOSExName$token))


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
