library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
library(tm)

tidy_books <- austen_books() %>%
  group_by(book) %>%  ungroup()

# split the bigram in twos for stopwords
jane_ss=tidy_books %>% filter(book=="Sense & Sensibility")
jane_ss=tidy_books

austen_bigrams = jane_ss %>%  unnest_tokens(bigram, text, token = "ngrams", n = 2)  %>% 
filter(!is.na(bigram))
austen_bigrams <- austen_bigrams %>%  separate(bigram,c("w1","w2"),sep=" ")

# HE SHE?
pnlist=list("he","she","He","She")
austen_bigramsGender <- austen_bigrams %>% filter(w1 %in% pnlist)
austen_bigramsGenderVerbs <- austen_bigramsGender %>% group_by(w1,w2) %>% count(w2, sort=T) %>% ungroup()
austen_bigramsGenderVerbsCl <- austen_bigramsGenderVerbs %>% filter(!w2 %in% stopwords(kind="en"))

# make gender specific dataframes
dfm=austen_bigramsGenderVerbs %>% filter(w1 %in% c("he","He"))
dff=austen_bigramsGenderVerbs %>% filter(w1 %in% c("she","She"))
commonw=inner_join(dfm,dff,by="w2")
str(commonw$n.y)
colnames(commonw)=c("w1m","w2","mct","w1f","fct","hebyher")
commonw$hebyher=log(commonw$mct)-log(commonw$fct)
commonw = commonw %>% mutate(total=mct+fct)

commonw %>% filter(total>20) %>% mutate(gender=ifelse(hebyher>0,"F","M")) %>% 
  ggplot(aes(x=reorder(w2,hebyher),y=hebyher, fill=gender))+geom_bar(stat="identity",position="dodge")+coord_flip()+
  labs(title="Common words by gender", x="weight according to gender", y="common word")

# network plot
austen_bigramsGenderVerbsPlot <- austen_bigramsGenderVerbs %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()
plot(austen_bigramsGenderVerbsPlot)
# network plot
austen_bigramsGenderVerbsPlot <- austen_bigramsGenderVerbsCl %>% 
  filter(n > 20) %>% 
  graph_from_data_frame()
plot(austen_bigramsGenderVerbsPlot)
