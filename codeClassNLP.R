library(gutenbergr)

metagut = gutenberg_metadata

metaidhca=metagut %>% filter(str_detect(author,"Hans Christian") & 
                               language=="en")
metaidvf=metagut %>% filter(str_detect(author,"Woolf, Virginia") & 
                               language=="en")

hcatext=gutenberg_download(metaidhca$gutenberg_id)

hcatextOneEx=hcatext %>% filter(gutenberg_id==1597)

hcatextOneEx = hcatextOneEx %>% mutate(ftale=str_detect(text,"^[A-Z'\\s-]+$"))

# bigrams

hcabigram=hcatextOneEx %>% unnest_tokens(bigram,text,token="ngrams",n=2)

# data cleaning
hcabigram=hcabigram %>% filter(!is.na(bigram))

# data feature engineering
# w1 og w2 from bigram
hcabigram=hcabigram %>% separate(bigram,c("w1","w2"), sep=" ")
hcabigramSubM=hcabigram %>% filter(w1 %in% c("he"))
hcabigramSubF=hcabigram %>% filter(w1 %in% c("she"))

#count af bigrams
hcabigramSubF=hcabigramSubF %>% count(w1,w2, sort=T)
hcabigramSubM=hcabigramSubM %>% count(w1,w2, sort=T)

# EDA
hist(hcabigramSubF$n)

# transformation
combinedhca=inner_join(hcabigramSubF,hcabigramSubM,by="w2")

# FE - genderratio, gender
combinedhca = combinedhca %>% mutate(mfratio=n.x/n.y,gender=ifelse(mfratio>1,"F","M"))
combinedhca = combinedhca %>% mutate(total=n.x+n.y)

combinedhca %>% filter(total > 10) %>% 
  ggplot(aes(x=reorder(w2,mfratio),y=mfratio, fill=gender))+geom_bar(stat="identity")+
  coord_flip()


