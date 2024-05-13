library(gutenbergr)

metag=gutenberg_metadata
# find gutenberg-books

hca=metag %>% filter(str_detect(author,"Hans Christ") & language=="en")
hcaid=hca$gutenberg_id
hcaBooks=gutenberg_download(hcaid)

hcaColl=hcaBooks %>% filter(gutenberg_id==1597)
# find individual stories
hcaColl <-hcaColl %>% mutate(linenr=row_number(),
                   story=str_detect(text,regex("^[A-Z\\s]+$"))) 

hcabigrams = hcaColl %>% unnest_tokens(bigram,text,token = "ngrams",n=2) %>% filter(!is.na(bigram))
hcabigrams = separate(hcabigrams,col=bigram,into = c("w1","w2"), sep = " ")

hcabigramsM <- hcabigrams %>% filter(w1 %in% c("He","he")) %>% filter(!w2 %in% stop_words$word)
hcabigramsF <- hcabigrams %>% filter(w1 %in% c("she","She")) %>% filter(!w2 %in% stop_words$word)


hcabigramsUM <- unite(hcabigramsM,c("w1","w2"), col=bigram, sep = " ",remove = F)
hcabigramsUF <- unite(hcabigramsF,c("w1","w2"), col=bigram, sep = " ",remove = F)
hcabigramsUM <- hcabigramsUM %>% group_by(w2) %>% mutate(mc=n()) %>% ungroup()
hcabigramsUF <- hcabigramsUF %>% group_by(w2) %>% mutate(fc=n()) %>% ungroup()
# now join
hcajoin <- inner_join(hcabigramsUF,hcabigramsUM, by="w2")

