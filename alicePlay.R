library(gutenbergr)
library(dplyr)
library(stringr)
library(igraph)
library(tidytext)
library(spacyr)


metag=gutenberg_metadata

bookm=metag %>% filter(str_detect(title,"Adventures in Wonderland"))
text=gutenberg_download(28885)

alice= text %>% mutate(chap=cumsum(str_detect(text,regex("^chapter [\\divxlc]",
                       ignore_case = T)))) %>% select(-gutenberg_id) %>% filter(chap != 0) %>% 
  mutate(chap = as.factor(chap),text=str_remove(text,"_"))

#alice = alice %>% mutate(text=str_replace(text,"al[\\s]+ice","alice"))
abig=alice %>% unnest_tokens(bigram, text,token="ngrams",n=2)

unnest_tokens(output = sentence, input = txt, token = "sentences")
abig2=abig %>% filter(!is.na(bigram))
abig2 %>% count(bigram) %>% arrange(desc(n))

# test sentences
asent=alice %>% unnest_tokens(asent,text,token="sentences")
(asent[18,2])

# load spacy






