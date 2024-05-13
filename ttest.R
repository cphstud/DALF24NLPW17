library(rvest)
library(logr)
library(stringr)
library(RSelenium)
library(dplyr)
# docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "firefox")
remDr$open()
urla='https://dansketaler.dk/?s=Anders+Fog+Rasmussen&s_talekategori%5B%5D=251&s_talekategori%5B%5D=258'
urlb='https://dansketaler.dk/?s=Mette+Frederiksen&s_talekategori%5B%5D=251&s_talekategori%5B%5D=258'
urlc='https://dansketaler.dk/?s=Poul+Schlüter&s_talekategori%5B%5D=251&s_talekategori%5B%5D=258'
urld='https://dansketaler.dk/?s=Poul+Nyrup+Rasmussen&s_talekategori%5B%5D=251&s_talekategori%5B%5D=258'

page=read_html("/Users/thor/Downloads/Du søgte efter Anders Fog Rasmussen - Danske Taler.mhtml")

remDr$navigate(urla)
remDr$navigate(urlb)
remDr$navigate(urlc)
remDr$navigate(urld)
Sys.sleep(4)
page=remDr$getPageSource()
pageMF=remDr$getPageSource()
resMF=read_html(pageMF[[1]]) 
tttag=".speech__right__title"

links=resMF %>% html_elements(tttag) %>% html_nodes("a") %>% html_attr("href") %>% as.data.frame()
colnames(links)="link"
links2 = links %>% rowwise() %>% mutate(content=scraplinks(link))
links3 =links2 %>% filter(grepl("mette",link))
saveRDS(links3,"metteF.rds")


scraplinks <- function(nurl){
  ctag=".single-tale__content"
  content <- tryCatch({
  remDr$navigate(nurl)
  Sys.sleep(4)
  page=remDr$getPageSource()
  res=read_html(page[[1]]) 
  content <- res %>% html_nodes(ctag) %>% html_text()},
  error = function(e) {content="Not found"}
  )
  return(content)
}
