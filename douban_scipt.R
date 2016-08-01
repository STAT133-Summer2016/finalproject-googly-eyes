# ---
#   title: "Douban Data"
# author: "GuanghongFu"
# date: "July 30, 2016"
# output: html_document
# ---
#   

knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(stringr)
library(xml2)
library(rvest)
library(readr)
library(tidyr)
library(plyr)
library(XML)
```

## R Markdown
```{r}
start = 0
end = 980
page_num <- seq(from = start, to = end, by = 20)

df = data.frame(matrix(vector(), 0, 1,
                       dimnames=list(c(), c("link"))),
                stringsAsFactors=F)

for (i in page_num) {
  html_address <- str_c("https://movie.douban.com/tag/2015%20%E7%BE%8E%E5%9B%BD?start=", i, "&type=", sep = "")
  print(str_c(as.character(round((i - start)/(end - start)*100, 2)), "%"))
  
  page = html_address %>%
    read_html()
  
  name = page %>% 
    xml_find_all(xpath = "//div[@class = 'pl2']//a") %>%
    html_attr("href")
  
  for (j in name) {
    movie = j %>% read_html()
    link = movie %>% 
      xml_find_all(xpath = "//div[@class = 'subject clearfix']//div//a") %>%
      html_attr("href")
    
    index = str_detect(link, "[i][m][d][b]")
    link <- link[index]
    
    if (length(link) > 0) {
      df <- rbind(df, data.frame(link))
    }
  }
}
