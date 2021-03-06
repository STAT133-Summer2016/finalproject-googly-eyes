library(rvest)
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
FetchAYear = function(year){
  col_names = c("name",
                "ratingValue",
                # contentRating,
                "date",
                "director",
                "stars",
                "metaScore",
                "review_users",
                "review_critcs",
                "popularity",
                "keywords",
                "genres",
                "contentRating",
                "country",
                "language",
                "budget",
                "opening_week",
                "gross",
                "runtime",
                "color",
                "asp")
  
  links = read_csv(str_c("./douban/douban", year, ".csv", sep=""))
  start = 1
  i = start
  end = nrow(links)
  links = links[start:end,]
  
  
  movies_matrix = matrix(nrow = 0, ncol = 20)
  for (link in links$link){
    print(str_c(year, " ", as.character(i), " ", as.character(round((i-start) / (end - start) * 100, 2)), "% ", link, sep = ""))
    i = i + 1
    page = link %>% read_html()
    head_attrs = page %>% 
      xml_find_all("//div[@class='credit_summary_item']/h4") %>% 
      xml_text()
    if ("Note:" %in% head_attrs){
      print("NOOOOTTTTEEEEE")
      next
    }
    
    name = page %>% 
      xml_find_all(xpath = "//h1[@itemprop='name']") %>% 
      xml_text()
    
    ratingValue = page %>% 
      xml_find_all(xpath = "//div[@class = 'ratingValue']/strong") %>% 
      xml_attr(attr = "title")
    if (length(ratingValue)==0){
      ratingValue = NA
    }
    
    dateIndex = page %>% 
      xml_find_all(xpath = "//div[@class = 'subtext']/a") %>% 
      xml_attr(attr = "href") %>% 
      str_detect("releaseinfo")
    
    date = page %>% 
      xml_find_all(xpath = "//div[@class = 'subtext']/a") %>% 
      .[dateIndex] %>% 
      xml_text() %>% 
      str_replace_all("\\([a-zA-Z]*\\)", "")
    if (length(date) == 0){
      date = NA
    }else{
      date_str = date
      date = dmy(date_str)
      if (is.na(date) | str_detect(date_str, "Episode") | str_detect(date_str, "aired") | str_detect(date_str, "TV")){
        print(date_str)
        next
      }
    }
    date = as.character(date)
    
    plot_summary_attrs = page %>% 
      xml_find_all(xpath = "//div[@class = 'plot_summary ']
                   /div[@class='credit_summary_item']/h4") %>%  
      xml_text()
    
    if ("Directors:" %in% plot_summary_attrs | "Director:" %in% plot_summary_attrs){
      director = page %>% 
        xml_find_all(xpath = "//span[@itemprop = 'director']/a/span") %>% 
        xml_text() %>% 
        str_c("", collapse = ",")
    }else{
      director = NA
    }
    
    if ("Stars:" %in% plot_summary_attrs){
      stars = page %>% 
        xml_find_all(xpath = "//span[@itemprop = 'actors']/a/span") %>% 
        xml_text() %>% 
        str_c(collapse = ",")
    }else{
      stars = NA
    }
    
    metaScore = page %>% 
      xml_find_all(xpath = 
                     "//a[@href='criticreviews?ref_=tt_ov_rt']/div/span") %>% 
      xml_text() %>% 
      as.numeric() %>% 
      as.character()
    if (length(metaScore) == 0){
      metaScore = NA
    }
    
    review_users = page %>% 
      xml_find_all(xpath = "//a[@href='reviews?ref_=tt_ov_rt']") %>% 
      xml_text()
    if (length(review_users) == 0){
      review_users = NA
    }
    
    review_critics = page %>% 
      xml_find_all(xpath = "//a[@href='externalreviews?ref_=tt_ov_rt']") %>% 
      xml_text()
    if (length(review_critics) == 0){
      review_critics = NA
    }
    mPopularity = page %>% 
      xml_find_all("//div[@class = 'titleReviewBarSubItem']/div/span[@class = 'subText']")
    
    if (length(mPopularity) > 1){
      popularity = mPopularity %>%
        .[2] %>% 
        xml_text() %>% 
        str_match("[0-9]+") %>% 
        as.numeric() %>% 
        as.character()
    }else{
      popularity = NA
    }
    
    
    classify_attrs = page %>% 
      xml_find_all("//div[@class = 'see-more inline canwrap']") %>% 
      xml_attr("itemprop")
    
    if ("keywords" %in% classify_attrs){
      keywords = page %>% 
        xml_find_all("//div[@class = 'see-more inline canwrap']/a") %>% 
        xml_attr("href") 
      keywords = keywords[str_detect(keywords, "keyword")] %>% 
        str_replace("/keyword/", "") %>% 
        str_replace("\\?ref_=tt_stry_kw", "") %>% 
        str_c("", collapse = ",")
    }else{
      keywords = NA
    }
    
    if ("genre" %in% classify_attrs){
      genres = page %>% 
        xml_find_all("//div[@class = 'see-more inline canwrap']/a") %>% 
        xml_attr("href") 
      genres = genres[str_detect(genres, "genre")] %>% 
        str_replace("/genre/", "") %>% 
        str_replace("\\?ref_=tt_stry_gnr", "") %>% 
        str_c("", collapse = ",")
    }else{
      genres = NA
    }
    
    attrs = page %>% 
      xml_find_all("//div[@class='txt-block']/h4") %>% 
      xml_text()
    
    if ("Motion Picture Rating\n                    (MPAA)\n                " %in% attrs){
      contentRating = page %>% 
        xml_find_all("//div[@class='txt-block']/span[@itemprop = 'contentRating']") %>% 
        xml_text()
    }else{
      contentRating = NA
    }
    
    if ("Country:" %in% attrs){
      country = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Country:']/following-sibling::a") %>%
        xml_text() %>% 
        str_c(collapse = ",")
    }else{
      country = NA
    }
    
    if ("Language:" %in% attrs){
      language = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Language:']/following-sibling::a") %>% 
        xml_text()  %>% 
        str_c("", collapse = ",")
    }else{
      language = NA
    }
    
    if ("Release Date:" %in% attrs){
      release_date = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Release Date:']/..") %>% 
        xml_contents() %>% 
        .[3] %>% 
        xml_text()
    }else{
      release_date = NA
    }
    
    if ("Budget:" %in% attrs){
      budget = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Budget:']/..") %>% 
        xml_contents() %>% 
        .[3] %>% 
        xml_text() %>% 
        str_replace_all("[ ]+", "") %>% 
        str_replace_all("\n", "") %>%
        str_replace_all(",", "") %>% 
        str_replace("\\$","")
    }else{
      budget = NA
    }
    
    if ("Opening Weekend:" %in% attrs){
      opening_week = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Opening Weekend:']/..") %>% 
        xml_contents() %>% 
        .[3] %>% 
        xml_text() %>% 
        str_replace_all("[ ]+", "") %>% 
        str_replace_all("\n", "") %>%
        str_replace_all(",", "") %>% 
        str_replace("\\$","")
    }else{
      opening_week = NA
    }
    
    if ("Gross:" %in% attrs){
      gross = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Gross:']/..") %>% 
        xml_contents() %>% 
        .[3] %>% 
        xml_text() %>% 
        str_replace_all("[ ]+", "") %>% 
        str_replace_all("\n", "") %>%
        str_replace_all(",", "") %>% 
        str_replace("\\$","")
    }else{
      gross = NA
    }
    
    if ("Runtime:" %in% attrs){
      runtime = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Runtime:']/..") %>% 
        xml_contents() %>% 
        .[4] %>% 
        xml_text() %>% 
        str_replace_all(" min", "")
    }else{
      runtime = NA
    }
    
    if ("Color:" %in% attrs){
      color = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Color:']/..") %>% 
        xml_contents() %>% 
        .[4] %>% 
        xml_text()
    }else{
      color = NA
    }
    
    if ("Aspect Ratio:" %in% attrs){
      asp = page %>% 
        xml_find_all("//div[@class='txt-block']/h4[text()='Aspect Ratio:']/..") %>% 
        xml_contents() %>% 
        .[3] %>% 
        xml_text() %>% 
        str_replace_all(" ", "") %>% 
        str_replace_all("\n", "")
    }else{
      asp = NA
    }
    
    new_movie = c(name,
                  ratingValue,
                  # contentRating,
                  date,
                  director,
                  stars,
                  metaScore,
                  review_users,
                  review_critics,
                  popularity,
                  keywords,
                  genres,
                  contentRating,
                  country,
                  language,
                  budget,
                  opening_week,
                  gross,
                  runtime,
                  color,
                  asp) %>% 
      matrix(nrow = 1, ncol = 20)
    movies_matrix = rbind(movies_matrix, new_movie)
  }
  
  movies_df = movies_matrix %>% 
    data.frame()
  colnames(movies_df) = col_names
  write_csv(movies_df, str_c("movies_imdb_", year, ".csv", sep=""))
  
}

years = c(2000:2015)
for (year in years){
  FetchAYear(as.character(year))
}