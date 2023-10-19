library(RSelenium);library(rvest)
library(dplyr);library(googledrive);library(httpuv)


setwd("/Users/gwagdoseong/Documents/TA")
path = "/Users/gwagdoseong/Documents/TA/geckodriver.exe"
driver <- rsDriver(browser = "firefox", extraCapabilities = list(firefoxOptions = list(binary = path)))
remDr <- driver[["client"]]

code_url <-
  'https://www.airportal.go.kr/knowledge/airports/KbAirport01.jsp?PAGENO=1&PAGEROWS=7666&START=&keyword1=&keyword2=&gubun=&sortvalue=name&order=1&target=&search='

remDr$navigate(code_url)
page_source <- remDr$getPageSource()[[1]]

page_html <- page_source %>% read_html()

trs <- page_html %>%
  html_node('body') %>%
  html_node('table') %>%
  html_nodes('tbody') %>%
  html_nodes('tr')

trs <- trs[39:7704]

code_col <- c('번호', '공항명', 'IATA', 'ICAO', '국가명', '도시명')

all_elements <- trs %>% html_nodes('td') %>% html_text()
all_elements2 <- #두 번째 원소마다 빈칸임
  all_elements[c(T, F, T, T, T, T, T)]

airport_code <-
  data.frame(matrix(all_elements2, ncol = 6 , byrow = T))# %>% data_frame()
colnames(airport_code) <- code_col

airport_code <- airport_code %>% tibble()

kor_airport <- airport_code %>% 
  filter(국가명 == 'Republic of Korea') %>% 
  unique()#공항, 코드만 보고싶다.


kor_airport #한국의 공항 코드들

kr_airport <- kor_airport[-c(9,13),] #제주 정석비행장, 서울공항은 기밀시설이라 정보 x

kor_code <- kr_airport$ICAO
airport_name <- kr_airport$공항명
icaos <- kor_code







real_time <- function(Arr_Dep, date, icaos){
  all_dataframes <- list()
  
  for(icao in icaos){
    
    base_url <- "https://www.airportal.go.kr/life/airinfo/RbHanList.jsp?gubun=c_getList&"
    
    url = paste0(base_url,'depArr=',Arr_Dep,'&current_date=',date,'&airport=',icao,'&fp_id=')
    
    remDr$navigate(url)
    page_source <- remDr$getPageSource()[[1]]
    page <- read_html(page_source)
    
    
    if(Arr_Dep == "D"){
      tr <- page %>% html_node('body') %>%
        html_node('form') %>%
        html_node('table') %>%
        html_node('tbody') %>%
        html_node('td') %>% 
        html_node('tbody')
      
      
      td <- tr %>% html_nodes('td') %>% html_text(trim = T)
      td <- td[-length(td)]
      td <- td[c(T,F)]
      
      
      
      df_col <- c('항공사','편명','목적지','계획','예상','출발','구분','현황')
      
      real_time_df <- matrix(td, ncol = 8, byrow = T) %>%data.frame()
      colnames(real_time_df) <- df_col
      
      nrows <- nrow(real_time_df)
      
      
      real_time_df$날짜 = rep(date, nrows) 
      real_time_df$공항 = rep(kr_airport$공항명[kr_airport$ICAO == icao],nrows)
      
      all_dataframes[[icao]] <- real_time_df
      
    }else{
      trs <- page %>% html_node('body') %>%
        html_node('form') %>%
        html_node('table') %>%
        html_node('tbody') %>%
        html_node('tr') %>% 
        html_node('td') %>%
        html_node('table') %>%
        html_node('tbody') %>%
        html_nodes('tr')
      
      #벡터 자체에서 마지막 원소를 빼버리고 싶긴 한데..
      tds <- trs %>% html_nodes('td') %>% html_text(trim= T)
      tds <- tds[-length(tds)]
      tds <- tds[c(T,F)]
      
      df_col2 <- c('항공사','편명','출발지','계획','예상','도착','구분','현황')
      
      real_time_df <- matrix(tds, ncol = 8, byrow = T) %>%
        data.frame()
      colnames(real_time_df) <- df_col2
      
      nrows <- nrow(real_time_df)
      
      real_time_df$날짜 = rep(date, nrows) 
      real_time_df$공항 = rep(kr_airport$공항명[kr_airport$ICAO == icao],nrows)
      
      
      all_dataframes[[icao]] <- real_time_df
    }
  }
  combined_df <- bind_rows(all_dataframes, .id = "icao")


  
 #combined_df <- combined_df[-which(combined_df$편명=="검색된 결과가 없습니다"),]
  

  
  
  
  return(combined_df)
}

date_range <- seq(as.Date("2007-01-01"), as.Date("2023-10-17"), by = "days")
date_range <- format(date_range, format="%Y%m%d")

for (date in date_range) {

  Arr_Dep = 'D'
  result_ = real_time(Arr_Dep , date , icaos )
  csv_file_name <- paste0(date, "_", Arr_Dep, ".csv")
  write.csv(result_, file = csv_file_name, row.names = FALSE)
}

driver$server$stop()


