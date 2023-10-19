library(httr);library(rvest);library(tidyverse);library(writexl);library(jsonlite);library(XML)

url <-  'http://openapi.airport.co.kr/service/rest/FlightScheduleList/getDflightScheduleList?ServiceKey=SAVci1fQ4wq8bTcGe5hFLcehlvuSEbkldgeF%2F07ihyl%2BUzygz11il6nG%2FZ01xdH4wo5OjU2faZ%2B6zV0Kf2nFZA%3D%3D&'
url <-   paste0(url,'schDeptCityCode=','MWX',
  '&schArrvCityCode=',
  'ICN',
  '&pageNo=',
  1,
  collapse = ''
)

json <- GET(url = url) %>% content(as = 'text' , encoding = 'UTF-8') %>%fromJSON()

numOfRows <-json$response$body$numOfRows
totalCount <- json$response$body$totalCount
loopCount <- ceiling(totalCount / numOfRows)

df_ <- data.frame()
for (i in 1:loopCount){
  tempo <- json$response$body$items$item
   df_ <- rbind(df_, tempo)}

head(subset(df, arrivalcityCode == "ICN" & startcityCode == "MWX"),3)
head(df_,3)
