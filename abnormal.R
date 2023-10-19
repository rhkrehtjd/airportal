library(RSelenium);library(rvest);library(dplyr);library(writexl)
setwd("/Users/gwagdoseong/Documents/TA")
path = "/Users/gwagdoseong/Documents/TA/geckodriver.exe"
driver <- rsDriver(browser = "firefox", extraCapabilities = list(firefoxOptions = list(binary = path)))
remote_driver <- driver[["client"]]

# 공항 코드 & 한글명 전환을 위한 변수 지정
ap_str <- "인천: RKSI, 김포: RKSS, 청주: RKTU, 양양: RKNY, 군산: RKJK, 원주: RKNW, 김해: RKPK, 제주: RKPC, 대구: RKTN, 광주: RKJJ, 여수: RKJY, 울산: RKPU, 포항: RKPH,  사천: RKPS, 무안: RKJB"
ap_str <- gsub(" ", "", ap_str)
ap_list <- unlist(strsplit(ap_str, ","))
ap_list <- strsplit(ap_list, ":")
airport_kor <- sapply(ap_list, "[", 1)
airport_code <- sapply(ap_list, "[", 2)

# 함수 정의
get_airplane_abnormal <- function(dep_arr, date, airport) {
      remote_driver$navigate("https://www.airportal.go.kr/life/airinfo/RbHanFrmMain.jsp")
      
      # 출도착 정보에서 비정상운항현황 선택
      link <- remote_driver$findElements(using = "css selector", value = 'a[target="main"]')
      third_link = link[[3]]
      third_link$clickElement()
      
      # 옵션 선택위한 frame으로 진입
      remote_driver$switchToFrame(remote_driver$findElement("xpath", '//iframe[@name="main"]'))
      
      # 출도착 선택
      radio_input_xpath <- paste("//input[@type='radio' and @value='",dep_arr, "']", sep = "")
      radio_input <- remote_driver$findElement("xpath", radio_input_xpath)
      remote_driver$executeScript("arguments[0].click();", list(radio_input))
      
      # 날짜 수정
      input_element <- remote_driver$findElement("id", "current_date")
      input_element$clearElement()
      input_element$sendKeysToElement(list(date))
      
      # 공항 수정
      airport = airport_code[which(airport_kor == airport)]
      select_element <- remote_driver$findElement("name", "airport")
      remote_driver$executeScript( paste("arguments[0].value =", "'", airport, "';", sep = "")
                                   , list(select_element))
      
      # 서치 
      remote_driver$executeScript("go_search();")
      Sys.sleep(1)
      
      
      # 테이블 조회위한 frame진입
      remote_driver$switchToFrame(remote_driver$findElement("xpath", '//iframe[@name="sframe"]'))
      
      page_content <- remote_driver$getPageSource()
      page <- read_html(page_content[[1]])
      
      form_element <- page %>%html_node("form")
      table_element <- form_element %>%html_node("table")
      table_element2 <- table_element %>%html_node("table")
      tbody <- table_element2 %>%html_node("tbody")
      tr <- tbody %>%html_nodes("tr")
      
      td_list <- lapply(tr, function(tr_node) {tr_node %>% html_nodes("td") %>% html_text(trim = TRUE)})
      
      # 마지막 원소는 총 개수를 나타내는 원소로서 제외해도 무방
      td_list <- td_list[-length(td_list)]
      # 홀수번째 원소만. -> 짝수번째 원소는 테이블 구성을 위해 포함된 것으로 아무런 내용이 없음
      td_list <- td_list[seq(1, length(td_list), by = 2)]
      # 리스트의 각 원소에서 홀수번째 자리에만 내용이 들어가 있음
      td_list <- lapply(td_list, function(td_contents) {
        cleaned_td_contents <- td_contents[c(TRUE, FALSE)]  
        cleaned_td_contents <- cleaned_td_contents[cleaned_td_contents != ""]  
        return(cleaned_td_contents)})
      df <- as.data.frame(do.call(rbind, td_list))
      
      # 출도착에 따라 변수명 변경
      if (dep_arr == "A") {colnames(df) <- c("항공사", "편명", "출발지", "계획", "예상", "도착", "구분", "현황","비정상원인")
      } else colnames(df) <- c("항공사", "편명", "목적지", "계획", "예상", "출발", "구분", "현황","비정상원인")
      
      # 날짜와 공항 칼럼 생성
      df$날짜 <- rep(date, nrow(df))
      df$공항 <- rep(airport_kor[which(airport_code == airport)], nrow(df))
      
      csv_file_name <- paste0(date, "_", dep_arr,"_abnormal", ".csv")
      write.csv(df, file = csv_file_name, row.names = FALSE)
      
      return(df)}

dep_arr <- "D"
date <- "20230909"
airport <- "제주"

df1 <- get_airplane_abnormal(dep_arr, date, airport)

driver$server$stop()


