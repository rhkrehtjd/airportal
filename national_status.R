library(RSelenium)
library(rvest)
library(dplyr)

path = "/Users/gwagdoseong/Documents/TA/geckodriver.exe"
driver <- rsDriver(browser = "firefox", extraCapabilities = list(firefoxOptions = list(binary = path)))
remote_driver <- driver[["client"]]

# 함수 정의
get_national <- function(date) {

# Network의 Request URL 이용
url <- paste0("https://www.airportal.go.kr/servlet/aips.life.airinfo.RbJunCTL?gubun=c_getList&current_dt_from=", date)

remote_driver$navigate(url)
page <- read_html(remote_driver$getPageSource()[[1]])

table <- tables[2]
tr <- table %>%html_node("tbody") %>% html_nodes("tr")
td_list <- lapply(tr, function(tr_node) {tr_node %>% html_nodes("td") %>% html_text(trim = TRUE)})

# 홀수번째 원소만
td_list <- td_list[seq(1, length(td_list), by = 2)]
# 필요없는 값은 제외
td_list <- lapply(td_list, function(x) x[x != ""])

df <- as.data.frame(do.call(rbind, td_list))
colnames(df) <- c("공항", "계획D", "운항D", "결항D", "기타D", "잔여D","계획A", "운항A",
                  "결항A","기타A","잔여A","계획SUM","운항SUM","결항SUM","기타SUM","잔여SUM")

csv_file_name <- paste0(date, "_national_status", ".csv")
write.csv(df, file = csv_file_name, row.names = FALSE)
return(df)}

date <- "20230909"
df1 = get_national(date)

driver$server$stop()
