link <- "http://www.nfl.com/draft/history/fulldraft?type=position"
stran <- html_session(link) %>% read_html()


tabele_html <- stran %>% html_nodes(xpath="//table[@class='data-table1']")
tabele <- lapply(tabele_html[2:40], html_table)
leta <- sapply(tabele, . %>% .[1,1]) %>% strapplyc("([0-9]+)") %>% unlist() %>% parse_integer()
draft_ekipe <- lapply(1:length(tabele), . %>% { mutate(tabele[[.]][-c(1, 2), ], leto = leta[.]) }) %>% bind_rows()

uvozi.statistiko <- function()
  statistika <- read_csv("podatki/pro-football-reference2.csv", na = ":", locale = locale(encoding="UTF-8"))


