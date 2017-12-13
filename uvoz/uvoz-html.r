link <- "http://www.nfl.com/draft/history/fulldraft?type=position"
stran <- html_session(link) %>% read_html()

# HTML
tabele_html <- stran %>% html_nodes(xpath="//table[@class='data-table1']")
tabele <- lapply(tabele_html[2:40], html_table)
leta <- sapply(tabele, . %>% .[1,1]) %>% strapplyc("([0-9]+)") %>% unlist() %>% parse_integer()
draft_ekipe <- lapply(1:length(tabele), . %>% { mutate(tabele[[.]][-c(1, 2), ], Leto = leta[.]) }) %>% bind_rows()

colnames(draft_ekipe) <- c("Krog", "Izbor", "Ime", "Pozicija", "College", "Ekipa", "Leto")

draft_ekipe$Krog <- parse_integer(draft_ekipe$Krog)
draft_ekipe$Izbor <- parse_integer(draft_ekipe$Izbor)
draft_ekipe$Pozicija <- NULL
draft_ekipe <- draft_ekipe[,-c(1:2,6)]


write_csv(draft_ekipe, "podatki/draft-ekipe.csv")

# CSV
uvozi.statistiko <- function()
  statistika <- read_csv("podatki/pro-football-reference.csv",
                         na = ":", locale = locale(encoding="UTF-8"))[,c(2:5, 11:12, 14:15, 18:25)]

colnames(statistika) <- c("Leto", "Krog", "Izbor", "Ime", "All-Pro", "Pro Bowl",
                          "Povprecje", "St_tekem", "Uspesne_podaje", "Podaje",
                          "Jardi podaje", "Podaje za TD", "Prestrezene podaje",
                          "St_tekov","Jardi_tek", "TD_tek")

write_csv(statistika, "podatki/statistika.csv")

# 
skupna <- merge(statistika, draft_ekipe, by = "Ime")



