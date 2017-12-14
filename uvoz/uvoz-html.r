link <- "http://www.nfl.com/draft/history/fulldraft?type=position"
stran <- html_session(link) %>% read_html()

# HTML
tabele_html <- stran %>% html_nodes(xpath="//table[@class='data-table1']")
tabele <- lapply(tabele_html[2:40], html_table)
leta <- sapply(tabele, . %>% .[1,1]) %>% strapplyc("([0-9]+)") %>% unlist() %>% parse_integer()
draft.ekipe <- lapply(1:length(tabele), . %>% { mutate(tabele[[.]][-c(1, 2), ], Leto = leta[.]) }) %>% bind_rows()

colnames(draft.ekipe) <- c("Krog", "Izbor", "Ime", "Pozicija", "College", "Ekipa", "Leto")

draft.ekipe$Krog <- parse_integer(draft.ekipe$Krog)
draft.ekipe$Izbor <- parse_integer(draft.ekipe$Izbor)
draft.ekipe$Pozicija <- NULL
draft.ekipe <- draft.ekipe[,-c(1:2,6)]


write_csv(draft.ekipe, "podatki/draft-ekipe.csv")

# CSV
uvozi.statistiko <- function() {
  statistika <- read_csv("podatki/pro-football-reference.csv",
                         na = ":", locale = locale(encoding="UTF-8"))[,c(2:5, 11:12,15, 18:25)]

  colnames(statistika) <- c("Leto", "Krog", "Izbor", "Ime", "All-Pro", "Pro Bowl",
                          "St.tekem", "Uspesne.p", "Podaje",
                          "Jardi.podaje", "Podaje.TD", "Prestrezene.p",
                          "St.tekov","Jardi.tek", "TD.tek")



  write_csv(statistika, "podatki/statistika.csv")
  
}
  

# 
skupna <- merge(statistika, draft.ekipe, by = "Ime")
skupna <- skupna[, c(1:2, 16:17, 3:15)]


