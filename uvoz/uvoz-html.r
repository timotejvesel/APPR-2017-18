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
draft.ekipe <- draft.ekipe[,c(3,6,1:2,4:5)]

# CSV
uvozi.statistiko <- function() {
  statistika <- read_csv("podatki/pro-football-reference1.csv",
                         na = ":", locale = locale(encoding="UTF-8"))[,c(2:5, 11:12,15, 18:25)]

  colnames(statistika) <- c("Leto", "Krog", "Izbor", "Ime", "All.Pro", "Pro.Bowl",
                          "St.tekem", "Uspesne.p", "Podaje",
                          "Jardi.podaje", "Podaje.TD", "Prestrezene.p",
                          "St.tekov","Jardi.tek", "TD.tek")


  return(statistika)
  
}

statistika <- uvozi.statistiko()

podaje.td <- statistika %>% select(Ime, Uspesne.p, Podaje, Podaje.TD, Prestrezene.p, TD.tek) %>%
  melt(variable.name = "tip", value.name = "stevilo", na.rm = TRUE)

jardi <- statistika %>% select(Ime, Podaje, Jardi.podaje, St.tekov, Jardi.tek) %>% 
  melt(variable.name = "tip", value.name = "stevilo", na.rm = TRUE)

tekme <- statistika[,c("Ime","St.tekem")]
tekme[is.na(tekme)] <- 0

pro <- statistika %>% select(Ime, All.Pro, Pro.Bowl) %>% 
  melt(variable.name = "tip", value.name = "stevilo", na.rm = TRUE) 

# draft <- select(statistika, Ime, Leto, Krog, Izbor)

drzave <-  read_csv("podatki/team-states2.csv",
                    locale = locale(encoding="UTF-8"))
colnames(drzave) <- c("Drzava", "Stevilo", "Regija")

### 

super.bowl <- read_csv("podatki/super-bowl.csv",
                       locale = locale(encoding="UTF-8"))
super.bowl<- super.bowl[,c(2:5)]
colnames(super.bowl) <- c("ekipa", "tekme", "zmage", "porazi")
