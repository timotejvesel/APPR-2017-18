# HTML
link <- "http://www.nfl.com/draft/history/fulldraft?type=position"
stran <- html_session(link) %>% read_html()

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

### Dodaj stolpec NFL QB rating
statistika$jardi.podaja <- statistika$Jardi.podaje / statistika$Podaje

# procent podaje za TD/podaje
statistika$TD.podaja <- 100 * (statistika$Podaje.TD / statistika$Podaje)

statistika$prestrezene.podaja <- 100 * (statistika$Prestrezene.p / statistika$Podaje)

statistika$procent.uspesnih <- 100 * (statistika$Uspesne.p / statistika$Podaje)

statistika$rate <- ((statistika$procent.uspesnih - 30) * 0.05 +  (statistika$jardi.podaja - 3) * 0.25 + 
                  statistika$TD.podaja * 0.2 + (2.375 - statistika$prestrezene.podaja * 0.25)) / 6 * 100
###
uvozi.prvi.krog <- function() {
  prvi.krog <- read_csv("podatki/prvi-krog.csv",
                         na = ":", locale = locale(encoding="UTF-8"))[,c(2:5, 11:12,15, 18:25)]
  
  colnames(prvi.krog) <- c("Leto", "Krog", "Izbor", "Ime", "All.Pro", "Pro.Bowl",
                            "St.tekem", "Uspesne.p", "Podaje",
                            "Jardi.podaje", "Podaje.TD", "Prestrezene.p",
                            "St.tekov","Jardi.tek", "TD.tek")
  
  
  return(prvi.krog)
  
}
prvi.krog <- uvozi.prvi.krog()

### Dodaj stolpec NFL QB rating
prvi.krog$jardi.podaja <- prvi.krog$Jardi.podaje / prvi.krog$Podaje

# procent podaje za TD/podaje
prvi.krog$TD.podaja <- 100 * (prvi.krog$Podaje.TD / prvi.krog$Podaje)

prvi.krog$prestrezene.podaja <- 100 * (prvi.krog$Prestrezene.p / prvi.krog$Podaje)

prvi.krog$procent.uspesnih <- 100 * (prvi.krog$Uspesne.p / prvi.krog$Podaje)

prvi.krog$rate <- ((prvi.krog$procent.uspesnih - 30) * 0.05 +  (prvi.krog$jardi.podaja - 3) * 0.25 + 
                     prvi.krog$TD.podaja * 0.2 + (2.375 - prvi.krog$prestrezene.podaja * 0.25)) / 6 * 100



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


#### uvoz tabele s primerjavo ekip top QB in non-top QB
uvozi.topQB <- function() {
  topQB <- read_csv("podatki/topQB.csv",
                         na = ":", locale = locale(encoding="UTF-8"))
  
  return(topQB)
  
}

topQB <- uvozi.topQB()

### Uvoz podatkov za sezono 2017
link1 <- "http://www.nfl.com/stats/categorystats?seasonType=REG&d-447263-n=1&d-447263-o=2&d-447263-p=1&statisticPositionCategory=QUARTERBACK&d-447263-s=PASSING_YARDS_GAME_AVG&tabSeq=1&season=2017&Submit=Go&experience=&archive=true&conference=null&qualified=false"
stran1 <- html_session(link1) %>% read_html()
tabela1 <- stran1 %>% html_nodes(xpath = "//table[@class='data-table1']") %>% .[[1]] %>% html_table()

# place
link2 <- "https://overthecap.com/position/quarterback/2017"
stran2 <- html_session(link2) %>% read_html()
tabela2 <- stran2 %>% html_nodes(xpath = "//table[@class='position-table sortable']") %>% .[[1]] %>% html_table()

tabela2$`Salary Cap Value` <- gsub("\\$","",tabela2$`Salary Cap Value`)
tabela2$`Salary Cap Value` <- parse_double(gsub(",","",tabela2$`Salary Cap Value`))

