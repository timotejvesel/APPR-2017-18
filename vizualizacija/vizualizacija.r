# 3. faza: Vizualizacija podatkov

### Skupno število podaj na tekmo vseh podajalcev, ki so bili na naboru izbrani v določenem letu.

a <- podaje.td[podaje.td$tip == "Podaje",]
a <- inner_join(draft.ekipe[,c(1,2)], a, by = "Ime")
a <- inner_join(a, tekme, by = "Ime") 
a <- add_column(a, "podaje/tekma" = a$stevilo / a$St.tekem)
b <- aggregate(a$`podaje/tekma`, by=list(a$Leto), FUN=sum)
colnames(b) <- c("leto","podaje/tekma")

g1 <- ggplot(b, aes(x=b$leto, y = b$`podaje/tekma`)) + geom_col(fill = "#56B4E9") + 
 labs(title="Število podaj na tekmo", x = "Leto", y = "podaje / tekma") + 
 scale_x_continuous(breaks = seq(1980,2015,5))

# Vidimo lahko, da so v povprečju podajalci, ki so bili na naboru izbrani v drugi polovici (po letu 1997) 
# vrgli več podaj na tekmo, kot pa podajalci, ki so bili izbrani pred letom 1997. Torej lahko sklepamo,
# da se v ligi NFL v zadnjih dveh desetletji igra nekoliko več po zraku kot pa se je pred tem.


### Korelacija med izborom na naboru in uspešnostjo pri podajah.

k <- statistika[,c("Izbor", "Krog", "Podaje", "Uspesne.p")]
k["uspesnost.p"] <- (k$Uspesne.p / k$Podaje) * 100
k$uspesnost.p[is.na(k$uspesnost.p)] <- 0 # Tistim, ki nikoli niso igrali damo vrednost 0 (namesto NA)
k$Podaje[is.na(k$Podaje)] <- 0
k$Uspesne.p[is.na(k$Uspesne.p)] <- 0
#k$uspesnost.p[k$uspesnost.p > 67] <- NA 
k <- na.omit(k)

g2 <- ggplot(k, aes(x = k$Izbor, y = k$uspesnost.p)) + geom_point(aes(colour = factor(k$Krog))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,300,50)) + 
  scale_y_continuous(name = "Uspešnost podaj (%)", breaks = seq(0,100,5), limits = c(0, 68)) + 
  labs(title = "Korelacija med izborom in uspešnostjo podaj", colour = "Krog") 

p <- aggregate(k$uspesnost.p, by=list(k$Krog), FUN=mean, na.rm = TRUE)
colnames(p) <- c("krog","povprecje")

g3 <- ggplot(p, aes(x = p$krog, y = p$povprecje)) + geom_col(fill = "thistle2", color = "black") + 
  scale_x_continuous(name = "Krog", breaks = seq(0,12,1)) + 
  scale_y_continuous(name = "Povprečje uspešnih podaj", breaks = seq(0,60,5)) + 
  labs(title = "Uspešnost podaj glede na krog izbora (%)") 

# Opazimo, da res obstaja povezava med izborom na naboru in odstotkom uspešnih podaj.
# Iz grafa g3 je razvidno, da so podajalci, ki so bili izbrani v prvih treh krogih v povprečju res precej uspešnejši
# kot podajalci, ki so bili izbrani v zadnjih treh krogih.
# Pri tem je pomembno poudariti, da so pri izborih v drugi polovici zajeti samo boljši podajalci, ki so bili izbrani
# v teh krogih (zajeti pa so vsi igralci, ki so bili izbrani v prvem krogu), saj slabši podajalci sploh niso dobili
# priložnosti za igro oziroma so zbrali premalo podaj, da bi lahko ocenili njihovo uspešnost.


### Povprečno število touchdownov na tekmo

m <- statistika[, c("Izbor", "Krog", "Podaje.TD", "Ime", "St.tekem")]
#m <- inner_join(m, tekme, by = "Ime" )
m["td.tekma"] <- (m$Podaje.TD/ m$St.tekem)
m$St.tekem[is.na(m$St.tekem)] <- 0
m$Podaje.TD[is.na(m$Podaje.TD)] <- 0
m$td.tekma[is.na(m$td.tekma)] <- 0

g4 <- ggplot(m, aes(x = m$Izbor, y = m$td.tekma)) + geom_point(aes(colour = factor(m$Krog))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,300,50)) + 
  scale_y_continuous(name = "touchdown/tekma", breaks = seq(0,2.3,0.2)) + 
  labs(title = "Povprečno število touchdownov na tekmo", colour = "Krog") 

n <- aggregate(m$td.tekma, by=list(m$Krog), FUN=mean, na.rm = TRUE)
colnames(n) <- c("krog","povprecje")

g5 <- ggplot(n, aes(x = n$krog, y = n$povprecje)) + geom_col(fill = "bisque1", color = "black") + 
  scale_x_continuous(name = "Krog", breaks = seq(0,12,1)) + 
  scale_y_continuous(name = "Povprečje td/tekma") + 
  labs(title = "Povprečje touchdownov/tekma glede na krog izbora") 

# Spet je razvidno, da so podajalci, ki so bili izbrani v prvem krogu v povprečju boljši od ostalih igralcev,
# vendar pa je vsake toliko časa kakšen podajalec izbran precej nižje, čeprav se kasneje izkaže,
# da mu je uspela boljša kariera kot pa ostalim.


### povprečno število pridobljenih jardov na tekmo
z <- statistika[, c("Izbor", "Krog", "Jardi.podaje", "Ime", "St.tekem")]
#z <- inner_join(z, tekme, by = "Ime" )
z["jardi.tekma"] <- (z$Jardi.podaje/ z$St.tekem)
z$St.tekem[is.na(z$St.tekem)] <- 0
z$Jardi.podaje[is.na(z$Jardi.podaje)] <- 0
z$jardi.tekma[is.na(z$jardi.tekma)] <- 0

g10 <- ggplot(z, aes(x = z$Izbor, y = z$jardi.tekma)) + geom_point(aes(colour = factor(z$Krog))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,300,50)) + 
  scale_y_continuous(name = "touchdown/tekma", breaks = seq(0,285,50)) + 
  labs(title = "Povprečno število pridobljenih jardov (s podajami) na tekmo", colour = "Krog") 

t <- aggregate(z$jardi.tekma, by=list(z$Krog), FUN=mean, na.rm = TRUE)
colnames(t) <- c("krog","povprecje")

g11 <- ggplot(t, aes(x = t$krog, y = t$povprecje)) + geom_col(fill = "bisque1", color = "black") + 
  scale_x_continuous(name = "Krog", breaks = seq(0,285,50)) + 
  scale_y_continuous(name = "Povprečje td/tekma") + 
  labs(title = "Povprečje jardi(po zraku)/tekma glede na krog izbora") 

### Ekipe, ki so izbrale podajalce v prvem krogu in ekipe, ki so izbrale "najboljše" podajalce.

ekipe <- draft.ekipe[, c("Ime", "Krog", "Izbor", "Ekipa")]
ekipe$Krog[ekipe$Krog > 1] <- NA
ekipe <- na.omit(ekipe)

st.1krog <- aggregate(ekipe$Krog, by=list(ekipe$Ekipa), FUN=sum)
colnames(st.1krog) <- c("ekipa", "stevilo")
st.1krog["14", "ekipa"] <- "Tennessee Titans"
st.1krog["3", "ekipa"] <- "Indianapolis Colts"
st.1krog["19", "ekipa"] <- "Oakland Raiders"
st.1krog["33", "ekipa"] <- "Los Angeles Rams"
st.1krog["32", "ekipa"] <- "Arizona Cardinals"

st.1krog <- aggregate(st.1krog$stevilo, by=list(st.1krog$ekipa), FUN=sum)
colnames(st.1krog) <- c("ekipa", "stevilo")


top <- statistika[,c("Ime","Podaje", "Uspesne.p", "St.tekem", "Podaje.TD")]
ekipa <- draft.ekipe[,c("Ime", "Ekipa")]
top["uspesnost.p"] <- (top$Uspesne.p / top$Podaje) * 100
top["td.tekma"] <- (top$Podaje.TD / top$St.tekem)
top <- merge(top, ekipa, by = "Ime")

top$Podaje[top$Podaje < 100] <- NA 
top$St.tekem[top$St.tekem < 10] <- NA
top$uspesnost.p[top$uspesnost.p < 58] <- NA
top$td.tekma[top$td.tekma < 1.25] <- NA 
top[top==0] <- NA
top <- na.omit(top)
top$nova <- rep(1, nrow(top))

top.ekipe <- aggregate(top$nova, by = list(top$Ekipa), FUN=sum)
colnames(top.ekipe) <- c("ekipa", "stevilo")

### Zemljevidi

zemljevid <- uvozi.zemljevid("http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_500k.zip", 
                             "cb_2016_us_state_500k",encoding = "UTF-8") %>% 
  pretvori.zemljevid() %>% filter(long > -135, long < -60, lat > 20, lat < 52)

zemljevid$NAME <- parse_character(zemljevid$NAME)


zemljevid.drzave <- ggplot() +
  geom_polygon(data = left_join(zemljevid, drzave %>% filter(Stevilo > 0) , 
                                by = c("NAME" = "Drzava")), 
                                aes(x = long, y = lat, group = group, fill = Stevilo), 
                                color = "black") +
  ggtitle("Število NFL ekip v zveznih državah ZDA") + xlab("") + ylab("") +
  guides(fill = guide_colorbar(title = "Število ekip")) +
  scale_fill_continuous(low = "#56B1F7", high = "#132B43")


#+
  #geom_text(data = inner_join(zemljevid, drzave %>% filter(Stevilo > 0), by = c("NAME" = "Drzava")) %>% group_by(NAME) %>%
   #        summarise(avg_long = mean(long), avg_lat = mean(lat)),
    #       aes(x = avg_long, y = avg_lat, label = NAME), color = "red")


### Za shiny

stat.tekma <- data.frame(statistika)
stat.tekma$St.tekem[stat.tekma$St.tekem < 20] <- NA # Izločimo podajalce s premalo tekmami.
stat.tekma <- na.omit(stat.tekma)
stat.tekma["Uspesne.p"] <- stat.tekma$Uspesne.p / stat.tekma$St.tekem
stat.tekma["Podaje"] <- stat.tekma$Podaje / stat.tekma$St.tekem
stat.tekma["Jardi.podaje"] <- stat.tekma$Jardi.podaje / stat.tekma$St.tekem
stat.tekma["Podaje.TD"] <- stat.tekma$Podaje.TD / stat.tekma$St.tekem
stat.tekma["Prestrezene.p"] <- stat.tekma$Prestrezene.p / stat.tekma$St.tekem
stat.tekma["St.tekov"] <- stat.tekma$St.tekov / stat.tekma$St.tekem
stat.tekma["Jardi.tek"] <- stat.tekma$Jardi.tek / stat.tekma$St.tekem
stat.tekma["TD.tek"] <- stat.tekma$TD.tek / stat.tekma$St.tekem

# "Boljša" rešitev za 
#test <- data.frame(statistika)
#test$St.tekem[test$St.tekem < 20] <- NA # Izločimo podajalce s premalo tekmami.
#test <- na.omit(test)
#test <- test[,c(-5, -6)]
#nova <- lapply(6:13, . %>% {mutate(test[.] <- test[.] / test$St.tekem)})
#nova["ime"] <- test$Ime


prvi <- aggregate(stat.tekma$Uspesne.p, by=list(stat.tekma$Krog), FUN=mean, na.rm = TRUE)
colnames(prvi) <- c("Krog","Uspesne.podaje")
drugi <- aggregate(stat.tekma$Podaje, by=list(stat.tekma$Krog), FUN=mean, na.rm = TRUE)
colnames(drugi) <- c("Krog","Podaje")
tretji <- aggregate(stat.tekma$Jardi.podaje, by=list(stat.tekma$Krog), FUN=mean, na.rm = TRUE)
colnames(tretji) <- c("Krog","Jardi.podaje")
cetrti <- aggregate(stat.tekma$Podaje.TD, by=list(stat.tekma$Krog), FUN=mean, na.rm = TRUE)
colnames(cetrti) <- c("Krog","Podaje.TD")
peti <- aggregate(stat.tekma$Prestrezene.p, by=list(stat.tekma$Krog), FUN=mean, na.rm = TRUE)
colnames(peti) <- c("Krog","Prestrezene.podaje")
sesti <- aggregate(stat.tekma$St.tekov, by=list(stat.tekma$Krog), FUN=mean, na.rm = TRUE)
colnames(sesti) <- c("Krog","St.tekov")
sedmi <- aggregate(stat.tekma$Jardi.tek, by=list(stat.tekma$Krog), FUN=mean, na.rm = TRUE)
colnames(sedmi) <- c("Krog","Jardi.tek")
osmi <- aggregate(stat.tekma$TD.tek, by=list(stat.tekma$Krog), FUN=mean, na.rm = TRUE)
colnames(osmi) <- c("Krog","TD.tek")

povprecja <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                    list(prvi, drugi, tretji,cetrti, peti, sesti, sedmi, osmi))

##### Primerjava v prvem krogu (6. izbor pomeni 6-10, 11 pomeni 11-15 itd...)
## Uspešnost podaj

kk <- prvi.krog[,c("Izbor", "Krog", "Podaje", "Uspesne.p")]
kk["uspesnost.p"] <- (kk$Uspesne.p / kk$Podaje) * 100
#kk$Podaje[k$Podaje < 100] <- NA # Izločimo podajalce s premalo podajami.
#kk[kk==0] <- NA
#kk <- na.omit(kk)

g7 <- ggplot(kk, aes(x = kk$Izbor, y = kk$uspesnost.p)) + geom_point(aes(colour = factor(kk$Izbor))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,35,1)) + 
  scale_y_continuous(name = "Uspešnost podaj (%)", breaks = seq(0,100,5), limits = c(43, 68)) + 
  labs(title = "Korelacija med izborom in uspešnostjo podaj", colour = "Izbor") 

pp <- aggregate(kk$uspesnost.p, by=list(kk$Izbor), FUN=mean, na.rm = TRUE)
colnames(pp) <- c("izbor","povprecje")

g8 <- ggplot(pp, aes(x = pp$izbor, y = pp$povprecje)) + geom_col(fill = "thistle2", color = "black") + 
  scale_x_continuous(name = "Izbor", breaks = seq(1,50,1)) + 
  scale_y_continuous(name = "Povprečje uspešnih podaj", breaks = seq(0,60,5)) + 
  labs(title = "Uspešnost podaj glede na krog izbora (%)") 

## Povprečno TD per game

mm <- prvi.krog[, c("Izbor", "Krog", "Jardi.podaje", "Podaje.TD", "Ime", "St.tekem")]
#mm <- inner_join(mm, tekme, by = "Ime" )
mm["td.tekma"] <- (mm$Podaje.TD/ mm$St.tekem)
#mm$St.tekem[mm$St.tekem < 10] <- NA # Izločimo podajalce s premalo tekmami.
#mm[mm==0] <- NA
#mm <- na.omit(mm)

g16 <- ggplot(mm, aes(x = mm$Izbor, y = mm$td.tekma)) + geom_point(aes(colour = factor(mm$Izbor))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,35,1)) + 
  scale_y_continuous(name = "Uspešnost podaj (%)", breaks = seq(0,2.1,0.2), limits = c(0, 2.1)) + 
  labs(title = "Korelacija med izborom in uspešnostjo podaj", colour = "Izbor") 


nn <- aggregate(mm$td.tekma, by=list(mm$Izbor), FUN=mean, na.rm = TRUE)
colnames(nn) <- c("izbor","povprecje")

g9 <- ggplot(nn, aes(x = nn$izbor, y = nn$povprecje)) + geom_col(fill = "bisque1", color = "black") + 
  scale_x_continuous(name = "Izbor", breaks = seq(1,32,1)) + 
  scale_y_continuous(name = "Povprečje td/tekma") + 
  labs(title = "Povprečje touchdownov/tekma glede izbor v prvem krogu")

### Jardi/game
zz <- prvi.krog[, c("Izbor", "Krog", "Jardi.podaje", "Podaje.TD", "Ime", "St.tekem")]
#zz <- inner_join(zz, tekme, by = "Ime" )
zz["jardi.tekma"] <- (zz$Jardi.podaje/ zz$St.tekem)
#zz$St.tekem[z$St.tekem < 10] <- NA # Izločimo podajalce s premalo tekmami.
#zz[z==0] <- NA
#zz <-na.omit(zz)

g17 <- ggplot(zz, aes(x = zz$Izbor, y = zz$jardi.tekma)) + geom_point(aes(colour = factor(mm$Izbor))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,35,1)) + 
  scale_y_continuous(name = "Uspešnost podaj (%)", breaks = seq(0,285,50), limits = c(0, 290)) + 
  labs(title = "Korelacija med  v prvem krogu in povprečnim številom jardov na tekmo", colour = "Izbor") 

tt <- aggregate(zz$jardi.tekma, by=list(zz$Izbor), FUN=mean, na.rm = TRUE)
colnames(tt) <- c("izbor","povprecje")

g12 <- ggplot(tt, aes(x = tt$izbor, y = tt$povprecje)) + geom_col(fill = "bisque1", color = "black") + 
  scale_x_continuous(name = "Izbor", breaks = seq(0,250,25)) + 
  scale_y_continuous(name = "Povprečje jardi/tekma") + 
  labs(title = "Povprečje tjardov po zraku/tekma glede na izbor v prvem krogu") 


#### NFL rate
r <- statistika[,c("Izbor", "Krog", "rate")]
r$rate[is.na(r$rate)] <- 0 # Na 0 nastavimo tiste, ki imajo NA, torej niso nikoli igrali (so preslabi)
r$rate[r$rate > 115] <- NA # Izločimo podajalce s prevelikim rate (nereprezntatvnim)
r$rate[r$rate < 0] <- 0
r <- na.omit(r)

f <- statistika[,c("Izbor", "Krog", "rate")]
f$rate[f$rate > 115] <- NA # Izločimo podajalce s prevelikim rate (nereprezntatvnim)
f$rate[f$rate < 0] <- 0
f <- na.omit(f)

g13 <- ggplot(r, aes(x = r$Izbor, y = r$rate)) + geom_point(aes(colour = factor(r$Krog))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,300,50)) + 
  scale_y_continuous(name = "NFL rate", breaks = seq(0,160,5), limits = c(0, 115)) + 
  labs(title = "Korelacija med izborom in NFl rate", colour = "Krog") 

q <- aggregate(r$rate, by=list(r$Krog), FUN=mean, na.rm = TRUE)
colnames(q) <- c("krog","rate.povprecje")

g14 <- ggplot(q, aes(x = q$krog, y = q$rate)) + geom_col(fill = "thistle2", color = "black") + 
  scale_x_continuous(name = "Krog", breaks = seq(0,12,1)) + 
  scale_y_continuous(name = "Povprečje NFL rate", breaks = seq(0,115,5)) + 
  labs(title = "Povprečje NFL passing rate glede na krog izbora") 

# prvi krog
rr <- prvi.krog[,c("Izbor", "Krog", "rate")]
rr$rate[rr$rate > 115] <- NA # Izločimo podajalce s prevelikim rate (nereprezntatvnim)
rr <- na.omit(rr)

qq <- aggregate(rr$rate, by=list(rr$Izbor), FUN=mean, na.rm = TRUE)
colnames(qq) <- c("izbor","rate.povprecje")

g15 <- ggplot(qq, aes(x = qq$izbor, y = qq$rate)) + geom_col(fill = "thistle2", color = "black") + 
  scale_x_continuous(name = "Izbor", breaks = seq(1,26,1)) + 
  scale_y_continuous(name = "Povprečje NFL rate", breaks = seq(0,115,5)) + 
  labs(title = "Povprečje NFL passing rate  glede na izbor v prvem krogu") 


##### Vizualizacija topQB

g16 <- ggplot(topQB, aes(x = topQB$QB, y = topQB$jardi.zrak.tekma)) + geom_col(fill = "thistle2", color = "black") + 
  scale_x_discrete(name = "QB") + 
  scale_y_continuous(name = "Popvrečno stevilo jardov po zraku na tekmo", breaks = seq(0,250,50)) + 
  labs(title = "Povpreco stevilo jaedov po zraku na tekmo glede na vrsto QB, ki je igral") 

g17 <- ggplot(topQB, aes(x = topQB$QB, y = topQB$jardi.tek.tekma)) + geom_col(fill = "bisque1", color = "black") + 
  scale_x_discrete(name = "QB") + 
  scale_y_continuous(name = "Average rushing jards per game", breaks = seq(0,150,25)) + 
  labs(title = "Average rushing jards per game") 

g18 <- ggplot(topQB, aes(x = topQB$QB, y = topQB$podaja.TD.tekma)) + geom_col(fill = "orchid2", color = "black") + 
  scale_x_discrete(name = "QB") + 
  scale_y_continuous(name = "Average number of passes for TD per game", breaks = seq(0,2.5,0.25)) + 
  labs(title = "Average number of passes for TD per game") 

g19 <- ggplot(topQB, aes(x = topQB$QB, y = topQB$interception.tekma)) + geom_col(fill = "olivedrab2", color = "black") + 
  scale_x_discrete(name = "QB") + 
  scale_y_continuous(name = "Average number of interceptions per game", breaks = seq(0,1,0.1)) + 
  labs(title = "Average number of interceptions per game") 

g20 <- ggplot(topQB, aes(x = topQB$QB, y = topQB$pass.percentage)) + geom_col(fill = "skyblue1", color = "black") + 
  scale_x_discrete(name = "QB") + 
  scale_y_continuous(name = "Percentage of successful passes", breaks = seq(0,80,10)) + 
  labs(title = "Percentage of successful passes") 

g21 <- ggplot(topQB, aes(x = topQB$QB, y = topQB$zmage.porazi)) + geom_col(fill = "tomato1", color = "black") + 
  scale_x_discrete(name = "QB") + 
  scale_y_continuous(name = "Wins and loses ratio", breaks = seq(0,5,0.25)) + 
  labs(title = "Wins and loses ratio") 

#### plače in uspesnost 2017

place <- inner_join(tabela1[,c(2,20)], tabela2[,c(1,3)], by = "Player")

g22 <- ggplot(place, aes(x = place$`Salary Cap Value`, y = place$Rate)) + geom_point(colour = "black") +
  scale_x_continuous(name = "Salary Cap Value", breaks = seq(280000,25000000,10000000)) + 
  scale_y_continuous(name = "Passer Rate", breaks = seq(54,130,10)) + 
  labs(title = "Povprečno število pridobljenih jardov (s podajami) na tekmo")

g23 <- g22 + geom_smooth(method = "lm")

##### interceptions per game


