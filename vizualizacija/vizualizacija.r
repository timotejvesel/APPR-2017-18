# 3. faza: Vizualizacija podatkov

### Skupno število podaj na tekmo vseh podajalcev, ki so bili na naboru izbrani v določenem letu.

x <- c("Podaje", "Uspesne.p", "Podaje.TD", "Prestrezene.p", "TD.tek")
a <- inner_join(draft.ekipe[,c(1,2)], podaje.td, by = "Ime") %>% mutate(tip = factor(tip, levels = x)) %>%
  arrange(tip) %>% distinct(Ime,.keep_all = TRUE)
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
k$Podaje[k$Podaje < 100] <- NA # Izločimo podajalce s premalo podajami.
k[k==0] <- NA
k <- na.omit(k)

g2 <- ggplot(k, aes(x = k$Izbor, y = k$uspesnost.p)) + geom_point(aes(colour = factor(k$Krog))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,300,50)) + 
  scale_y_continuous(name = "Uspešnost podaj (%)", breaks = seq(0,100,5), limits = c(43, 68)) + 
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

m <- statistika[, c("Izbor", "Krog", "Jardi.podaje", "Podaje.TD", "Ime")]
m <- inner_join(m, tekme, by = "Ime" )
m["td.tekma"] <- (m$Podaje.TD/ m$St.tekem)
m$St.tekem[m$St.tekem < 10] <- NA # Izločimo podajalce s premalo tekmami.
m[m==0] <- NA
m <- na.omit(m)

g4 <- ggplot(m, aes(x = m$Izbor, y = m$td.tekma)) + geom_point(aes(colour = factor(m$Krog))) +
  scale_x_continuous(name = "Izbor", breaks = seq(0,300,50)) + 
  scale_y_continuous(name = "touchdown/tekma", breaks = seq(0,2,0.25)) + 
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


### Ekipe, ki so izbrale podajalce v prvem krogu in ekipe, ki so izbrale "najboljše" podajalce.

ekipe <- draft.ekipe[, c("Ime", "Krog", "Izbor", "Ekipa")]
ekipe$Krog[ekipe$Krog > 1] <- NA
ekipe[ekipe==0] <- NA
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

