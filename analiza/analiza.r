# 4. faza: Analiza podatkov

skupine <- left_join(st.1krog, super.bowl[,c(1,3)], by = "ekipa")
colnames(skupine) <- c("ekipa", "st.1izborov", "zmage.SB")

km <- kmeans(skupine[2], 3, nstart = 1000)

razvrstitev <- data.frame(ekipa = skupine$ekipa, skupina = factor(km$cluster),
                            zmage.SB = skupine$zmage.SB)

graf.povezava1 <- ggplot(razvrstitev, aes(x = ekipa, 
                                         y = zmage.SB)) + 
  geom_point(colour = razvrstitev$skupina, size = 4) +
  xlab("Ekipa") + 
  ylab("Število naslovov") +
  ggtitle("Število osvojenih naslovov glede na število izbranih podajalcev v prvem krogu") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))




graf.podaje <- ggplot(b, aes(x=b$leto, y = b$`podaje/tekma`)) + geom_point(fill = "#56B4E9") + 
  labs(title="Število podaj na tekmo", x = "Leto", y = "podaje / tekma") + 
  scale_x_continuous(breaks = pretty(b$leto, n = 10))

graf.podaje <- graf.podaje + geom_smooth(method = 'lm', color = 'red')
  

#######

brees <- read_csv("podatki/brees.csv",
                       locale = locale(encoding="UTF-8"))
brees<- brees[,c(1,5,6,7)]

colnames(brees) <- c("leto", "usp.podaj", "jardi", "touchdown")



pomozna.napoved <- lm(usp.podaj ~ leto + I(leto^2), data = brees)
leta <- data.frame(leto = seq(2018,2021))
napoved <- leta %>% mutate(uspesnost.podaj = round(predict(pomozna.napoved, .)))

graf.usp <- ggplot(brees,(aes(x = brees$leto, y = brees$usp.podaj))) + geom_point() +
  geom_smooth(method = 'lm',  formula = y ~ x + I(x^2))


pomozna.napoved2 <- lm(jardi ~ leto + I(leto^2), data = brees)
napoved <- napoved %>% mutate(jardi = round(predict(pomozna.napoved2, .)))


graf.jardi <- ggplot(brees,(aes(x = brees$leto, y = brees$jardi))) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))

pomozna.napoved3 <- lm(touchdown ~ leto + I(leto^2), data = brees)
napoved <- napoved %>% mutate(touchdown = round(predict(pomozna.napoved3, .)))

graf.td <- ggplot(brees,(aes(x = brees$leto, y = brees$touchdown))) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))


tabela.graf <- bind_rows(brees[c(1,4)], napoved[c(1,4)])

graf.td.napoved <- ggplot(tabela.graf, aes(x = leto, y = touchdown)) + 
  geom_point() + scale_x_continuous(name = "Leto", breaks = seq(2001,2021,1)) + ylab("Podaje za touchdown") +
  ggtitle("Število podaj za touchdown na leto") +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), color = "green")
  