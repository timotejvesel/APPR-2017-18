### Manning
manning <- read_csv("podatki/manning.csv",
                  locale = locale(encoding="UTF-8"))
manning<- manning[,c(1,4,5,6)]

colnames(manning) <- c("leto", "usp.podaj", "jardi", "touchdown")

pomozna.napoved <- lm(usp.podaj ~ leto + I(leto^2), data = manning)
leta <- data.frame(leto = seq(2018,2021))
napoved <- leta %>% mutate(usp.podaj = round(predict(pomozna.napoved, .)))

graf.usp2 <- ggplot(manning,(aes(x = manning$leto, y = manning$usp.podaj))) + geom_point() +
  geom_smooth(method = 'lm',  formula = y ~ x + I(x^2))


pomozna.napoved2 <- lm(jardi ~ leto + I(leto^2), data = manning)
napoved <- napoved %>% mutate(jardi = round(predict(pomozna.napoved2, .)))


graf.jardi2 <- ggplot(manning,(aes(x = manning$leto, y = manning$jardi))) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))

pomozna.napoved3 <- lm(touchdown ~ leto + I(leto^2), data = manning)
napoved <- napoved %>% mutate(touchdown = round(predict(pomozna.napoved3, .)))

graf.td2 <- ggplot(manning,(aes(x = manning$leto, y = manning$touchdown))) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))

### Brady
brady <- read_csv("podatki/brady.csv",
                    locale = locale(encoding="UTF-8"))
brady<- brady[,c(1,5,6,7)]

colnames(brady) <- c("leto", "usp.podaj", "jardi", "touchdown")

pomozna.napoved <- lm(usp.podaj ~ leto + I(leto^2), data = brady)
leta <- data.frame(leto = seq(2018,2021))
napoved <- leta %>% mutate(usp.podaj = round(predict(pomozna.napoved, .)))

graf.usp3 <- ggplot(brady,(aes(x = brady$leto, y = brady$usp.podaj))) + geom_point() +
  geom_smooth(method = 'lm',  formula = y ~ x + I(x^2))


pomozna.napoved2 <- lm(jardi ~ leto + I(leto^2), data = brady)
napoved <- napoved %>% mutate(jardi = round(predict(pomozna.napoved2, .)))


graf.jardi3 <- ggplot(brady,(aes(x = brady$leto, y = brady$jardi))) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))

pomozna.napoved3 <- lm(touchdown ~ leto + I(leto^2), data = brady)
napoved <- napoved %>% mutate(touchdown = round(predict(pomozna.napoved3, .)))

graf.td3 <- ggplot(brady,(aes(x = brady$leto, y = brady$touchdown))) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))

### E. Manning
emanning <- read_csv("podatki/emanning.csv",
                  locale = locale(encoding="UTF-8"))
emanning<- emanning[,c(1,2,3,4)]

colnames(emanning) <- c("leto", "usp.podaj", "jardi", "touchdown")

pomozna.napoved <- lm(usp.podaj ~ leto + I(leto^2), data = emanning)
leta <- data.frame(leto = seq(2018,2021))
napoved <- leta %>% mutate(usp.podaj = round(predict(pomozna.napoved, .)))

graf.usp4 <- ggplot(emanning,(aes(x = emanning$leto, y = emanning$usp.podaj))) + geom_point() +
  geom_smooth(method = 'lm',  formula = y ~ x + I(x^2))


pomozna.napoved2 <- lm(jardi ~ leto + I(leto^2), data = emanning)
napoved <- napoved %>% mutate(jardi = round(predict(pomozna.napoved2, .)))


graf.jardi4 <- ggplot(emanning,(aes(x = emanning$leto, y = emanning$jardi))) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))

pomozna.napoved3 <- lm(touchdown ~ leto + I(leto^2), data = emanning)
napoved <- napoved %>% mutate(touchdown = round(predict(pomozna.napoved3, .)))

graf.td4 <- ggplot(emanning,(aes(x = emanning$leto, y = emanning$touchdown))) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2))
