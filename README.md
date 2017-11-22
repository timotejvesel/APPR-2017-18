# Analiza podatkov s programom R, 2017/18

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2017/18

## Tematika



## Analiza podajalcev na draftu lige NFL
V projektu bom analiziral statistiko podajalcev (quarterback) v ligi NFL, ki so bili na naboru izbrani v zadnjih 30 letih. Analiziral bo opravljena na podlagi naslednjih podatkov:  uspešnost podaj, število pridobljenih jardov s podajami, število podaj za zadetek (touchdown), število izborov v prvo ekipo lige in število izborov na Pro Bowl.

Zanima me, kakšna je povezava med izborom na draftu in uspešnostjo kariere.
Analizirati želim tudi, katere ekipe so imele največ izborov v prvem krogu in pa, katere ekipe so izbrale največ podajalcev, ki so jim uspele vrhunske kariere.

Podatke bom črpal iz naslednji spletnih strani:

* http://www.nfl.com/draft/history/fulldraft?type=position
* https://www.pro-football-reference.com/ (CSV)




## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
