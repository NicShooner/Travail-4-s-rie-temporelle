library(readr)
hawai <- read_csv("Downloads/hawai.csv")
View(hawai)
library("forecast")
library("fpp2")
#série temporelle du CO2
hawai_graph<- hawai %>%
  ggplot(aes(x=time, y = CO2))+
  geom_line()
hawai_graph

#convertir la colonne "time" en date
hawai_date<- hawai%>%
  mutate(time= date_decimal(time))
view(hawai_date)

#préparation de la série temporelle
hawai_ts<- ts(hawai_date %>% select(-time),
              start = c(hawai_date$time[1] %>% year(), 1),
              frequency = 12)
autoplot(hawai_ts)

#séparation de la série en partie d'entrainement et de test 
#sur les 43 années de données, 70% correspond à environ 30 ans donc 1958 à 1988
hawai_ts_train<- window(hawai_ts,  end= 1988.999)
hawai_ts_test<- window(hawai_ts, start= 1989)

#modèle prévisionnel à l'aide de la méthode SES avec fluctuation saisonnière
hawai_prev<-hw(hawai_ts_train, h=12*10, alpha = 0.8)
autoplot(hawai_prev)+ 
  autolayer(hawai_ts_test)
#analyse des résidus
hawai_prev %>%
  checkresiduals()

#aucune structure n'est identifiable dans le nuage de résidus
#le P-value est très bas, bien en bas de 0,05. 
#Ce qui "appuie" l'hypothèse que les résidus ont été générés par un bruit blanc
#l'histogramme des résidus semble suivre une distribution normale !
#en général le modèle semble fiable ! 
#On peut le voir graphiquement en comparent le modèle prévisionnel aux données test
#Mais aussi en regardant les résidus
#En terme d'amélioration, ça s'emble bien...
#peut ête changer le alpha ou prendre la méthode ARIMA car elle permet d'inclure des covariables



