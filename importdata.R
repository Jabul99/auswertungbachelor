library(tidyverse)
dat <- read.csv2('D:\\Desktop\\BIT\\Bachelor\\06_Auswertung\\Angepasste Tabelle.csv')
q1_Suchmaschine <-  subset(dat, select=c("X01_1", "X01_2", "X01_3", "X01_4", "X01_5", "X01_6", "X01b"))
q1_Suchmaschine <-  q1_Suchmaschine %>% rename(google = X01_1, bing = X01_2, yahoo = X01_3, search.ch = X01_4 , escosia.org = X01_5, Andere =X01_6, TextAndere = X01b)

q2_3_4_5_bezahlteLinks <- subset(dat, select=c("X2","X3","X4","X5"))
q2_3_4_5_bezahlteLinks <- q2_3_4_5_bezahlteLinks %>% rename(ZweiteSeite = X2, ProzentBezahlteLinks = X3, ProzentKaufBezahlteLinks = X4, ZweckKauf = X5)

q6_VewendungSozialMedia <- subset(dat, select=c("X06_1", "X06_2", "X06_3", "X06_4", "X06_5", "X06_6", "X06_7", "X06_8", "X06b"))
q6_VewendungSozialMedia <-  q6_VewendungSozialMedia %>% rename(Facebook = X06_1, Instagram = X06_2, YouTube = X06_3, Twitter = X06_4 , LinkedIn = X06_5, Spnapchat =X06_6, TikTok =X06_7, Andere =X06_8, AndereText = X06b)

q7_SucheProduktSozialMedia <- subset(dat, select=c("X07_1", "X07_2"))
q7_SucheProduktSozialMedia <- q7_SucheProduktSozialMedia %>% rename(privat = X07_1, geschäftlich = X07_2)