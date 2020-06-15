library(dplyr)
library(tidyverse)
library(magrittr)
library(knitr)
library(dplyr)
library(stringr)

dat <- read.csv2('D:\\Desktop\\BIT\\Bachelor\\06_Auswertung\\Angepasste Tabelle.csv')

q24_Alter <- subset(dat, select=c("Nummer", "X24"))
q24_Alter <- q24_Alter %>% rename(Alter = X24)

levels = c("unter 18 Jahre", "18-25", "26-32","33-40","41-55","56-65","über 65 Jahre", "Leer")
q24_Alter <- q24_Alter %>% mutate(AlterText = case_when(
  Alter == 1 ~ levels[1], 
  Alter == 2 ~ levels[2], 
  Alter == 3 ~ levels[3],
  Alter == 4 ~ levels[4],
  Alter == 5 ~ levels[5],
  Alter == 6 ~ levels[6],
  Alter == 7 ~ levels[7],
  TRUE ~  levels[8]) %>% factor(levels))


q23_Geschlecht <- subset(dat, select=c("Nummer", "X23"))
q23_Geschlecht <- q23_Geschlecht %>% rename(Geschlecht = X23)
levels = c("männlich", "weiblich", "Keine Auswahl")
q23_Geschlecht <- q23_Geschlecht %>% mutate(GeschlechtText = case_when(Geschlecht == 1 ~ levels[1], Geschlecht == 2 ~ levels[2], TRUE ~  levels[3]) %>% factor(levels))


q23_Geschlecht %>% ggplot(aes(x= GeschlechtText, fill = GeschlechtText)) + labs(x = "Geschlecht", fill = "Geschlecht", y="Anzahl")  + geom_bar() + geom_text(aes(label =stat(count)), stat= 'count', position = position_stack(vjust = .5),color="white") + scale_fill_manual(values=colorsGender)


q24_AlterJoined <- left_join(q24_Alter,q23_Geschlecht, by = "Nummer")

q24_Alter %>% ggplot(aes(x= AlterText, fill = AlterText)) + labs(x = "Alter", fill = "Alter", y="Anzahl")  + geom_bar() + geom_text(aes(label =stat(count)), stat= 'count', position = position_stack(vjust = .5))

q24_AlterJoined %>% ggplot(aes(x=AlterText, fill =GeschlechtText))+ labs(x = "Alter", y="Anzahl")  + geom_bar() + geom_text(aes(label =stat(count)), stat= 'count', position = position_stack(vjust = .5), color ="white")+ scale_fill_manual(values=c(colorMan, colorWoman, colorGenderNot))


q1_Suchmaschine <-  subset(dat, select=c("Nummer", "X01_1", "X01_2", "X01_3", "X01_4", "X01_5", "X01_6", "X01b"))
q1_Suchmaschine <-  q1_Suchmaschine %>% rename(google = X01_1, bing = X01_2, yahoo = X01_3, search.ch = X01_4 , escosia.org = X01_5, Andere =X01_6, TextAndere = X01b)

levels = c("nie", "selten", "monatlich","wöchentlich","nahezu täglich")
q1_SuchmaschineAngepasst <- q1_Suchmaschine %>% subset(select =c(Nummer, google, bing, yahoo, search.ch, escosia.org, Andere)) %>% 
  mutate(
    google = case_when(
      google == 1 ~ levels[1], 
      google == 2 ~ levels[2], 
      google == 3 ~ levels[3],
      google == 4 ~ levels[4],
      google == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    bing = case_when(
      bing == 1 ~ levels[1], 
      bing == 2 ~ levels[2], 
      bing == 3 ~ levels[3],
      bing == 4 ~ levels[4],
      bing == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    yahoo = case_when(
      yahoo == 1 ~ levels[1], 
      yahoo == 2 ~ levels[2], 
      yahoo == 3 ~ levels[3],
      yahoo == 4 ~ levels[4],
      yahoo == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    search.ch = case_when(
      search.ch == 1 ~ levels[1], 
      search.ch == 2 ~ levels[2], 
      search.ch == 3 ~ levels[3],
      search.ch == 4 ~ levels[4],
      search.ch == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    escosia.org = case_when(
      escosia.org == 1 ~ levels[1], 
      escosia.org == 2 ~ levels[2], 
      escosia.org == 3 ~ levels[3],
      escosia.org == 4 ~ levels[4],
      escosia.org == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Andere = case_when(
      Andere == 1 ~ levels[1], 
      Andere == 2 ~ levels[2], 
      Andere == 3 ~ levels[3],
      Andere == 4 ~ levels[4],
      Andere == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q1_SuchmaschineAngepasst <- q1_SuchmaschineAngepasst %>% gather(google, bing, yahoo, search.ch, escosia.org, Andere, key = Suchmaschine, value = Frequenz)

q1_SuchmaschineAngepasst$Suchmaschine <- as.factor(q1_SuchmaschineAngepasst$Suchmaschine)
q1_SuchmaschineAngepasst$Frequenz <- as.factor(q1_SuchmaschineAngepasst$Frequenz)
q1_SuchmaschineAngepasst$Suchmaschine <- factor(q1_SuchmaschineAngepasst$Suchmaschine, levels = c("google", "bing", "yahoo","search.ch","escosia.org", "Andere"))
q1_SuchmaschineAngepasst$Frequenz <- factor(q1_SuchmaschineAngepasst$Frequenz, levels = c("nie", "selten", "monatlich","wöchentlich","nahezu täglich"))


q1_SuchmaschineJoined <- left_join(q1_SuchmaschineAngepasst,q23_Geschlecht, by = "Nummer")
q1_SuchmaschineJoined <- left_join(q1_SuchmaschineJoined, q24_Alter, by = "Nummer")

q1_SuchmaschineAngepasst %>% ggplot(aes(x=Frequenz, fill= Suchmaschine)) + geom_bar(position = "dodge2") + xlab("Frequenz") + ylab("Anzahl") + coord_flip()


q1_SuchmaschineTexte <-  q1_Suchmaschine %>% subset(select = c(TextAndere))
q1_SuchmaschineTexte$TextAndere <- str_trim(q1_SuchmaschineTexte$TextAndere)

q1_SuchmaschineTexte <-  q1_SuchmaschineTexte %>% filter(q1_SuchmaschineTexte$TextAndere != "")
AndereTexte <- data.frame(Andere = q1_SuchmaschineTexte)

knitr::kable(
  AndereTexte, longtable = TRUE, booktabs = TRUE
)

q1_lageparameter <- getLageparameter(subset(q1_Suchmaschine, select = c(google, bing, yahoo, search.ch, escosia.org, Andere)), levels(q1_SuchmaschineJoined$Suchmaschine), c(1:6))

knitr::kable(
  q1_lageparameter, longtable = TRUE, booktabs = TRUE
)

q1_frequenz <- getfrequenz(subset(q1_Suchmaschine, select = c(google, bing, yahoo, search.ch, escosia.org, Andere)),5,levels(q1_SuchmaschineJoined$Suchmaschine), c(
  "nie",
  "selten",
  "monatlich", 
  "wöchentlich",
  "nahezu täglich"), c(1:6), TRUE)

knitr::kable(
  q1_frequenz, longtable = TRUE, booktabs = TRUE
)

q1_SuchmaschineJoined %>% ggplot(aes(x=Frequenz, fill= Suchmaschine)) + geom_bar(aes(y =(..count..) / sum(..count..)), position = "dodge2") + 
  scale_y_continuous(labels = scales::percent)+ xlab("Frequenz") + ylab("Anzahl") + facet_wrap(.~AlterText, nrow=4, scales = "free")  + coord_flip()

q1_SuchmaschineJoined %>% ggplot(aes(x=Frequenz, fill= Suchmaschine)) + geom_bar(aes(y =(..count..) / sum(..count..)), position = "dodge2") + 
  stat_count(mapping = aes(x=Frequenz, y=..prop.., fill= Suchmaschine))+ xlab("Frequenz") + ylab("Anzahl") + facet_wrap(.~AlterText)  + coord_flip()

q1_SuchmaschineJoined %>% ggplot(aes(x=Frequenz, fill= Suchmaschine)) + geom_bar(aes(y =(..count..) / sum(..count..)), position = "dodge2") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) + xlab("Frequenz") + ylab("Anzahl") + facet_wrap(.~AlterText)  + coord_flip()



q1_SuchmaschineJoined %>% ggplot(aes(x=Frequenz, fill= Suchmaschine)) + geom_bar(position = "dodge2") + 
 xlab("Frequenz") + ylab("Anzahl") + facet_wrap(.~AlterText, nrow=4, ncol = 2, scales = "free", dir = "h")  + coord_flip()

q1_SuchmaschineJoined %>% ggplot(aes(x=Frequenz, fill= Suchmaschine)) + geom_bar(position = "dodge2") + 
  xlab("Frequenz") + ylab("Anzahl") + facet_wrap(.~AlterText, nrow=3, ncol = 3, scales = "free", as.table = TRUE )  + coord_flip()

