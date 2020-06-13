library(tidyverse)
dat <- read.csv2('D:\\Desktop\\BIT\\Bachelor\\06_Auswertung\\Angepasste Tabelle.csv')
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

q23_Geschlecht <- subset(dat, select=c("Nummer", "X23"))
q23_Geschlecht <- q23_Geschlecht %>% rename(Geschlecht = X23)
levels = c("männlich", "weiblich", "Keine Auswahl")
q23_Geschlecht <- q23_Geschlecht %>% mutate(GeschlechtText = case_when(Geschlecht == 1 ~ levels[1], Geschlecht == 2 ~ levels[2], TRUE ~  levels[3]) %>% factor(levels))


q24_Alter <- subset(dat, select=c("Nummer", "X24"))
q24_Alter <- q24_Alter %>% rename(Alter = X24)

levels = c("unter 18 Jahre", "18-25", "26-32","33-40","41-55","56-65","über 65 Jahre", "Keine Angabe")
q24_Alter <- q24_Alter %>% mutate(AlterText = case_when(
  Alter == 1 ~ levels[1], 
  Alter == 2 ~ levels[2], 
  Alter == 3 ~ levels[3],
  Alter == 4 ~ levels[4],
  Alter == 5 ~ levels[5],
  Alter == 6 ~ levels[6],
  Alter == 7 ~ levels[7],
  TRUE ~  levels[8]) %>% factor(levels))


q1_SuchmaschineJoined <- left_join(q1_SuchmaschineAngepasst,q23_Geschlecht, by = "Nummer")
q1_SuchmaschineJoined <- left_join(q1_SuchmaschineJoined, q24_Alter, by = "Nummer")
q24_AlterJoined <- left_join(q24_Alter,q23_Geschlecht, by = "Nummer")









getfrequenzCompared <- function(x, NumberOfItems, RowNamesLevels, NumberOfItemsCompared, RowNamesLevelsCompared, ColumnNames, itemNumbers){
  numberofColumns <- ncol(x) -1
  numberofRows <-  nrow(x)
  items <- c(1:NumberOfItems)
  itemscolums <- c(1: numberofColumns)
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels)
  
  for(itemNumber in items){
    columnname = ColumnNames[itemNumber]
    resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "0.00% (0)")
  }
  
  resultDataframe <- resultDataframe %>% mutate(Summe = "0")
  
  for(numberofCurrentColumn in itemscolums){
    
    numbers <- transform(as.data.frame(table(x[numberofCurrentColumn])), percentage = round(Freq/nrow(x)*100, digits = 2))
    numbers$Var1 <- as.numeric(as.character(numbers$Var1))
    
    summeItems <- 0
    for(val in items)
    {
      if(!is.na(numbers[val,1]))
      {
        columnnumber <- as.numeric(numbers[val,1]) +2
        rownumber <- numberofCurrentColumn
        
        resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers[val,3]), "%\t (",as.character(numbers[val,2]), ")", sep ="")
        
        summeItems <- summeItems + as.numeric(numbers[val,2])
      }
      resultDataframe[numberofCurrentColumn,numberofColumns +2] <- summeItems
    }
  }
  
  return(resultDataframe)
}

q1_frequenzalter <- getfrequenzCompared(subset(q1_SuchmaschineNormalJoined, select = c(google, bing, yahoo, search.ch, escosia.org, Andere, Alter)),5,levels(q1_SuchmaschineJoined$Suchmaschine), 7, levels(q1_SuchmaschineJoined$AlterText), c(
  "nie",
  "selten",
  "monatlich", 
  "wöchentlich",
  "nahezu täglich"), c(1:6))

q1_frequenzalter
