library(dplyr)
library(tidyverse)
library(magrittr)
library(knitr)
library(dplyr)
library(stringr)

colorSingleBar <- "steelblue"
colorMan <-"#0703f0"
colorWoman <- #c74dc2"
  colorGenderNot <- #85796f
  colorsGender <- c("#0703f0","#c74dc2", "#85796f")  

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

JoinTableGeschlecht <- function(x){
  geschlectfiltertabe <- q23_Geschlecht %>% subset(select = c(Nummer,Geschlecht))
  newtable <- left_join(x,geschlectfiltertabe, by = "Nummer")
  newtable <- newtable %>% filter(!is.na(Geschlecht))
  return(newtable)
}

getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getfrequenzgeschlecht <- function(x, Rowfactors, ColumnNames, TotalAnzeigen){
  numberofColumns <- ncol(x)
  numberoflevels <- nlevels(Rowfactors)
  itemsoflevels <- c(1:numberoflevels)
  
  numberoflevels2 <- numberoflevels * 2
  items <- c(1:numberoflevels2)
  
  numberofColumnNames = length(ColumnNames)
  itemscolums <- c(1: numberofColumnNames)
  

  resultDataframe <- data.frame()
  
  rownumber <- 0
  nummerforItem <- 0
  for(itemNumber in itemsoflevels){
    rownumber <- rownumber + 1
    nummerforItem <- nummerforItem +1
    
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- Rowfactors[itemNumber]
    resultDataframe[rownumber,3] <- c("männlich")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- Rowfactors[itemNumber]
    resultDataframe[rownumber,3] <- c("weiblich")
  }
  
  resultDataframe <- resultDataframe %>% rename(
    Nr = V1,
    Feld = V2,
    Auspraegung = V3)
  
  for(itemNumber in itemscolums){
  columnname = ColumnNames[itemNumber]
  resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "0.00% (0)")
  }
   if(TotalAnzeigen){
    resultDataframe <- resultDataframe %>% mutate(Summe = "0")
  }

  correctRow <- 1
  for(numberofCurrentColumn in itemsoflevels){

    numbers <- transform(as.data.frame(table(x[,numberofCurrentColumn], x$Geschlecht)), 
                         percentagem = round(Freq/length(which(!is.na(x[,numberofCurrentColumn]) & x$Geschlecht == 1))*100, digits = 2),
                         percentagew = round(Freq/length(which(!is.na(x[,numberofCurrentColumn]) & x$Geschlecht == 2))*100, digits = 2))
    
    numbers$Var1 <- as.numeric(as.character(numbers$Var1))
    numbers$Var2 <- as.numeric(as.character(numbers$Var2))
    
    numbers<- numbers[order(numbers$Var1,numbers$Var2),]
    
    summeItemsm <- 0
    summeItemsw <- 0
    for(val in items)
    {
      if(!is.na(numbers[val,1]))
      {
        columnnumber <- as.numeric(numbers[val,1]) +3
        rownumber <- 0
        if(as.numeric(numbers[val,2]) %% 2 == 0)
          {
          rownumber <- correctRow + 1
          resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers[val,5]), "% (",as.character(numbers[val,3]), ")", sep ="")
          summeItemsw <- summeItemsw + as.numeric(numbers[val,3])
          }
        else
        { 
          rownumber <- correctRow
          resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers[val,4]), "% (",as.character(numbers[val,3]), ")", sep ="")
          summeItemsm <- summeItemsm + as.numeric(numbers[val,3])
          }



      }
      if(TotalAnzeigen){
        resultDataframe[correctRow,numberofColumnNames +4] <- summeItemsm
        resultDataframe[correctRow+1,numberofColumnNames +4] <- summeItemsw
      }
    }
    
    correctRow <- correctRow + 2
  }

  return(resultDataframe)
  }


q23_Geschlecht <- subset(dat, select=c("Nummer", "X23"))
q23_Geschlecht <- q23_Geschlecht %>% rename(Geschlecht = X23)
levels2 = c("männlich", "weiblich")
levels = addNA(levels2)

q23_Geschlecht <- q23_Geschlecht %>% mutate(GeschlechtText = case_when(
  Geschlecht == 1 ~ levels[1], 
  Geschlecht == 2 ~ levels[2], 
  TRUE ~  levels[3]) %>% factor(levels))

q23_Geschlecht <- q23_Geschlecht %>% filter(!is.na(GeschlechtText))

q1_Suchmaschine <-  subset(dat, select=c("Nummer", "X01_1", "X01_2", "X01_3", "X01_4", "X01_5", "X01_6", "X01b"))
q1_Suchmaschine <-  q1_Suchmaschine %>% rename(google = X01_1, bing = X01_2, yahoo = X01_3, search.ch = X01_4 , escosia.org = X01_5, Andere =X01_6, TextAndere = X01b)

levels2 = c("nie", "selten", "monatlich","wöchentlich","nahezu täglich")
levels = addNA(levels2)

q1_SuchmaschineAngepasst <- q1_Suchmaschine %>% gather(google, bing, yahoo, search.ch, escosia.org, Andere, key = Suchmaschine, value = Frequenz)

q1_SuchmaschineAngepasst <- q1_SuchmaschineAngepasst %>% filter(!is.na(Frequenz))

q1_SuchmaschineAngepasst$Suchmaschine <- as.factor(q1_SuchmaschineAngepasst$Suchmaschine)
q1_SuchmaschineAngepasst$Frequenz <- as.factor(q1_SuchmaschineAngepasst$Frequenz)
q1_SuchmaschineAngepasst$Suchmaschine <- factor(q1_SuchmaschineAngepasst$Suchmaschine, levels = c("google", "bing", "yahoo","search.ch","escosia.org", "Andere"))
q1_SuchmaschineAngepasst$Frequenz <- factor(q1_SuchmaschineAngepasst$Frequenz, levels = c("nie", "selten", "monatlich","wöchentlich","nahezu täglich"))


q1_SuchmaschineJoined <- JoinTableAlterAndGeschlecht(q1_SuchmaschineAngepasst)

test <- JoinTableGeschlecht(q1_Suchmaschine %>% subset(select =c(Nummer, google, bing, yahoo, search.ch, escosia.org, Andere)))
as <- getfrequenzgeschlecht(test %>%
                              subset(select =c(google, bing, yahoo, search.ch, escosia.org, Andere, Geschlecht)), 
                            factor(c("google", "bing", "yahoo", "search.ch", "esosia.org", "Andere")), 
                            c("nie", "selten", "monatlich","wöchentlich","nahezu täglich"), TRUE)