library(dplyr)
library(tidyverse)
library(magrittr)
library(knitr)
library(dplyr)
library(stringr)
library(plyr)

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

JoinTableAlterAndGeschlecht <- function(x){
  newtable <- left_join(x,q23_Geschlecht, by = "Nummer")
  newtable <- left_join(newtable, q24_Alter, by = "Nummer")
  newtable <- newtable %>% filter(!is.na(Geschlecht))
  newtable <- newtable %>% filter(!is.na(Alter))
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
    if(TotalAnzeigen){
      resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "0.00% (0)")
    }
    else
    {
      resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "(0)")
    }
  }
  
  if(TotalAnzeigen){
    resultDataframe <- resultDataframe %>% mutate(Summe = "(0)")
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
          
          if(TotalAnzeigen){
            resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                     paste(as.character(numbers[val,5]),"% ", sep =""), ""),  "(",as.character(numbers[val,3]), ")", sep ="")
            summeItemsw <- summeItemsw + as.numeric(numbers[val,3])
          }
          else
          { 
            rownumber <- correctRow
            resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                     paste(as.character(numbers[val,4]),"% ", sep =""), ""), "(",as.character(numbers[val,3]), ")", sep ="")
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
}

getfrequenz1DimensionGeschlecht <- function(x, Rowfactors){
  numberofColumns <- ncol(x)
  numberoflevels <- nlevels(Rowfactors)
  itemsoflevels <- c(1:numberoflevels)
  
  numberoflevels2 <- numberoflevels
  items <- c(1:numberoflevels2)

  
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
  

  resultDataframe <- resultDataframe %>% mutate(Antwortanzahl= "0.00% (0)")

  
  numbers <- transform(as.data.frame(table(x)), 
                       percentagem = round(Freq/length(which(!is.na(x[1]) & x$Geschlecht == 1))*100, digits = 2),
                       percentagew = round(Freq/length(which(!is.na(x[1]) & x$Geschlecht == 2))*100, digits = 2))
  

  numbers[,1] <- as.numeric(as.character(numbers[,1]))
  numbers[,2] <- as.numeric(as.character(numbers[,2]))
  
  numbersm<- numbers[numbers$Geschlecht == 1,]
  numbersw<- numbers[numbers$Geschlecht == 2,]
  
  summeItemsm <- 0
  summeItemsw <- 0
  correctRow <- 1
  for(val in items)
  {
    columnnumber <- 4
    
    if(!is.na(numbersm[val,1]))
    { 
      rownumber <- correctRow
      resultDataframe[rownumber,columnnumber] <-paste(as.character(numbersm[val,4]),"% ", "(",as.character(numbersm[val,3]), ")", sep ="")
      summeItemsm <- summeItemsm + as.numeric(numbersm[val,3])
    }
    
    if(!is.na(numbersw[val,1]))
    {
      rownumber <- correctRow + 1
              resultDataframe[rownumber,columnnumber] <- paste(as.character(numbersw[val,5]),"% ", "(",as.character(numbersw[val,3]), ")", sep ="")
        summeItemsw <- summeItemsw + as.numeric(numbersw[val,3])
      }

    correctRow <- correctRow + 2
    
    }

  resultDataframe[correctRow,1] <- c("Total")
    resultDataframe[correctRow,4] <- paste("(",summeItemsm, ")", sep ="")
    
    correctRow <- correctRow + 1
    resultDataframe[correctRow,1] <- c("Total")
    resultDataframe[correctRow,4] <- paste("(",summeItemsw, ")", sep ="")

  
  return(resultDataframe)
}

getfrequenz1DimensionAlter <- function(x, Rowfactors){
  numberofColumns <- ncol(x)
  numberoflevels <- nlevels(Rowfactors)
  itemsoflevels <- c(1:numberoflevels)
  
  numberoflevels2 <- numberoflevels
  items <- c(1:numberoflevels2)
  
  
  resultDataframe <- data.frame()
  
  
  rownumber <- 0
  nummerforItem <- 0
  for(itemNumber in itemsoflevels){
    rownumber <- rownumber + 1
    nummerforItem <- nummerforItem +1
    
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- Rowfactors[itemNumber]
    resultDataframe[rownumber,3] <- c("18-25")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- Rowfactors[itemNumber]
    resultDataframe[rownumber,3] <- c("26-32")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- Rowfactors[itemNumber]
    resultDataframe[rownumber,3] <- c("33-40")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- Rowfactors[itemNumber]
    resultDataframe[rownumber,3] <- c("41-55")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- Rowfactors[itemNumber]
    resultDataframe[rownumber,3] <- c("56-65")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- Rowfactors[itemNumber]
    resultDataframe[rownumber,3] <- c("über 65")
  }
  
  resultDataframe <- resultDataframe %>% rename(
    Nr = V1,
    Feld = V2,
    Auspraegung = V3)
  
  
  resultDataframe <- resultDataframe %>% mutate(Antwortanzahl= "0.00% (0)")
  
  
  numbers <- transform(as.data.frame(table(x)), 
                       percentage18 = round(Freq/length(which(!is.na(x[1]) & x$Alter == 2))*100, digits = 2),
                       percentage26 = round(Freq/length(which(!is.na(x[1]) & x$Alter == 3))*100, digits = 2),
                       percentage33 = round(Freq/length(which(!is.na(x[1]) & x$Alter == 4))*100, digits = 2),
                       percentage41 = round(Freq/length(which(!is.na(x[1]) & x$Alter == 5))*100, digits = 2),
                       percentage55 = round(Freq/length(which(!is.na(x[1]) & x$Alter == 6))*100, digits = 2),
                       percentage65 = round(Freq/length(which(!is.na(x[1]) & x$Alter == 7))*100, digits = 2))

  numbers[,1] <- as.numeric(as.character(numbers[,1]))
  numbers[,2] <- as.numeric(as.character(numbers[,2]))
  
  numbers18<- numbers[numbers$Alter == 2,]
  numbers26<- numbers[numbers$Alter == 3,]
  numbers33<- numbers[numbers$Alter == 4,]
  numbers41<- numbers[numbers$Alter == 5,]
  numbers55<- numbers[numbers$Alter == 6,]
  numbers65<- numbers[numbers$Alter == 7,]
  
  summeItems18 <- 0
  summeItems26 <- 0
  summeItems33 <- 0
  summeItems41 <- 0
  summeItems55 <- 0
  summeItems65 <- 0
  correctRow <- 1
  for(val in items)
  {
    columnnumber <- 4
    
    if(!is.na(numbers18[val,1]))
    { 
      rownumber <- correctRow
      resultDataframe[rownumber,columnnumber] <-paste(as.character(numbers18[val,4]),"% ", "(",as.character(numbers18[val,3]), ")", sep ="")
      summeItems18 <- summeItems18 + as.numeric(numbers18[val,3])
    }
    
    if(!is.na(numbers26[val,1]))
    {
      rownumber <- correctRow + 1
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers26[val,5]),"% ", "(",as.character(numbers26[val,3]), ")", sep ="")
      summeItems26 <- summeItems26 + as.numeric(numbers26[val,3])
    }
    
    if(!is.na(numbers33[val,1]))
    {
      rownumber <- correctRow + 2
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers33[val,6]),"% ", "(",as.character(numbers33[val,3]), ")", sep ="")
      summeItems33 <- summeItems33 + as.numeric(numbers33[val,3])
    }
    
    if(!is.na(numbers41[val,1]))
    {
      rownumber <- correctRow + 3
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers41[val,7]),"% ", "(",as.character(numbers41[val,3]), ")", sep ="")
      summeItems41 <- summeItems41 + as.numeric(numbers41[val,3])
    }
    
    if(!is.na(numbers55[val,1]))
    {
      rownumber <- correctRow + 4
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers55[val,8]),"% ", "(",as.character(numbers55[val,3]), ")", sep ="")
      summeItems55 <- summeItems55 + as.numeric(numbers55[val,3])
    }
    
    if(!is.na(numbers65[val,1]))
    {
      rownumber <- correctRow + 5
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers65[val,9]),"% ", "(",as.character(numbers65[val,3]), ")", sep ="")
      summeItems65 <- summeItems65 + as.numeric(numbers65[val,3])
    }
    
    correctRow <- correctRow + 6
    
  }
  
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,4] <- paste("(",summeItems18, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,4] <- paste("(",summeItems26, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,4] <- paste("(",summeItems33, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,4] <- paste("(",summeItems41, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,4] <- paste("(",summeItems55, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,4] <- paste("(",summeItems65, ")", sep ="")
  
  return(resultDataframe)
}

JoinTableAlterAndGeschlecht <- function(x){
  newtable <- left_join(x,q23_Geschlecht, by = "Nummer")
  newtable <- left_join(newtable, q24_Alter, by = "Nummer")
  newtable <- newtable %>% filter(!is.na(Geschlecht))
  newtable <- newtable %>% filter(!is.na(Alter))
  return(newtable)
}

JoinTableGeschlecht <- function(x){
  geschlectfiltertabe <- q23_Geschlecht %>% subset(select = c(Nummer,Geschlecht))
  newtable <- left_join(x,geschlectfiltertabe, by = "Nummer")
  newtable <- newtable %>% filter(!is.na(Geschlecht))
  newtable <- subset(newtable, select = -c(Nummer))
  return(newtable)
}

JoinTableAlter <- function(x){
  alterfiltertabe <- q24_Alter %>% subset(select = c(Nummer,Alter))
  newtable <- left_join(x,alterfiltertabe, by = "Nummer")
  newtable <- newtable %>% filter(!is.na(Alter))
  newtable <- subset(newtable, select = -c(Nummer))
  return(newtable)
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


q2_bezahlteLinks <- subset(dat, select=c("Nummer", "X2"))
q2_bezahlteLinks <- q2_bezahlteLinks %>% rename(ZweiteSeite = X2)

levels2 = c("0 bis 20%", "21 bis 40%", "41 bis 60%","61 bis 80%","81 bis 100%")
levels = addNA(levels2)

q2_bezahlteLinksAngepasst <- q2_bezahlteLinks  %>% subset(select =c(Nummer, ZweiteSeite)) %>% filter(!is.na(ZweiteSeite)) %>% 
  mutate(
    ZweiteSeite = case_when(
      ZweiteSeite == 1 ~ levels[1], 
      ZweiteSeite == 2 ~ levels[2], 
      ZweiteSeite == 3 ~ levels[3],
      ZweiteSeite == 4 ~ levels[4],
      ZweiteSeite == 5 ~ levels[5],
      TRUE ~  levels[6]) %>% factor(levels))

q2_bezahlteLinksAngepasst <- q2_bezahlteLinksAngepasst %>% filter(!is.na(ZweiteSeite))

q2_bezahlteLinksAngepasst %>% ggplot(aes(x=ZweiteSeite)) + geom_bar(fill=colorSingleBar) + xlab("Prozent") + ylab("Anzahl") + coord_flip()

q2_lageparameter <- getLageparameter1Dimensional(subset(q2_bezahlteLinks, select = c(ZweiteSeite)), "Zweite Seite")

knitr::kable(
  q2_lageparameter, longtable = TRUE, booktabs = TRUE
)

q2_frequenz <- getfrequenz1Dimension(subset(q2_bezahlteLinks, select = c(ZweiteSeite)),5, levels2, c(1:5))

knitr::kable(
  q2_frequenz, longtable = TRUE, booktabs = TRUE
)

q2_bezahlteLinksJoined <- JoinTableAlterAndGeschlecht(q2_bezahlteLinksAngepasst)

q2_bezahlteLinksJoined %>% ggplot(aes(x=ZweiteSeite)) + geom_bar(fill=colorSingleBar) + xlab("Prozent") + ylab("Anzahl") + facet_wrap(.~AlterText, nrow=3, ncol = 2, scales = "free", dir = "h")  + coord_flip()


q2_bezahlteLinksJoinedGeschlecht <- JoinTableGeschlecht(q2_bezahlteLinks)
q_frequenzGeschlecht <- getfrequenz1DimensionGeschlecht(q2_bezahlteLinksJoinedGeschlecht,factor(levels2))

q2_bezahlteLinksJoinedAlter <- JoinTableAlter(q2_bezahlteLinks)
q_frequenzAlter <- getfrequenz1DimensionAlter(q2_bezahlteLinksJoinedAlter,factor(levels2))