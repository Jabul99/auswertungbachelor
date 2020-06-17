library(plyr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(knitr)
library(stringr)
options(knitr.kable.NA = '')

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
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
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
    
    numbersm<- numbers[numbers$Var2 == 1,]
    numbersw<- numbers[numbers$Var2 == 2,]
    
    summeItemsm <- 0
    summeItemsw <- 0
    for(val in itemscolums)
    {
      columnnumber <- as.numeric(numbers[val,1]) +3
      
      if(!is.na(numbersm[val,1]))
      { 
        rownumber <- correctRow
        resultDataframe[rownumber,columnnumber] <-
        paste(if_else(TotalAnzeigen, 
                         paste(as.character(numbersm[val,4]),"% ", sep =""), ""),  "(",as.character(numbersm[val,3]), ")", sep ="")

        summeItemsm <- summeItemsm + as.numeric(numbersm[val,3])
      }
      
      if(!is.na(numbersw[val,1]))
      { 
        rownumber <- correctRow + 1
        resultDataframe[rownumber,columnnumber] <-
        paste(if_else(TotalAnzeigen, 
                      paste(as.character(numbersw[val,5]),"% ", sep =""), ""),  "(",as.character(numbersw[val,3]), ")", sep ="")
        
        summeItemsw <- summeItemsw + as.numeric(numbersw[val,3])
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

getfrequenzAlter <- function(x, Rowfactors, ColumnNames, TotalAnzeigen){
  numberofColumns <- ncol(x)
  numberoflevels <- nlevels(Rowfactors)
  itemsoflevels <- c(1:numberoflevels)
  
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
    resultDataframe[rownumber,3] <- c("18-25")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("26-32")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("33-40")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("41-55")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("56-65")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("über 65")
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
    else{
      resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "(0)")
    }
  }
  if(TotalAnzeigen){
    resultDataframe <- resultDataframe %>% mutate(Summe = "0")
  }
  
  correctRow <- 1
  for(numberofCurrentColumn in itemsoflevels){
    
    numbers <- transform(as.data.frame(table(x[,numberofCurrentColumn], x$Alter)), 
                         percentage18 = round(Freq/length(which(!is.na(x[,numberofCurrentColumn]) & x$Alter == 2))*100, digits = 2),
                         percentage26 = round(Freq/length(which(!is.na(x[,numberofCurrentColumn]) & x$Alter == 3))*100, digits = 2),
                         percentage33 = round(Freq/length(which(!is.na(x[,numberofCurrentColumn]) & x$Alter == 4))*100, digits = 2),
                         percentage41 = round(Freq/length(which(!is.na(x[,numberofCurrentColumn]) & x$Alter == 5))*100, digits = 2),
                         percentage55 = round(Freq/length(which(!is.na(x[,numberofCurrentColumn]) & x$Alter == 6))*100, digits = 2),
                         percentage65 = round(Freq/length(which(!is.na(x[,numberofCurrentColumn]) & x$Alter == 7))*100, digits = 2))
    
    numbers$Var1 <- as.numeric(as.character(numbers$Var1))
    numbers$Var2 <- as.numeric(as.character(numbers$Var2))
    
    numbers18<- numbers[numbers$Var2 == 2,]
    numbers26<- numbers[numbers$Var2 == 3,]
    numbers33<- numbers[numbers$Var2 == 4,]
    numbers41<- numbers[numbers$Var2 == 5,]
    numbers55<- numbers[numbers$Var2 == 6,]
    numbers65<- numbers[numbers$Var2 == 7,]
    
    summeItems18 <- 0
    summeItems26 <- 0
    summeItems33 <- 0
    summeItems41 <- 0
    summeItems55 <- 0
    summeItems65 <- 0
    for(val in itemscolums)
    {
      if(!is.na(numbers[val,1]))
      {
        columnnumber <- as.numeric(numbers[val,1]) +3
        rownumber <- 0
        
        if(!is.na(numbers18[val,1]) & !is.na(numbers18[val,3]))
        { 
          rownumber <- correctRow
          resultDataframe[rownumber,columnnumber] <-paste(if_else(TotalAnzeigen, 
                                                                  paste(as.character(numbers18[val,4]),"% ", sep =""), ""), "(",as.character(numbers18[val,3]), ")", sep ="")
          summeItems18 <- summeItems18 + as.numeric(numbers18[val,3])
        }

        if(!is.na(numbers26[val,1]) & !is.na(numbers26[val,3]))
        {
          rownumber <- correctRow + 1
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers26[val,5]),"% ", sep =""), ""), "(",as.character(numbers26[val,3]), ")", sep ="")
          summeItems26 <- summeItems26 + as.numeric(numbers26[val,3])
        }
        
        if(!is.na(numbers33[val,1]) & !is.na(numbers33[val,3]))
        {
          rownumber <- correctRow + 2
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers33[val,6]),"% ", sep =""), ""), "(",as.character(numbers33[val,3]), ")", sep ="")
          summeItems33 <- summeItems33 + as.numeric(numbers33[val,3])
        }
        
        if(!is.na(numbers41[val,1]) & !is.na(numbers41[val,3]))
        {
          rownumber <- correctRow + 3
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers41[val,7]),"% ", sep =""), ""), "(",as.character(numbers41[val,3]), ")", sep ="")
          summeItems41 <- summeItems41 + as.numeric(numbers41[val,3])
        }
        
        if(!is.na(numbers55[val,1]) & !is.na(numbers55[val,3]))
        {
          rownumber <- correctRow + 4
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers55[val,8]),"% ", sep =""), ""), "(",as.character(numbers55[val,3]), ")", sep ="")
          summeItems55 <- summeItems55 + as.numeric(numbers55[val,3])
        }
        
        if(!is.na(numbers65[val,1]) & !is.na(numbers65[val,3]))
        {
          rownumber <- correctRow + 5
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers65[val,9]),"% ", sep =""), ""), "(",as.character(numbers65[val,3]), ")", sep ="")
          summeItems65 <- summeItems65 + as.numeric(numbers65[val,3])
        }
        
      }
      if(TotalAnzeigen){
        resultDataframe[correctRow,numberofColumnNames +4] <- summeItems18
        resultDataframe[correctRow+1,numberofColumnNames +4] <- summeItems26
        resultDataframe[correctRow+2,numberofColumnNames +4] <- summeItems33
        resultDataframe[correctRow+3,numberofColumnNames +4] <- summeItems41
        resultDataframe[correctRow+4,numberofColumnNames +4] <- summeItems55
        resultDataframe[correctRow+5,numberofColumnNames +4] <- summeItems65
      }
    }
    
    correctRow <- correctRow + 6
  }
  
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


q7_SucheProduktSozialMedia <- subset(dat, select=c("Nummer", "X07_1", "X07_2"))
q7_SucheProduktSozialMedia <- q7_SucheProduktSozialMedia %>% rename(privat = X07_1, geschaeftlich = X07_2)

levels2 = c("nie", "eher nicht", "vielleicht","ziemlich wahrscheinlich","ganz sicher")
levels = addNA(levels2)
q7_SucheProduktSozialMediaAngepasst <- q7_SucheProduktSozialMedia %>% subset(select = c(Nummer, privat, geschaeftlich)) %>% 
  mutate(
    privat = case_when(
      privat == 1 ~ levels[1], 
      privat == 2 ~ levels[2], 
      privat == 3 ~ levels[3],
      privat == 4 ~ levels[4],
      privat == 5 ~ levels[5],
      TRUE ~  levels[6]) %>% factor(levels),
    geschaeftlich = case_when(
      geschaeftlich == 1 ~ levels[1], 
      geschaeftlich == 2 ~ levels[2], 
      geschaeftlich == 3 ~ levels[3],
      geschaeftlich == 4 ~ levels[4],
      geschaeftlich == 5 ~ levels[5],
      TRUE ~  levels[6]) %>% factor(levels))

q7_SucheProduktSozialMediaAngepasst <- q7_SucheProduktSozialMediaAngepasst %>% gather(privat, geschaeftlich, key = Art, value = Frequenz)


q7_SucheProduktSozialMediaAngepasst <- q7_SucheProduktSozialMediaAngepasst %>% filter(!is.na(Frequenz))

q7_SucheProduktSozialMediaAngepasst$Art <- as.factor(q7_SucheProduktSozialMediaAngepasst$Art)
q7_SucheProduktSozialMediaAngepasst$Frequenz <- as.factor(q7_SucheProduktSozialMediaAngepasst$Frequenz)
q7_SucheProduktSozialMediaAngepasst$Art <- factor(q7_SucheProduktSozialMediaAngepasst$Art, levels = c("privat", "geschäftlich"))
q7_SucheProduktSozialMediaAngepasst$Frequenz <- factor(q7_SucheProduktSozialMediaAngepasst$Frequenz, levels = levels2)

q7_SucheProduktSozialMediaJoined <- JoinTableAlterAndGeschlecht(q7_SucheProduktSozialMediaAngepasst)


q7_SucheProduktSozialMediaAngepasst %>% ggplot(aes(x=Frequenz, fill= Art)) + geom_bar(position = "dodge2") + xlab("") + ylab("Anzahl") + coord_flip()

q7_lageparameter <- getLageparameter(subset(q7_SucheProduktSozialMedia, select = c(privat, geschaeftlich)), levels(q7_SucheProduktSozialMediaAngepasst$Art), c(1:2))

knitr::kable(
  q7_lageparameter, longtable = TRUE, booktabs = TRUE
)

q7_frequenz <- getfrequenz(subset(q7_SucheProduktSozialMedia, select = c(privat, geschaeftlich)),5,levels(q7_SucheProduktSozialMediaAngepasst$Art),levels2, c(1:2), TRUE)

knitr::kable(
  q7_frequenz, longtable = TRUE, booktabs = TRUE
)

frequenzfactors <- factor(c("privat", "geschäftlich"))

q7_SucheProduktSozialMediaJoined %>% ggplot(aes(x=Frequenz, fill= Art)) + geom_bar(position = "dodge2") + xlab("") + ylab("Anzahl") + facet_wrap(.~AlterText, nrow=3, ncol = 2, scales = "free", dir = "h")  + coord_flip()


q7_SucheProduktSozialMediaAlterJoined <- JoinTableAlter(q7_SucheProduktSozialMedia)

q_frequenzalter<- getfrequenzAlter(q7_SucheProduktSozialMediaAlterJoined, frequenzfactors, levels2, TRUE)

knitr::kable(
  q_frequenzalter, longtable = TRUE, booktabs = TRUE
)


q7_SucheProduktSozialMediaJoined %>% ggplot(aes(x=Frequenz, fill= Art)) + geom_bar(position = "dodge2") + xlab("") + ylab("Anzahl") + facet_wrap(.~GeschlechtText)  + coord_flip()


q7_SucheProduktSozialMediaJoinedGeschlecht <- JoinTableGeschlecht(q7_SucheProduktSozialMedia)

q_frequenzgeschlecht <- getfrequenzgeschlecht(q7_SucheProduktSozialMediaJoinedGeschlecht,frequenzfactors,levels2, TRUE)

knitr::kable(
  q_frequenzgeschlecht, longtable = TRUE, booktabs = TRUE
)

