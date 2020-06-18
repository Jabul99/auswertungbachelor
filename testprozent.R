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

getRowNumber <- function(CurrentRowNumber, CheckRowNumber, NumberOfCategorie, RowAddition)
{
  returnRow <- CurrentRowNumber
  
  if(CheckRowNumber < NumberOfCategorie)
  {
    numberOfAddition <- NumberOfCategorie - CheckRowNumber
    addition <- RowAddition * numberOfAddition
    
    returnRow <- returnRow + addition
  }
  return(returnRow)
}

getfrequenz <- function(x, NumberOfItems, RowNamesLevels, ColumnNames, itemNumbers, TotalAnzeigen){
  numberofColumns <- ncol(x)
  numberofRows <-  nrow(x)
  items <- c(1:NumberOfItems)
  itemscolums <- c(1: numberofColumns)
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels)
  
  for(itemNumber in items){
    columnname = ColumnNames[itemNumber]
    if(TotalAnzeigen){
      resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "0.00% (0)")
    }else
    {
      resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "(0)")
    }
  }
  
  if(TotalAnzeigen){
    resultDataframe <- resultDataframe %>% mutate(Summe = "0")
  }
  
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
        
        resultDataframe[rownumber,columnnumber] <-  paste(if_else(TotalAnzeigen, 
                                                                  paste(as.character(numbers[val,3]),"% ", sep =""), ""), "(",as.character(numbers[val,2]), ")", sep ="")
        
        summeItems <- summeItems + as.numeric(numbers[val,2])
      }
      if(TotalAnzeigen){
        resultDataframe[numberofCurrentColumn,NumberOfItems +3] <- summeItems
      }
    }
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
  checkRowNumber <- 1
  RowadditionCount <- 6
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
          rrownumber <- getRowNumber(correctRow, checkRowNumber, numbers18[val,1], RowadditionCount)
          resultDataframe[rownumber,columnnumber] <-paste(if_else(TotalAnzeigen, 
                                                                  paste(as.character(numbers18[val,4]),"% ", sep =""), ""), "(",as.character(numbers18[val,3]), ")", sep ="")
          summeItems18 <- summeItems18 + as.numeric(numbers18[val,3])
        }
        
        if(!is.na(numbers26[val,1]) & !is.na(numbers26[val,3]))
        {
          rownumber <- getRowNumber(correctRow, checkRowNumber, numbers26[val,1], RowadditionCount)+ 1
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers26[val,5]),"% ", sep =""), ""), "(",as.character(numbers26[val,3]), ")", sep ="")
          summeItems26 <- summeItems26 + as.numeric(numbers26[val,3])
        }
        
        if(!is.na(numbers33[val,1]) & !is.na(numbers33[val,3]))
        {
          rownumber <- getRowNumber(correctRow, checkRowNumber, numbers33[val,1], RowadditionCount)+ 2
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers33[val,6]),"% ", sep =""), ""), "(",as.character(numbers33[val,3]), ")", sep ="")
          summeItems33 <- summeItems33 + as.numeric(numbers33[val,3])
        }
        
        if(!is.na(numbers41[val,1]) & !is.na(numbers41[val,3]))
        {
          rownumber <- getRowNumber(correctRow, checkRowNumber, numbers41[val,1], RowadditionCount) + 3
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers41[val,7]),"% ", sep =""), ""), "(",as.character(numbers41[val,3]), ")", sep ="")
          summeItems41 <- summeItems41 + as.numeric(numbers41[val,3])
        }
        
        if(!is.na(numbers55[val,1]) & !is.na(numbers55[val,3]))
        {
          rownumber <- getRowNumber(correctRow, checkRowNumber, numbers55[val,1], RowadditionCount)+ 4
          resultDataframe[rownumber,columnnumber] <- paste(if_else(TotalAnzeigen, 
                                                                   paste(as.character(numbers55[val,8]),"% ", sep =""), ""), "(",as.character(numbers55[val,3]), ")", sep ="")
          summeItems55 <- summeItems55 + as.numeric(numbers55[val,3])
        }
        
        if(!is.na(numbers65[val,1]) & !is.na(numbers65[val,3]))
        {
          rownumber <- getRowNumber(correctRow, checkRowNumber, numbers65[val,1], RowadditionCount) + 5
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
    checkRowNumber <- checkRowNumber + 1
  }
  
  return(resultDataframe)
}

getfrequenz1Dimension <- function(x, NumberOfItems, RowNamesLevels, itemNumbers){
  items <- c(1:NumberOfItems)
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels)
  resultDataframe <- resultDataframe %>% mutate(Antwortanzahl= "0.00% (0)")
  
  numbers <- transform(as.data.frame(table(x[1])), percentage = round(Freq/nrow(x)*100, digits = 2))
  numbers$Var1 <- as.numeric(as.character(numbers$Var1))
  
  summeItems <- 0
  rownumber <- 0
  for(val in items)
  {
    if(!is.na(numbers[val,1]))
    {
      columnnumber <- 3
      rownumber <- as.numeric(numbers[val,1])
      
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers[val,3]), "% (",as.character(numbers[val,2]), ")", sep ="")
      
      summeItems <- summeItems + as.numeric(numbers[val,2])
    }
  }
  resultDataframe[rownumber+1,1] <- c("Total")
  
  resultDataframe[rownumber+1,3] <- paste("100", "% (",summeItems, ")", sep ="")
  
  return(resultDataframe)
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
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
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
  checkRowNumber <- 1
  RowadditionCount <- 2
  for(val in items)
  {
    columnnumber <- 4
    
    if(!is.na(numbersm[val,1]))
    { 
      rownumber <- getRowNumber(correctRow, checkRowNumber, numbersm[val,1], RowadditionCount)
      resultDataframe[rownumber,columnnumber] <-paste(as.character(numbersm[val,4]),"% ", "(",as.character(numbersm[val,3]), ")", sep ="")
      summeItemsm <- summeItemsm + as.numeric(numbersm[val,3])
    }
    
    if(!is.na(numbersw[val,1]))
    {
      rownumber <- getRowNumber(correctRow, checkRowNumber, numbersw[val,1], RowadditionCount) + 1
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbersw[val,5]),"% ", "(",as.character(numbersw[val,3]), ")", sep ="")
      summeItemsw <- summeItemsw + as.numeric(numbersw[val,3])
    }
    
    correctRow <- correctRow + RowadditionCount
    checkRowNumber <- checkRowNumber + 1
  }
  
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,3] <- c("männlich")
  resultDataframe[correctRow,4] <- paste("(",summeItemsm, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,3] <- c("weiblich")
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
  checkRowNumber <- 1
  RowadditionCount <- 6
  for(val in items)
  {
    columnnumber <- 4

    if(!is.na(numbers18[val,1]) & !is.na(numbers18[val,3]))
    { 
      rownumber <- getRowNumber(correctRow, checkRowNumber, numbers18[val,1], RowadditionCount)
      resultDataframe[rownumber,columnnumber] <-paste(as.character(numbers18[val,4]),"% ", "(",as.character(numbers18[val,3]), ")", sep ="")
      summeItems18 <- summeItems18 + as.numeric(numbers18[val,3])
    }
    
    if(!is.na(numbers26[val,1]) & !is.na(numbers26[val,3]))
    {
      rownumber <- getRowNumber(correctRow, checkRowNumber, numbers26[val,1], RowadditionCount) + 1
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers26[val,5]),"% ", "(",as.character(numbers26[val,3]), ")", sep ="")
      summeItems26 <- summeItems26 + as.numeric(numbers26[val,3])
    }
    
    if(!is.na(numbers33[val,1]) & !is.na(numbers33[val,3]))
    {
      rownumber <- getRowNumber(correctRow, checkRowNumber, numbers33[val,1], RowadditionCount) + 2
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers33[val,6]),"% ", "(",as.character(numbers33[val,3]), ")", sep ="")
      summeItems33 <- summeItems33 + as.numeric(numbers33[val,3])
    }
    
    if(!is.na(numbers41[val,1]) & !is.na(numbers41[val,3]))
    {
      rownumber <- getRowNumber(correctRow, checkRowNumber, numbers41[val,1], RowadditionCount) + 3
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers41[val,7]),"% ", "(",as.character(numbers41[val,3]), ")", sep ="")
      summeItems41 <- summeItems41 + as.numeric(numbers41[val,3])
    }
    
    if(!is.na(numbers55[val,1]) & !is.na(numbers55[val,3]))
    {
      rownumber <- getRowNumber(correctRow, checkRowNumber, numbers55[val,1], RowadditionCount) + 4
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers55[val,8]),"% ", "(",as.character(numbers55[val,3]), ")", sep ="")
      summeItems55 <- summeItems55 + as.numeric(numbers55[val,3])
    }
    
    if(!is.na(numbers65[val,1]) & !is.na(numbers65[val,3]))
    {
      rownumber <- getRowNumber(correctRow, checkRowNumber, numbers65[val,1], RowadditionCount) + 5
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers65[val,9]),"% ", "(",as.character(numbers65[val,3]), ")", sep ="")
      summeItems65 <- summeItems65 + as.numeric(numbers65[val,3])
    }
    
    checkRowNumber <- checkRowNumber +1
    correctRow <- correctRow + RowadditionCount
    
  }
  
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,3] <- c("18-25")
  resultDataframe[correctRow,4] <- paste("(",summeItems18, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,3] <- c("26-32")
  resultDataframe[correctRow,4] <- paste("(",summeItems26, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,3] <-  c("33-40")
  resultDataframe[correctRow,4] <- paste("(",summeItems33, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,3] <- c("41-55")
  resultDataframe[correctRow,4] <- paste("(",summeItems41, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,3] <- c("56-65")
  resultDataframe[correctRow,4] <- paste("(",summeItems55, ")", sep ="")
  
  correctRow <- correctRow + 1
  resultDataframe[correctRow,1] <- c("Total")
  resultDataframe[correctRow,3] <- c("über 65")
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


getLageparameter <- function(x, RowNamesLevels, itemNumbers){
  numberofColumns <-ncol(x)

  itemscolums <- c(1: numberofColumns)
  
  values <- as.vector(rep(0, numberofColumns))
  
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels, Minimum =values, Maximum=values, Modus=values, Mittelwert=values, Standardabweichung=values, Median=values)

  for(numberofCurrentColumn in itemscolums){
    
    resultDataframe[numberofCurrentColumn,3] <- min(x[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[numberofCurrentColumn,4] <- max(x[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[numberofCurrentColumn,5] <- getmode(x[,numberofCurrentColumn])
    resultDataframe[numberofCurrentColumn,6] <- round(mean(x[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[numberofCurrentColumn,7] <- round(sd(x[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[numberofCurrentColumn,8] <- round(median(x[,numberofCurrentColumn], na.rm = TRUE),digits=2)
  }
  
  return(resultDataframe)
}

getLageparameterAlter <- function(x, RowNamesLevels){
  numberofColumns <-ncol(x)  -1
  itemscolums <- c(1: numberofColumns)
  numberoflevels = nlevels(RowNamesLevels)
  RowNamesLevels <- levels(RowNamesLevels)
  itemsoflevels <- c(1:numberoflevels)
 

  resultDataframe <- data.frame()
  
  rownumber <- 0
  nummerforItem <- 0
  for(itemNumber in itemsoflevels){
    rownumber <- rownumber + 1
    nummerforItem <- nummerforItem +1
    
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- RowNamesLevels[itemNumber]
    resultDataframe[rownumber,3] <- c("18-25")
    resultDataframe[rownumber,4] <- NA
    resultDataframe[rownumber,5] <- NA
    resultDataframe[rownumber,6] <- NA
    resultDataframe[rownumber,7] <- NA
    resultDataframe[rownumber,8] <- NA
    resultDataframe[rownumber,9] <- NA
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("26-32")
    resultDataframe[rownumber,4] <- NA
    resultDataframe[rownumber,5] <- NA
    resultDataframe[rownumber,6] <- NA
    resultDataframe[rownumber,7] <- NA
    resultDataframe[rownumber,8] <- NA
    resultDataframe[rownumber,9] <- NA
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("33-40")
    resultDataframe[rownumber,4] <- NA
    resultDataframe[rownumber,5] <- NA
    resultDataframe[rownumber,6] <- NA
    resultDataframe[rownumber,7] <- NA
    resultDataframe[rownumber,8] <- NA
    resultDataframe[rownumber,9] <- NA
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("41-55")
    resultDataframe[rownumber,4] <- NA
    resultDataframe[rownumber,5] <- NA
    resultDataframe[rownumber,6] <- NA
    resultDataframe[rownumber,7] <- NA
    resultDataframe[rownumber,8] <- NA
    resultDataframe[rownumber,9] <- NA
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("56-65")
    resultDataframe[rownumber,4] <- NA
    resultDataframe[rownumber,5] <- NA
    resultDataframe[rownumber,6] <- NA
    resultDataframe[rownumber,7] <- NA
    resultDataframe[rownumber,8] <- NA
    resultDataframe[rownumber,9] <- NA
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("über 65")
    resultDataframe[rownumber,4] <- NA
    resultDataframe[rownumber,5] <- NA
    resultDataframe[rownumber,6] <- NA
    resultDataframe[rownumber,7] <- NA
    resultDataframe[rownumber,8] <- NA
    resultDataframe[rownumber,9] <- NA
  }

  resultDataframe <- resultDataframe %>% rename(
    Nr = V1,
    Feld = V2,
    Auspraegung = V3,
    Minimum =V4, 
    Maximum=V5, 
    Modus=V6, 
    Mittelwert=V7, 
    Standardabweichung=V8, 
    Median=V9)
  
  
  x18 <- x %>% filter(Alter == 2) %>% subset(select =-c(Alter))
  x26 <- x %>% filter(Alter == 3) %>% subset(select =-c(Alter))
  x33 <- x %>% filter(Alter == 4) %>% subset(select =-c(Alter))
  x41 <- x %>% filter(Alter == 5) %>% subset(select =-c(Alter))
  x55 <- x %>% filter(Alter == 6) %>% subset(select =-c(Alter))
  x65 <- x %>% filter(Alter == 7) %>% subset(select =-c(Alter))
  
  rowNumber <- 0
  for(numberofCurrentColumn in itemscolums){
    
    rowNumber <- rowNumber + 1
    resultDataframe[rowNumber,4] <- min(x18[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[rowNumber,5] <- max(x18[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[rowNumber,6] <- getmode(x18[,numberofCurrentColumn])
    resultDataframe[rowNumber,7] <- round(mean(x18[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,8] <- round(sd(x18[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,9] <- round(median(x18[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    
    rowNumber <- rowNumber + 1
    resultDataframe[rowNumber,4] <- min(x26[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[rowNumber,5] <- max(x26[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[rowNumber,6] <- getmode(x26[,numberofCurrentColumn])
    resultDataframe[rowNumber,7] <- round(mean(x26[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,8] <- round(sd(x26[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,9] <- round(median(x26[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    
    rowNumber <- rowNumber + 1
    resultDataframe[rowNumber,4] <- min(x33[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[rowNumber,5] <- max(x33[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[rowNumber,6] <- getmode(x33[,numberofCurrentColumn])
    resultDataframe[rowNumber,7] <- round(mean(x33[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,8] <- round(sd(x33[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,9] <- round(median(x33[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    
    rowNumber <- rowNumber + 1
    resultDataframe[rowNumber,4] <- min(x41[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[rowNumber,5] <- max(x41[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[rowNumber,6] <- getmode(x41[,numberofCurrentColumn])
    resultDataframe[rowNumber,7] <- round(mean(x41[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,8] <- round(sd(x41[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,9] <- round(median(x41[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    
    rowNumber <- rowNumber + 1
    resultDataframe[rowNumber,4] <- min(x55[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[rowNumber,5] <- max(x55[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[rowNumber,6] <- getmode(x55[,numberofCurrentColumn])
    resultDataframe[rowNumber,7] <- round(mean(x55[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,8] <- round(sd(x55[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,9] <- round(median(x55[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    
    rowNumber <- rowNumber + 1
    resultDataframe[rowNumber,4] <- min(x65[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[rowNumber,5] <- max(x65[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[rowNumber,6] <- getmode(x65[,numberofCurrentColumn])
    resultDataframe[rowNumber,7] <- round(mean(x65[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,8] <- round(sd(x65[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,9] <- round(median(x65[,numberofCurrentColumn], na.rm = TRUE),digits=2)
  }
  
  return(resultDataframe)
}


getLageparameterGeschlecht <- function(x, RowNamesLevels){
  numberofColumns <-ncol(x)  -1
  itemscolums <- c(1: numberofColumns)
  numberoflevels = nlevels(RowNamesLevels)
  RowNamesLevels <- levels(RowNamesLevels)
  itemsoflevels <- c(1:numberoflevels)
  
  
  resultDataframe <- data.frame()
  
  rownumber <- 0
  nummerforItem <- 0
  for(itemNumber in itemsoflevels){
    rownumber <- rownumber + 1
    nummerforItem <- nummerforItem +1
    
    resultDataframe[rownumber,1] <- nummerforItem
    resultDataframe[rownumber,2] <- RowNamesLevels[itemNumber]
    resultDataframe[rownumber,3] <- c("männlich")
    resultDataframe[rownumber,4] <- NA
    resultDataframe[rownumber,5] <- NA
    resultDataframe[rownumber,6] <- NA
    resultDataframe[rownumber,7] <- NA
    resultDataframe[rownumber,8] <- NA
    resultDataframe[rownumber,9] <- NA
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("weiblich")
    resultDataframe[rownumber,4] <- NA
    resultDataframe[rownumber,5] <- NA
    resultDataframe[rownumber,6] <- NA
    resultDataframe[rownumber,7] <- NA
    resultDataframe[rownumber,8] <- NA
    resultDataframe[rownumber,9] <- NA

   

  }
  
  resultDataframe <- resultDataframe %>% rename(
    Nr = V1,
    Feld = V2,
    Auspraegung = V3,
    Minimum =V4, 
    Maximum=V5, 
    Modus=V6, 
    Mittelwert=V7, 
    Standardabweichung=V8, 
    Median=V9)
  
  
  xm <- x %>% filter(Geschlecht == 1) %>% subset(select =-c(Geschlecht))
  xw <- x %>% filter(Geschlecht == 2) %>% subset(select =-c(Geschlecht))

    rowNumber <- 0
  for(numberofCurrentColumn in itemscolums){
    
    rowNumber <- rowNumber + 1
    resultDataframe[rowNumber,4] <- min(xm[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[rowNumber,5] <- max(xm[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[rowNumber,6] <- getmode(xm[,numberofCurrentColumn])
    resultDataframe[rowNumber,7] <- round(mean(xm[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,8] <- round(sd(xm[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,9] <- round(median(xm[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    
    rowNumber <- rowNumber + 1
    resultDataframe[rowNumber,4] <- min(xw[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[rowNumber,5] <- max(xw[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[rowNumber,6] <- getmode(xw[,numberofCurrentColumn])
    resultDataframe[rowNumber,7] <- round(mean(xw[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,8] <- round(sd(xw[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[rowNumber,9] <- round(median(xw[,numberofCurrentColumn], na.rm = TRUE),digits=2)
  }
  
  return(resultDataframe)
}


getLageparameter1DimensionalAlter <- function(x, TitelField){

  x18 <- x %>% filter(Alter == 2) %>% subset(select =-c(Alter))
  x26 <- x %>% filter(Alter == 3) %>% subset(select =-c(Alter))
  x33 <- x %>% filter(Alter == 4) %>% subset(select =-c(Alter))
  x41 <- x %>% filter(Alter == 5) %>% subset(select =-c(Alter))
  x55 <- x %>% filter(Alter == 6) %>% subset(select =-c(Alter))
  x65 <- x %>% filter(Alter == 7) %>% subset(select =-c(Alter))
  
  resultDataframe <- data.frame()
  
  rownumber <- 1
    resultDataframe[rownumber,1] <- 1
    resultDataframe[rownumber,2] <- TitelField
    resultDataframe[rownumber,3] <- c("18-25")
    resultDataframe[rownumber,4] <- min(x18[,1] , na.rm = TRUE)
    resultDataframe[rownumber,5] <- max(x18[,1], na.rm = TRUE)
    resultDataframe[rownumber,6] <- getmode(x18[,1])
    resultDataframe[rownumber,7] <- round(mean(x18[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,8] <- round(sd(x18[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,9] <- round(median(x18[,1], na.rm = TRUE),digits=2)

    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("26-32")
    resultDataframe[rownumber,4] <- min(x26[,1] , na.rm = TRUE)
    resultDataframe[rownumber,5] <- max(x26[,1], na.rm = TRUE)
    resultDataframe[rownumber,6] <- getmode(x26[,1])
    resultDataframe[rownumber,7] <- round(mean(x26[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,8] <- round(sd(x26[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,9] <- round(median(x26[,1], na.rm = TRUE),digits=2)
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("33-40")
    resultDataframe[rownumber,4] <- min(x33[,1] , na.rm = TRUE)
    resultDataframe[rownumber,5] <- max(x33[,1], na.rm = TRUE)
    resultDataframe[rownumber,6] <- getmode(x33[,1])
    resultDataframe[rownumber,7] <- round(mean(x33[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,8] <- round(sd(x33[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,9] <- round(median(x33[,1], na.rm = TRUE),digits=2)
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("41-55")
    resultDataframe[rownumber,4] <- min(x41[,1] , na.rm = TRUE)
    resultDataframe[rownumber,5] <- max(x41[,1], na.rm = TRUE)
    resultDataframe[rownumber,6] <- getmode(x41[,1])
    resultDataframe[rownumber,7] <- round(mean(x41[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,8] <- round(sd(x41[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,9] <- round(median(x41[,1], na.rm = TRUE),digits=2)
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("56-65")
    resultDataframe[rownumber,4] <- min(x55[,1] , na.rm = TRUE)
    resultDataframe[rownumber,5] <- max(x55[,1], na.rm = TRUE)
    resultDataframe[rownumber,6] <- getmode(x55[,1])
    resultDataframe[rownumber,7] <- round(mean(x55[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,8] <- round(sd(x55[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,9] <- round(median(x55[,1], na.rm = TRUE),digits=2)
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("über 65")
    resultDataframe[rownumber,4] <- min(x65[,1] , na.rm = TRUE)
    resultDataframe[rownumber,5] <- max(x65[,1], na.rm = TRUE)
    resultDataframe[rownumber,6] <- getmode(x65[,1])
    resultDataframe[rownumber,7] <- round(mean(x65[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,8] <- round(sd(x65[,1], na.rm = TRUE),digits=2)
    resultDataframe[rownumber,9] <- round(median(x65[,1], na.rm = TRUE),digits=2)
    
    resultDataframe <- resultDataframe %>% rename(
      Nr = V1,
      Feld = V2,
      Auspraegung = V3,
      Minimum =V4, 
      Maximum=V5, 
      Modus=V6, 
      Mittelwert=V7, 
      Standardabweichung=V8, 
      Median=V9)

  return(resultDataframe)
}


getLageparameter1DimensionalGeschlecht <- function(x, TitelField){
  resultDataframe <- data.frame()
  
  xm <- x %>% filter(Geschlecht == 1) %>% subset(select =-c(Geschlecht))
  xw <- x %>% filter(Geschlecht == 2) %>% subset(select =-c(Geschlecht))
  
  rownumber <- 1
  resultDataframe[rownumber,1] <- 1
  resultDataframe[rownumber,2] <- TitelField
  resultDataframe[rownumber,3] <- c("männlich")
  resultDataframe[rownumber,4] <- min(xm[,1] , na.rm = TRUE)
  resultDataframe[rownumber,5] <- max(xm[,1], na.rm = TRUE)
  resultDataframe[rownumber,6] <- getmode(xm[,1])
  resultDataframe[rownumber,7] <- round(mean(xm[,1], na.rm = TRUE),digits=2)
  resultDataframe[rownumber,8] <- round(sd(xm[,1], na.rm = TRUE),digits=2)
  resultDataframe[rownumber,9] <- round(median(xm[,1], na.rm = TRUE),digits=2)
  
  rownumber <- rownumber + 1
  resultDataframe[rownumber,1] <- NA
  resultDataframe[rownumber,2] <- NA
  resultDataframe[rownumber,3] <- c("weiblich")
  resultDataframe[rownumber,4] <- min(xw[,1] , na.rm = TRUE)
  resultDataframe[rownumber,5] <- max(xw[,1], na.rm = TRUE)
  resultDataframe[rownumber,6] <- getmode(xw[,1])
  resultDataframe[rownumber,7] <- round(mean(xw[,1], na.rm = TRUE),digits=2)
  resultDataframe[rownumber,8] <- round(sd(xw[,1], na.rm = TRUE),digits=2)
  resultDataframe[rownumber,9] <- round(median(xw[,1], na.rm = TRUE),digits=2)

  resultDataframe <- resultDataframe %>% rename(
    Nr = V1,
    Feld = V2,
    Auspraegung = V3,
    Minimum =V4, 
    Maximum=V5, 
    Modus=V6, 
    Mittelwert=V7, 
    Standardabweichung=V8, 
    Median=V9)
  

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














q17_BevorzugteKommunikation <- subset(dat, select=c("Nummer", "X17_1", "X17_2", "X17_3", "X17_4","X17_5", "X17_6","X17_7", "X17_8","X17_9", "X17_10","X17_10_TEXT"))
q17_BevorzugteKommunikation <- q17_BevorzugteKommunikation  %>% rename(Email = X17_1, Brief = X17_2,
                                                                       MobileApp = X17_3, SocialMedia = X17_4,
                                                                       Messanger = X17_5, SMS = X17_6,
                                                                       Telefon = X17_7, Chatbots = X17_8,
                                                                       ServiceProtale = X17_9, Andere = X17_10,
                                                                       AndereText = X17_10_TEXT)


q17_BevorzugteKommunikationAngepasst <- q17_BevorzugteKommunikation %>% gather(Email, Brief, MobileApp, SocialMedia, Messanger,SMS, Telefon, Chatbots, ServiceProtale, Andere, key = Medium, value = Frequenz)

levels = c("Email", "Brief", "MobileApp", "SocialMedia", "Messanger", "SMS", "Telefon", "Chatbots", "ServiceProtale", "Andere")

q17_BevorzugteKommunikationAngepasst$Medium <- as.factor(q17_BevorzugteKommunikationAngepasst$Medium )
q17_BevorzugteKommunikationAngepasst$Medium  <- factor(q17_BevorzugteKommunikationAngepasst$Medium, levels = levels)

q17_BevorzugteKommunikationAngepasst <- q17_BevorzugteKommunikationAngepasst %>% filter(!is.na(Frequenz))
q17_BevorzugteKommunikationAngepasst$Frequenz <- as.factor(q17_BevorzugteKommunikationAngepasst$Frequenz )


q17_BevorzugteKommunikationAngepasst %>% ggplot(aes(x=Medium)) + geom_bar(fill=colorSingleBar) + xlab("Medium") + ylab("Anzahl") + coord_flip()

q17_BevorzugteKommunikationText <-  q17_BevorzugteKommunikation %>% subset(select = c(AndereText))
q17_BevorzugteKommunikationText$AndereText <- str_trim(q17_BevorzugteKommunikationText$AndereText)

q17_BevorzugteKommunikationText <-  q17_BevorzugteKommunikationText %>% filter(q17_BevorzugteKommunikationText$AndereText != "")
AndereTexte <- data.frame(Andere = q17_BevorzugteKommunikationText)

knitr::kable(
  AndereTexte, longtable = TRUE, booktabs = TRUE
)

q_frequenz <- getfrequenz(subset(q17_BevorzugteKommunikation, select = levels),1,levels(q17_BevorzugteKommunikationAngepasst$Medium), c("Antwortanzahl"), c(1:10), FALSE)

knitr::kable(
  q_frequenz, longtable = TRUE, booktabs = TRUE,
  caption = "privat"
)


q17_BevorzugteKommunikationJoined <- JoinTableAlterAndGeschlecht(q17_BevorzugteKommunikationAngepasst)


frequenzfactors <- factor(levels)

q17_BevorzugteKommunikationJoined %>% ggplot(aes(x=Medium)) + geom_bar(fill=colorSingleBar) + xlab("Medium") + ylab("Anzahl") + facet_wrap(.~AlterText, nrow=3, ncol = 2, scales = "free", dir = "h")  + coord_flip()

q17_BevorzugteKommunikationAlter <- JoinTableAlter(q17_BevorzugteKommunikation)

q_lageparameterAlter <- getLageparameterAlter(q17_BevorzugteKommunikationAlter %>% subset(select = -c(AndereText)), frequenzfactors)

knitr::kable(
  q_lageparameterAlter, longtable = TRUE, booktabs = TRUE
)

q_frequenzalter<- getfrequenzAlter(q17_BevorzugteKommunikationAlter, frequenzfactors,  c("Antwortanzahl"), FALSE)

knitr::kable(
  q_frequenzalter, longtable = TRUE, booktabs = TRUE
)


q17_BevorzugteKommunikationJoined %>% ggplot(aes(x=Medium)) + geom_bar(fill=colorSingleBar) + xlab("Medium") + ylab("Anzahl") + facet_wrap(.~GeschlechtText)  + coord_flip()

q17_BevorzugteKommunikationGeschlecht <- JoinTableGeschlecht(q17_BevorzugteKommunikation)

q_lageparameterGeschlecht <- getLageparameterGeschlecht (q17_BevorzugteKommunikationGeschlecht %>% subset(select = -c(AndereText)), frequenzfactors)

knitr::kable(
  q_lageparameterGeschlecht, longtable = TRUE, booktabs = TRUE
)



q_frequenzgeschlecht <- getfrequenzgeschlecht(q17_BevorzugteKommunikationGeschlecht,frequenzfactors, c("Antwortanzahl"), FALSE)

knitr::kable(
  q_frequenzgeschlecht, longtable = TRUE, booktabs = TRUE
)

