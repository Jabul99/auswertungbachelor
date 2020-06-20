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
    
    numbers <- transform(as.data.frame(table(x[numberofCurrentColumn])), percentage = round(Freq/nrow(x[numberofCurrentColumn] %>% filter( !is.na(x[numberofCurrentColumn])))*100, digits = 2))
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

getfrequenzgeschlecht15 <- function(x, Rowfactors, ColumnNames){
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
    resultDataframe[rownumber,4] <- c("(0)")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("weiblich")
    resultDataframe[rownumber,4] <- c("(0)")
  }
  
  resultDataframe <- resultDataframe %>% rename(
    Nr = V1,
    Feld = V2,
    Auspraegung = V3,
    Antwortanzahl =V4)

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
          paste("(",as.character(numbersm[val,3]), ")", sep ="")
        
        summeItemsm <- summeItemsm + as.numeric(numbersm[val,3])
      }
      
      if(!is.na(numbersw[val,1]))
      { 
        rownumber <- correctRow + 1
        resultDataframe[rownumber,columnnumber] <-
          paste("(",as.character(numbersw[val,3]), ")", sep ="")
        
        summeItemsw <- summeItemsw + as.numeric(numbersw[val,3])
      }
     
    }
    
    correctRow <- correctRow + 2
  }
  
  return(resultDataframe)
}

getfrequenzAlter15 <- function(x, Rowfactors, ColumnNames){
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
    resultDataframe[rownumber,4] <- c("(0)")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("26-32")
    resultDataframe[rownumber,4] <- c("(0)")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("33-40")
    resultDataframe[rownumber,4] <- c("(0)")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("41-55")
    resultDataframe[rownumber,4] <- c("(0)")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("56-65")
    resultDataframe[rownumber,4] <- c("(0)")
    
    rownumber <- rownumber + 1
    resultDataframe[rownumber,1] <- NA
    resultDataframe[rownumber,2] <- NA
    resultDataframe[rownumber,3] <- c("über 65")
    resultDataframe[rownumber,4] <- c("(0)")
  }
  
  resultDataframe <- resultDataframe %>% rename(
    Nr = V1,
    Feld = V2,
    Auspraegung = V3, 
    Antwortanzahl = V4)
  
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
          resultDataframe[rownumber,columnnumber] <-paste("(",as.character(numbers18[val,3]), ")", sep ="")
          summeItems18 <- summeItems18 + as.numeric(numbers18[val,3])
        }
        
        if(!is.na(numbers26[val,1]) & !is.na(numbers26[val,3]))
        {
          rownumber <- correctRow + 1
          resultDataframe[rownumber,columnnumber] <- paste("(",as.character(numbers26[val,3]), ")", sep ="")
          summeItems26 <- summeItems26 + as.numeric(numbers26[val,3])
        }
        
        if(!is.na(numbers33[val,1]) & !is.na(numbers33[val,3]))
        {
          rownumber <- correctRow + 2
          resultDataframe[rownumber,columnnumber] <- paste("(",as.character(numbers33[val,3]), ")", sep ="")
          summeItems33 <- summeItems33 + as.numeric(numbers33[val,3])
        }
        
        if(!is.na(numbers41[val,1]) & !is.na(numbers41[val,3]))
        {
          rownumber <- correctRow + 3
          resultDataframe[rownumber,columnnumber] <- paste("(",as.character(numbers41[val,3]), ")", sep ="")
          summeItems41 <- summeItems41 + as.numeric(numbers41[val,3])
        }
        
        if(!is.na(numbers55[val,1]) & !is.na(numbers55[val,3]))
        {
          rownumber <- correctRow + 4
          resultDataframe[rownumber,columnnumber] <- paste("(",as.character(numbers55[val,3]), ")", sep ="")
          summeItems55 <- summeItems55 + as.numeric(numbers55[val,3])
        }
        
        if(!is.na(numbers65[val,1]) & !is.na(numbers65[val,3]))
        {
          rownumber <- correctRow + 5
          resultDataframe[rownumber,columnnumber] <- paste("(",as.character(numbers65[val,3]), ")", sep ="")
          summeItems65 <- summeItems65 + as.numeric(numbers65[val,3])
        }
        
      }
    }
    
    correctRow <- correctRow + 6
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







q15_EmailNewsletterprivat <- subset(dat, select=c("Nummer", "X15_1_1", "X15_2_1","X15_3_1", "X15_4_1", "X15_5_1", "X15_6_1"))
q15_EmailNewsletterprivat <- q15_EmailNewsletterprivat  %>% rename(Intresse = X15_1_1, Aktionen = X15_2_1,
                                                                   Angebot = X15_3_1, Inhalt = X15_4_1, Trends = X15_5_1, Unternehmen = X15_6_1)

q15_EmailNewsletterfirm <- subset(dat, select=c("Nummer", "X15_1_2", "X15_2_2","X15_3_2", "X15_4_2", "X15_5_2", "X15_6_2"))
q15_EmailNewsletterfirm <- q15_EmailNewsletterfirm  %>% rename(Intresse = X15_1_2, Aktionen = X15_2_2,
                                                               Angebot = X15_3_2, Inhalt = X15_4_2, Trends = X15_5_2, Unternehmen = X15_6_2)

columnNames1 <- c("Intresse", "Aktionen", "Angebot", "Inhalt", "Trends", "Unternehmen")
levels2 = c("Interesse an einem Produkt / einer Dienstleistung", "Information über Aktionen", "Infromationen über neue Angebote","Inhalt der Informationen","Informationen über neue Trends", "Informationen über das unternehmen")
levels = addNA(levels2)
q15_EmailNewsletterprivatAngepasst <- q15_EmailNewsletterprivat %>% subset(select = columnNames1) %>% 
  mutate(
    Intresse = case_when(
      Intresse == 1 ~ levels[1], 
      Intresse == 2 ~ levels[2], 
      Intresse == 3 ~ levels[3],
      Intresse == 4 ~ levels[4],
      Intresse == 5 ~ levels[5],
      Intresse == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Angebot = case_when(
      Angebot == 1 ~ levels[1], 
      Angebot == 2 ~ levels[2], 
      Angebot == 3 ~ levels[3],
      Angebot == 4 ~ levels[4],
      Angebot == 5 ~ levels[5],
      Angebot == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Inhalt = case_when(
      Inhalt == 1 ~ levels[1], 
      Inhalt == 2 ~ levels[2], 
      Inhalt == 3 ~ levels[3],
      Inhalt == 4 ~ levels[4],
      Inhalt == 5 ~ levels[5],
      Inhalt == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Trends = case_when(
      Trends == 1 ~ levels[1], 
      Trends == 2 ~ levels[2], 
      Trends == 3 ~ levels[3],
      Trends == 4 ~ levels[4],
      Trends == 5 ~ levels[5],
      Trends == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Unternehmen = case_when(
      Unternehmen == 1 ~ levels[1], 
      Unternehmen == 2 ~ levels[2], 
      Unternehmen == 3 ~ levels[3],
      Unternehmen == 4 ~ levels[4],
      Unternehmen == 5 ~ levels[5],
      Unternehmen == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Aktionen = case_when(
      Aktionen == 1 ~ levels[1], 
      Aktionen == 2 ~ levels[2], 
      Aktionen == 3 ~ levels[3],
      Aktionen == 4 ~ levels[4],
      Aktionen == 5 ~ levels[5],
      Aktionen == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels))

q15_EmailNewsletterfirmAngepasst <- q15_EmailNewsletterfirm %>% subset(select = c(Nummer, Intresse, Aktionen, Angebot, Inhalt, Trends, Unternehmen)) %>% 
  mutate(
    Intresse = case_when(
      Intresse == 1 ~ levels[1], 
      Intresse == 2 ~ levels[2], 
      Intresse == 3 ~ levels[3],
      Intresse == 4 ~ levels[4],
      Intresse == 5 ~ levels[5],
      Intresse == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Angebot = case_when(
      Angebot == 1 ~ levels[1], 
      Angebot == 2 ~ levels[2], 
      Angebot == 3 ~ levels[3],
      Angebot == 4 ~ levels[4],
      Angebot == 5 ~ levels[5],
      Angebot == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Inhalt = case_when(
      Inhalt == 1 ~ levels[1], 
      Inhalt == 2 ~ levels[2], 
      Inhalt == 3 ~ levels[3],
      Inhalt == 4 ~ levels[4],
      Inhalt == 5 ~ levels[5],
      Inhalt == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Trends = case_when(
      Trends == 1 ~ levels[1], 
      Trends == 2 ~ levels[2], 
      Trends == 3 ~ levels[3],
      Trends == 4 ~ levels[4],
      Trends == 5 ~ levels[5],
      Trends == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Unternehmen = case_when(
      Unternehmen == 1 ~ levels[1], 
      Unternehmen == 2 ~ levels[2], 
      Unternehmen == 3 ~ levels[3],
      Unternehmen == 4 ~ levels[4],
      Unternehmen == 5 ~ levels[5],
      Unternehmen == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels),
    Aktionen = case_when(
      Aktionen == 1 ~ levels[1], 
      Aktionen == 2 ~ levels[2], 
      Aktionen == 3 ~ levels[3],
      Aktionen == 4 ~ levels[4],
      Aktionen == 5 ~ levels[5],
      Aktionen == 6 ~ levels[6],
      TRUE ~  levels[7]) %>% factor(levels))


q15_EmailNewsletterprivatAngepasst <- q15_EmailNewsletterprivat %>% gather(Intresse, Aktionen, Angebot, Inhalt, Trends, Unternehmen, key = Grund, value = Frequenz)

q15_EmailNewsletterprivatAngepasst <- q15_EmailNewsletterprivatAngepasst %>% filter(!is.na(Frequenz))

q15_EmailNewsletterprivatAngepasst <- q15_EmailNewsletterprivatAngepasst %>% mutate(Frequenz = "privat")

q15_EmailNewsletterfirmAngepasst <- q15_EmailNewsletterfirm %>% gather(Intresse, Aktionen, Angebot, Inhalt, Trends, Unternehmen, key = Grund, value = Frequenz)

q15_EmailNewsletterfirmAngepasst <- q15_EmailNewsletterfirmAngepasst %>% filter(!is.na(Frequenz))
q15_EmailNewsletterfirmAngepasst <- q15_EmailNewsletterfirmAngepasst %>% mutate(Frequenz = "geschäftlich")

q15_EmailNewsletterprivatAngepasst$Grund <- as.factor(q15_EmailNewsletterprivatAngepasst$Grund)
q15_EmailNewsletterfirmAngepasst$Grund <- as.factor(q15_EmailNewsletterfirmAngepasst$Grund)
q15_EmailNewsletterprivatAngepasst$Grund <- factor(q15_EmailNewsletterprivatAngepasst$Grund, levels = columnNames1)
q15_EmailNewsletterfirmAngepasst$Grund <- factor(q15_EmailNewsletterfirmAngepasst$Grund, levels = columnNames1)

q15_EmailNewsletter <- rbind(q15_EmailNewsletterprivatAngepasst,q15_EmailNewsletterfirmAngepasst)
q15_EmailNewsletter$Frequenz <- as.factor(q15_EmailNewsletter$Frequenz)

q15_EmailNewsletterJoined <- JoinTableAlterAndGeschlecht(q15_EmailNewsletter)

q15_EmailNewsletter %>% ggplot(aes(x=Frequenz, fill= Grund)) + geom_bar(position = "dodge2") + xlab("") + ylab("Anzahl") + coord_flip()

q_frequenz <- getfrequenz(subset(q15_EmailNewsletterprivat, select = columnNames1),1,levels(q15_EmailNewsletterprivatAngepasst$Grund), c("Email-Newsletter"), c(1:6), TRUE)

knitr::kable(
  q_frequenz, longtable = TRUE, booktabs = TRUE,
  caption = "privat"
)

q_frequenz <- getfrequenz(subset(q15_EmailNewsletterfirm, select = columnNames1),1,levels(q15_EmailNewsletterfirmAngepasst$Grund), c("Email-Newsletter"), c(1:6), TRUE)

knitr::kable(
  q_frequenz, longtable = TRUE, booktabs = TRUE,
  caption = "geschäftlich"
)


frequenzfactors <- factor(columnNames1, levels = columnNames1)

q15_EmailNewsletterJoined %>% ggplot(aes(x=Frequenz, fill= Grund)) + geom_bar(position = "dodge2") + xlab("") + ylab("Anzahl") + facet_wrap(.~AlterText, nrow=3, ncol = 2, scales = "free", dir = "h")  + coord_flip()

q15_EmailNewsletterAlter <- JoinTableAlter(q15_EmailNewsletterprivat)

q_frequenzalter<- getfrequenzAlter15(q15_EmailNewsletterAlter, frequenzfactors, columnNames1)

knitr::kable(
  q_frequenzalter, longtable = TRUE, booktabs = TRUE, caption = "private"
)

q15_EmailNewsletterAlter2 <- JoinTableAlter(q15_EmailNewsletterfirm)

q_frequenzalter<- getfrequenzAlter15(q15_EmailNewsletterAlter2, frequenzfactors, columnNames1)

knitr::kable(
  q_frequenzalter, longtable = TRUE, booktabs = TRUE, caption = "geschäftlich"
)


q15_EmailNewsletterJoined %>% ggplot(aes(x=Frequenz, fill= Grund)) + geom_bar(position = "dodge2") + xlab("") + ylab("Anzahl") + facet_wrap(.~GeschlechtText)  + coord_flip()

q15_EmailNewsletterGeschlecht <- JoinTableGeschlecht(q15_EmailNewsletterprivat)

q_frequenzgeschlecht <- getfrequenzgeschlecht15(q15_EmailNewsletterGeschlecht,frequenzfactors,levels2)

knitr::kable(
  q_frequenzgeschlecht, longtable = TRUE, booktabs = TRUE, caption = "private"
)

q15_EmailNewsletterGeschlecht2 <- JoinTableGeschlecht(q15_EmailNewsletterfirm)

q_frequenzgeschlecht <- getfrequenzgeschlecht15(q15_EmailNewsletterGeschlecht2,frequenzfactors,levels2)

knitr::kable(
  q_frequenzgeschlecht, longtable = TRUE, booktabs = TRUE , caption = "geschäftlich"
)
