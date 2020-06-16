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


getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getfrequenz <- function(x, NumberOfItems, RowNamesLevels, ColumnNames, itemNumbers, TotalAnzeigen){
  numberofColumns <- ncol(x)
  numberofRows <-  nrow(x)
  items <- c(1:NumberOfItems)
  itemscolums <- c(1: numberofColumns)
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels)
  
  for(itemNumber in items){
    columnname = ColumnNames[itemNumber]
    resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "0.00% (0)")
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
        
        resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers[val,3]), "% (",as.character(numbers[val,2]), ")", sep ="")
        
        summeItems <- summeItems + as.numeric(numbers[val,2])
      }
      if(TotalAnzeigen){
        resultDataframe[numberofCurrentColumn,NumberOfItems +3] <- summeItems
      }
    }
  }
  
  return(resultDataframe)
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

getLageparameter1Dimensional <- function(x, TitelField){
  numberofColumns <-ncol(x)
  itemscolums <- c(1: numberofColumns)
  
  values <- c(0)
  
  
  resultDataframe <- data.frame(Nr = c(1), Feld = TitelField, Minimum =values, Maximum=values, Modus=values, Mittelwert=values, Standardabweichung=values, Median=values)
  resultDataframe[1,3] <- min(x[,1] , na.rm = TRUE)
  resultDataframe[1,4] <- max(x[,1], na.rm = TRUE)
  resultDataframe[1,5] <- getmode(x[,1])
  resultDataframe[1,6] <- round(mean(x[,1], na.rm = TRUE),digits=2)
  resultDataframe[1,7] <- round(sd(x[,1], na.rm = TRUE),digits=2)
  resultDataframe[1,8] <- round(median(x[,1], na.rm = TRUE),digits=2)
  
  return(resultDataframe)
}

getfrequenz1Dimension <- function(x, NumberOfItems, RowNamesLevels, itemNumbers){
  items <- c(1:NumberOfItems)
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels)
  resultDataframe <- resultDataframe %>% mutate(Antwortanzahl= "0.00% (0)")
  
  numbers <- transform(as.data.frame(table(x[1])), percentage = round(Freq/nrow(x)*100, digits = 2))
  numbers$Var1 <- as.numeric(as.character(numbers$Var1))
  
  for(val in items)
  {
    if(!is.na(numbers[val,1]))
    {
      columnnumber <- 3
      rownumber <- as.numeric(numbers[val,1])
      
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers[val,3]), "% (",as.character(numbers[val,2]), ")", sep ="")
    }
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

q23_Geschlecht %>% ggplot(aes(x= GeschlechtText, fill = GeschlechtText)) + labs(x = "Geschlecht", fill = "Geschlecht", y="Anzahl")  + geom_bar() + geom_text(aes(label =stat(count)), stat= 'count', position = position_stack(vjust = .5),color="white") + scale_fill_manual(values=colorsGender)

q_lageparameter <- getLageparameter1Dimensional(subset(q23_Geschlecht, select = c(Geschlecht)), "Geschlecht")

knitr::kable(
  q_lageparameter, longtable = TRUE, booktabs = TRUE
)

q_frequenz <- getfrequenz1Dimension(subset(q23_Geschlecht, select = c(Geschlecht)),2, levels(q23_Geschlecht$GeschlechtText), c(1:2))

knitr::kable(
  q_frequenz, longtable = TRUE, booktabs = TRUE
)


q23 <- data.frame(Bezeichnung =c(
  "Männlich",
  "Weiblich",
  "Standardabweichung", 
  "Mittelwert",
  "Median",
  "Modus"),
  Wert = c(
    "1",
    "2",
    round(sd(q23_Geschlecht$Geschlecht, na.rm = TRUE),digits=6), 
    round(mean(q23_Geschlecht$Geschlecht, na.rm = TRUE),digits=6),
    round(median(q24_Alter$Alter, na.rm = TRUE),digits=6),
    getmode(q24_Alter$Alter)))

knitr::kable(
  q23, longtable = TRUE, booktabs = TRUE,
  caption = "Werte Geschlecht"
)
