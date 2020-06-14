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

q5_bezahlteLinks <- subset(dat, select=c("Nummer","X5"))
q5_bezahlteLinks <- q5_bezahlteLinks %>% rename(ZweckKauf = X5)

levels = c("privat", "geschäftlich", "beides")

q5_bezahlteLinksAngepasst <- q5_bezahlteLinks  %>% subset(select =c(Nummer, ZweckKauf)) %>% 
  mutate(
    ZweckKauf = case_when(
      ZweckKauf == 1 ~ levels[1], 
      ZweckKauf == 2 ~ levels[2], 
      ZweckKauf == 3 ~ levels[3],
      TRUE ~  levels[1]) %>% factor(levels))

q5_bezahlteLinksAngepasst %>% ggplot(aes(x=ZweckKauf)) + geom_bar(fill=colorSingleBar) + xlab("Zweck") + ylab("Anzahl") + coord_flip()

q5_lageparameter <- getLageparameter1Dimensional(subset(q5_bezahlteLinks, select = c(ZweckKauf)), "Zweck")

knitr::kable(
  q5_lageparameter, longtable = TRUE, booktabs = TRUE
)

q5_frequenz <- getfrequenz1Dimension(subset(q5_bezahlteLinks, select = c(ZweckKauf)),3, levels(q5_bezahlteLinksAngepasst$ZweckKauf), c(1:3))