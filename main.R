library(dplyr)
library(ggplot2)

loadData <- function(zipUrl, zipdest){
        if (!file.exists(zipdest)) download.file(zipUrl,zipdest)
        stormData <- read.csv(zipdest)
        stormData
}

prepData <- function(stormData){
        stormData <- mutate(stormData, PROPDMG = ifelse(stormData$PROPDMGEXP=="K",stormData$PROPDMG*1000, stormData$PROPDMG*1000000))
        stormData <- select(stormData, EVTYPE, FATALITIES, INJURIES, PROPDMG)
        stormData
}

runAnalisis <- function(table, valueField, minPercentileAnalised, chartTitle, order=""){
        
        tableX <- group_by(table, EVTYPE)
        tableX <- summarize(tableX, sumValue= sum(!! sym(valueField)))
        tableX <- filter(tableX, sumValue>0) 
        tableX <- arrange(tableX, desc(sumValue)) 
        tableX$percentile <- as.integer(.bincode(tableX$sumValue, quantile(tableX$sumValue, probs=seq(0, 1, 1/100)), include.lowest=TRUE))
        tableX <- filter(tableX, percentile>=minPercentileAnalised)
        
        if (order=="B") div = 1000000000
        if (order=="M") div = 1000000
        if (order=="K") div = 1000
        if (order=="")  div = 1

        #knitr::kable(tableX)
        #knitr::kable(head(iris), format = "markdown")
        
        g<- ggplot(tableX, aes(x = reorder(EVTYPE, sumValue), y=sumValue)) + 
                geom_bar(position = 'dodge',stat = "identity") +
                geom_text(aes(label=paste(round(sumValue/div,digits = 2), order)), position=position_dodge(width=0.9), hjust=-0.1, col="red")+
                ggtitle(chartTitle) +
                xlab("") +
                ylab("") +
                theme(axis.text.x=element_blank())+
                ylim(0, max(tableX$sumValue)) +
                coord_flip()
        
        g
}

minPercentileShowed<- 90

stormData <- loadData("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "repdata_data_StormData.csv.bz2")
stormData <- prepData(stormData)
g_fatalitis <- runAnalisis(stormData, "FATALITIES", minPercentileShowed, "Most harmful events by Fatalities (in Thousands)", "K")
g_injuries <- runAnalisis(stormData, "INJURIES", minPercentileShowed, "Most harmful events by Injuries (in Thousands)", "K")
g_propdmg <- runAnalisis(stormData, "PROPDMG", minPercentileShowed, "Most harmful events by Properties Damage (in Billions)", "B")


