## data processing

## cache = TRUE
dir <- 'D:/Coursera/5. Reproducible Research/Assessments'
setwd(dir)
data <- read.csv(bzfile('repdata-data-StormData.csv.bz2'))

evtype <- unique(data$EVTYPE)

unique(data$PROPDMGEXP)

## Question 1
fatalities <- aggregate(data$FATALITIES, list(data$EVTYPE), FUN= sum)
names(fatalities) <- c('Evtype', 'Fatalities')
fata_order <- fatalities[order(-fatalities$Fatalities), ]

injuries <- aggregate(data$INJURIES, list(data$EVTYPE), FUN= sum)
names(injuries) <- c('Evtype', 'Injuries')
inju_order <- injuries[order(-injuries$Injuries), ]

fata_inju <- merge(fata_order, inju_order, by='Evtype')
fata_inju$Fata_Inju <- fata_inju$Fatalities + fata_inju$Injuries
fata_inju_order <- fata_inju[order(-fata_inju$Fata_Inju), ]


library(ggplot2)
library(gridExtra)
p1 <- ggplot(data=head(fata_order, 10), aes(x=reorder(Evtype, Fatalities), y=Fatalities), fill=Fatalities) + 
    geom_bar(stat='identity') + 
    coord_flip() + ylab('Total fatalities') + xlab('Event type')
#plot(p1)

p2 <- ggplot(data=head(inju_order, 10), aes(x=reorder(Evtype, Injuries), y=Injuries)) + 
    geom_bar(stat='identity') +
    coord_flip() + ylab('Total injuries') + xlab('Event type')
#plot(p2)
grid.arrange(p1, p2)
## Question 2
head(data$PROPDMGEXP)
exp_transform <- function(e){
    #B: billion; K: thousand; h and H: hundred; m and M: million
    if(e %in% c('B', 'b'))
        return(9)
    else if(e %in% c('m', 'M'))
        return(6)
    else if(e %in% c('K', 'k'))
        return(3)
    else if(e %in% c('h', 'H'))
        return(2)
    else if(e %in% c(1,2,3,4,5,6,7,8,9,0))
        return(e)
    else{
        return(0)}
}
## cache=TRUE
data$Propdmg_new <- sapply(data$PROPDMGEXP, exp_transform)
data$Cropdmg_new <- sapply(data$CROPDMGEXP, exp_transform)
data$Prop_value <- data$PROPDMG*(10**data$Propdmg_new)
data$Crop_value <- data$CROPDMG*(10**data$Cropdmg_new)

propdmg <- aggregate(data$Prop_value, list(data$EVTYPE), FUN=sum)
cropdmg <- aggregate(data$Crop_value, list(data$EVTYPE), FUN=sum)
names(propdmg) <- c('Evtype', 'Propdmg')
names(cropdmg) <- c('Evtype', 'Cropdmg')
propdmg_order <- propdmg[order(-propdmg$Propdmg), ]
cropdmg_order <- cropdmg[order(-cropdmg$Cropdmg), ]
propdmg_order$Propdmg <- log(propdmg_order$Propdmg)

p3 <- ggplot(data=head(propdmg_order, 10), aes(x=reorder(Evtype, Propdmg), y=Propdmg)) +
    geom_bar(stat='identity') +
    coord_flip() + ylab('Total Prop damage') + xlab('Event type')
plot(p3)

p4 <- ggplot(data=head(cropdmg_order, 10), aes(x=reorder(Evtype, Cropdmg), y=Cropdmg)) +
    geom_bar(stat='identity') +
    coord_flip() + ylab('Total Crop damage') + xlab('Event type')
plot(p4)
