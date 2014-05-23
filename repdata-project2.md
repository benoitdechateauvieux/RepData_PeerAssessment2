Economic and health consequences of climate events in the United States (1950-2011)
========================================================

## Synopsis
In this report we aim to describe the economic and health consequences of climate events in the United States in the years 1950 to 2011. We obtained data from the [U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).  
From these data, we found that, on average across the U.S., Tornadoes are, by far, the events that have greatest impact on population health. They represent 62% of all the injuries and fatalities.  
From an economic point of view, Floods have the greatest impact. Tornadoes come in third position.  
Damages on properties represent 90% of the total amount of the damages, for all type of events.

## Data Processing
We first read the data from the raw text file included in the zip archive.

```r
stormData <- read.csv(bzfile("StormData.csv.bz2"))
```


### Cleaning type of events
Data in EVTYPE have a poor quality level: same EVTYPE with different case, singular or plural form of the same event, ...

```r
c("case sensitive"=length(unique(stormData$EVTYPE)), 
  "case insensitive"=length(unique(toupper(stormData$EVTYPE))))
```

```
##   case sensitive case insensitive 
##              985              898
```

```r
#EVTYPE are capitalized in order to reduce duplicated entries rate.
stormData <- transform(stormData, EVTYPE = toupper(EVTYPE))
```


### Pre-processing damages estimate
As indicated in [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf):  
*Alphabetical characters used to signify magnitude include “K” for thousands, “M” for millions, and “B” for billions.*

```r
summary(stormData$PROPDMGEXP)
```

```
##             -      ?      +      0      1      2      3      4      5 
## 465934      1      8      5    216     25     13      4      4     28 
##      6      7      8      B      h      H      K      m      M 
##      4      5      1     40      1      6 424665      7  11330
```

```r
summary(stormData$CROPDMGEXP)
```

```
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
```

PROPDMGEXP and CROPDMGEXP are capitalized and only "K", "M" and "B" are taken into account.  
No value in PROPDMGEXP or CROPDMGEXP columns indicates that the amount is expressed in units of dollars.  
Other levels make up a very small percentage of the overall data points. We decide to discard the observations associated with those events in the analysis.


Then we compute the amount of the damages for that event.

```r
getValueFromExp <- function(x) {
        if (x == "") { x <- 1 }
        else if (toupper(x) == "K") { x <- 1000 }
        else if (toupper(x) == "M") { x <- 1000000 }
        else if (toupper(x) == "B") { x <- 1000000000 }
        else { x <- NA }
       x }
stormData$PROPDMGEXPVAL <- sapply(stormData$PROPDMGEXP, getValueFromExp)
stormData$CROPDMGEXPVAL <- sapply(stormData$CROPDMGEXP, getValueFromExp)  
stormData$PROPDMGTOT <- stormData$PROPDMG*stormData$PROPDMGEXPVAL
stormData$CROPDMGTOT <- stormData$CROPDMG*stormData$CROPDMGEXPVAL
```


## Results

### Across the United States, tornadoes are the most harmful events with respect to population health

```r
library(ggplot2)
library(reshape2)

#aggregate Fatalities & Injuries by Event
fataAndInju <- aggregate(list(Fatalities=stormData$FATALITIES, 
                              Injuries=stormData$INJURIES), 
                         list(Event=stormData$EVTYPE), sum)

fataAndInjuSup0 <- fataAndInju[(fataAndInju$Fatalities+fataAndInju$Injuries)>0, ]
c("Total number of events"=length(unique(fataAndInju$Event)), 
  "Number of impacting events"=length(unique(fataAndInjuSup0$Event)))
```

```
##     Total number of events Number of impacting events 
##                        898                        205
```

Of the 898 events, 205 have an impact on population health (Injuries or Fatalities).


```r
#Keeping Top20
fataAndInjuSorted <- fataAndInjuSup0[order(fataAndInjuSup0$Fatalities+fataAndInjuSup0$Injuries, decreasing=T),]
fataAndInjuTop <- fataAndInjuSorted[1:20, ]
#Ordering factor levels by total => keep order in barplot
fataAndInjuTop <- within(fataAndInjuTop, Event <- factor(fataAndInjuTop$Event, levels=fataAndInjuTop$Event))
fataAndInjuMelted <- melt(fataAndInjuTop, id.var="Event")

ggplot(fataAndInjuMelted, aes(x = Event, y = value, fill = variable)) + 
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y="Number of Fatalities/Injuries") +
        ggtitle(expression(atop("Top 20 most harmful events with respect to population health", 
                                atop(italic("accross the United States (1950-2011)"), ""))))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



```r
sumTornado   <- sum(fataAndInju[fataAndInju$Event=='TORNADO', "Fatalities"])+
                sum(fataAndInju[fataAndInju$Event=='TORNADO', "Injuries"])

sumNoTornado <- sum(fataAndInju[fataAndInju$Event!='TORNADO', "Fatalities"])+
                sum(fataAndInju[fataAndInju$Event!='TORNADO', "Injuries"])
#Sum of injuries and fatalities for Tornadoes and for all other events
tornadoInfo   <- c("Tornado",sumTornado, sumTornado*100/(sumTornado+sumNoTornado))
noTornadoInfo <- c("Other events", sumNoTornado, sumNoTornado*100/(sumTornado+sumNoTornado))
tornadoVsNoTornado <- data.frame(rbind(tornadoInfo, noTornadoInfo), row.names=1)
names(tornadoVsNoTornado) <- c("Injuries + Fatalities", "% of total")
tornadoVsNoTornado
```

```
##              Injuries + Fatalities       % of total
## Tornado                      96979 62.2966089174102
## Other events                 58694 37.7033910825898
```

Tornadoes represent 62% of all the injuries and fatalities. 


### Across the United States, Floods, Hurricanes/Typhoons, Torandoes and Storm surges are the events that have the greatest economic consequences.

```r
library(ggplot2)
library(reshape2)

#aggregate Properties & Crop by Event
propAndCrop <- aggregate(list(Properties=stormData$PROPDMGTOT, 
                              Crops=stormData$CROPDMGTOT), 
                         list(Event=stormData$EVTYPE), sum, na.rm=T)

propAndCropSup0 <- propAndCrop[(propAndCrop$Properties+propAndCrop$Crop)>0, ]
c("Total number of events"=length(unique(propAndCrop$Event)), 
  "Number of impacting events"=length(unique(propAndCropSup0$Event)))
```

```
##     Total number of events Number of impacting events 
##                        898                        393
```

Of the 898 events, 393 have an economical impact (Properties or Crops).


```r
#Calculate total amount and percentage for Properties and Crops
propAndCropTot <- colSums(propAndCropSup0[,2:3])
propVsCrop <- data.frame(Damage=names(propAndCropTot), amount=propAndCropTot)
propVsCrop <- cbind(propVsCrop, percent=paste(round(propVsCrop$amount/sum(propVsCrop$amount), 3)*100,"%"))


ggplot(propVsCrop, aes(x = "", y = amount, fill = Damage)) + 
        geom_bar(stat = "identity", width = 1) + 
        geom_text(aes(y = amount/2 + c(0, cumsum(amount)[-length(amount)]), label = percent), size=8) +
        coord_polar(theta = "y") +
        labs(y="", x="") +
        ggtitle(expression(atop("Distribution of total amount of damages (in US$)", 
                                atop(italic("for all type of events accross the United States (1950-2011)"), ""))))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

From the plot above, we can notice that damages on properties represent 90% of the total amount of the damages, for all type of events.



```r
#Keeping Top20
propAndCropSorted <- propAndCropSup0[order(propAndCropSup0$Properties+propAndCropSup0$Crop, decreasing=T),]
propAndCropTop <- propAndCropSorted[1:20, ]
#Ordering factor levels by total => keep order in barplot
propAndCropTop <- within(propAndCropTop, Event <- factor(propAndCropTop$Event, levels=propAndCropTop$Event))
propAndCropMelted <- melt(propAndCropTop, id.var="Event", variable.name="Type")

propAndCropMelted$value <- propAndCropMelted$value/1000000000
ggplot(propAndCropMelted, aes(x = Event, y = value, fill = Type)) + 
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(y="Property & Crop damage estimates (in billions of US$)") +
        ggtitle(expression(atop("Top 20 of the most consequential events in economic terms", 
                                atop(italic("accross the United States (1950-2011)"), ""))))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

From the plot above, we can notice that 4 events concentrate the majority of the damages: Floods, Hurricanes/Typhoons, Torandoes and Storm surges.
