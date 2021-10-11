moth.data <- read.csv("TimNewboldMothDataPublicRelease.csv")

sink(paste0("Moths.Rmd"))

cat('
    
---
title: Moth Records
date: "`r Sys.Date()`"
author: "Tim Newbold"
output:
  rmdformats::downcute:
    code_folding: show
    self_contained: true
    thumbnails: false
    lightbox: true
---   

```{r,echo=FALSE,results=FALSE}
moth.data <- read.csv("TimNewboldMothDataPublicRelease.csv")

total.individs <- sum(moth.data$NumberCaught,na.rm=TRUE)

total.species <- length(unique(moth.data$Species))

total.traps <- length(unique(moth.data$TrapNumber))

all.yrs <- seq(from=min(moth.data$Year),to=max(moth.data$Year))
```
    
# Overview

I have been trapping moths in Cambridgeshire since 8th August 2020.

Since, then I have run `r total.traps` traps, and caught `r total.individs` individual moths, of `r total.species` different species.

```{r,echo=FALSE,results=TRUE}

traps <- unique(moth.data$TrapNumber)

sp.accum <- sapply(X = 1:length(traps),
                   FUN = function(i){
                     sub.data <- moth.data[(moth.data$TrapNumber<=traps[i]),]
                     
                     cum.sp <- length(unique(tolower(na.omit(sub.data$Species))))
                     
                   })

par(mar=c(3.2,3.2,0.5,0.2))
par(mgp=c(2.0,0.2,0))
par(tck=-0.01)
par(las=1)

plot(1:length(sp.accum),sp.accum,type="l",bty="l",col="#A40122",
     lwd=3,xlab="Trap number",ylab="Cumulative species count",
     xaxt="n")
axis(side = 1,at = 1:length(sp.accum))
```
\n
```{r,echo=FALSE,results=TRUE}

TrapCountsByMonth <- tapply(X = moth.data$TrapNumber,
                            INDEX = moth.data$Year_Month,
                            FUN = function(x) return(
                              length(unique(x))))

barplot(height = TrapCountsByMonth,col="#A40122",
        xlab="Month",ylab="No. traps")
box(bty="l")
```
\n
```{r,echo=FALSE,results=TRUE}
trap.sums <- data.frame(TotalCaught=tapply(X = moth.data$NumberCaught,
                                           INDEX = moth.data$TrapNumber,
                                           FUN = sum,na.rm=TRUE))

trap.sums$TrapNumber <- row.names(trap.sums)

trap.sums$Month <- moth.data$Month[match(trap.sums$TrapNumber,moth.data$TrapNumber)]

trap.sums$Year <- moth.data$Year[match(trap.sums$TrapNumber,moth.data$TrapNumber)]

colour.pal <- viridis::viridis(n = length(unique(trap.sums$Year)))

ymax <- max(tapply(
  X = trap.sums$TotalCaught,
  INDEX = paste(trap.sums$Month,trap.sums$Year),
  FUN = median))

par(mar=c(3.2,3.2,0.5,0.2))
par(mgp=c(2.0,0.2,0))
par(tck=-0.01)
par(las=1)

plot(9e99,9e99,xlim=c(0.5,12.5),ylim=c(0,ymax),xlab="Month",ylab="Median Individuals Caught")

invisible(mapply(FUN = function(yr.data,colour){
  
  month.avgs <- tapply(X = yr.data$TotalCaught,INDEX = yr.data$Month,FUN = median)
  
  points(names(month.avgs),month.avgs,type="b",pch=16,col=colour)
  
},split(x = trap.sums,f = trap.sums$Year),as.list(colour.pal)))

legend(x = 1,y = ymax,legend = unique(trap.sums$Year),bty="n",lty=1,col=colour.pal)

```
\n
```{r,echo=FALSE,results=TRUE}

species.counts <- data.frame(SpeciesNumber=tapply(
  X = moth.data$Species,INDEX = moth.data$Year_Month,FUN = function(spp){
    length(unique(spp))}))

species.counts$Year <- as.integer(sapply(
  X = row.names(species.counts),
  FUN = function(x) return(strsplit(x,"_")[[1]][1])))

species.counts$Month <- as.integer(sapply(
  X = row.names(species.counts),
  FUN = function(x) return(strsplit(x,"_")[[1]][2])))

ymax <- max(tapply(
  X = species.counts$SpeciesNumber,
  INDEX = paste(species.counts$Month,species.counts$Year),
  FUN = median))

par(mar=c(3.2,3.2,0.5,0.2))
par(mgp=c(2.0,0.2,0))
par(tck=-0.01)
par(las=1)

plot(9e99,9e99,xlim=c(0.5,12.5),ylim=c(0,ymax),xlab="Month",ylab="Total species caught")

invisible(mapply(FUN = function(yr.data,colour){
  
  points(yr.data$Month,yr.data$SpeciesNumber,type="b",pch=16,col=colour)
  
},split(x = species.counts,f = species.counts$Year),as.list(colour.pal)))

legend(x = 1,y = ymax,legend = unique(trap.sums$Year),bty="n",lty=1,col=colour.pal)
```

\n
')

cat('# By Month\n')

months <- sort(unique(moth.data$Month))

invisible(sapply(X = months,FUN = function(mo){
  
cat(paste0('\n## ',month.name[mo],'\n'))

cat('
```{r,echo=FALSE,results=TRUE}
')

cat(paste0('month.data <- moth.data[(moth.data$Month==',mo,'),]'))

cat('
head(month.data)
```
')

}))

cat('\n')

cat('# By Year\n')

years <- sort(unique(moth.data$Year))

invisible(sapply(X = years,FUN = function(yr){

cat(paste0('\n## ',yr,'\n'))
  
}))

cat('\n')

cat('# By Species\n')

species <- sort(unique(moth.data$Species))

invisible(sapply(X = species,FUN = function(sp){
  
cat(paste0('\n## ',sp,'\n'))
  
cat('
```{r,echo=FALSE,results=TRUE}
')

cat(paste0('species.data <- moth.data[which(moth.data$Species=="',sp,'"),]'))

cat('
total.caught <- sum(species.data$NumberCaught,na.rm=TRUE)
n.traps <- length(unique(species.data$TrapNumber))
first.year <- min(species.data$Year,na.rm=TRUE)
last.year <- max(species.data$Year,na.rm=TRUE)

species.data$Year <- factor(x = species.data$Year,levels = all.yrs)
```
')

cat(paste0('In total, I have caught `r total.caught` individuals of ',sp,', in `r n.traps` traps, between `r first.year` and `r last.year`.'))

cat('

```{r,echo=FALSE,results=TRUE}
yr.avg.catches <- tapply(X = species.data$NumberCaught,INDEX = species.data$Year,FUN = mean)
yr.avg.catches[is.na(yr.avg.catches)] <- 0

par(mar=c(3.2,3.2,0.5,0.2))
par(mgp=c(2.0,0.2,0))
par(tck=-0.01)
par(las=1)

plot(all.yrs,yr.avg.catches,xaxp=c(min(all.yrs),max(all.yrs),length(all.yrs)-1),xlab="Year",ylab="Average Catch",pch=16,type="b",col="#A40122",bty="l")

dens <- hist(species.data$Month,breaks=seq(from=0.5,to=12.5,by=1),plot=FALSE)
      
barplot(dens$density,space = 0,border=NA,col="#A40122",ylab="Density")
axis(side = 1,at = seq(from=0.5,to=11.5,length.out=12),labels = month.abb)
title(xlab="Month")
box(bty="l")
```
')
  
}))

sink()
