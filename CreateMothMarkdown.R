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
')

cat('# By Month\n')

months <- sort(unique(moth.data$Month))

sapply(X = months,FUN = function(mo){
  
cat(paste0('\n## ',month.name[mo],'\n'))
  
})

cat('\n')

cat('# By Year\n')

years <- sort(unique(moth.data$Year))

sapply(X = years,FUN = function(yr){

cat(paste0('\n## ',yr,'\n'))
  
})

cat('\n')

cat('# By Species\n')

species <- sort(unique(moth.data$Species))

sapply(X = species,FUN = function(sp){
  
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
  
})

sink()
