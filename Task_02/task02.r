setwd('/Users/efama137/Desktop/Evolution/Tasks/Task_02')
Data1<-read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
Data2<-read.csv('http://jonsmitchell.com/data/cyrus.csv', stringsAsFactors=F)
write.csv(Data1, 'rawdata.csv', quote=F)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1[1,]
Data1[2,]
Data1[1:3,]
Data1[1:3, 4]
Data1[1:5, 1:3]
Feeds<-which(Data1[,9] == 'bottle')
barenMilk<-Data1[Feeds,]
head(barenMilk)
Feeds<-which(Data1[,'event'] == 'bottle')
Feeds<-which(Data1$event == 'bottle')
dayID<-apply(Data1, 1, function(x) paste(x[1:3], collapse='-'))
dateID<-sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data1$age<-dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
beren2<-Data1
beren3<-beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
#
#
Feeds<-which(beren3$event == "bottle")
avgMILK<-mean(beren3$value[Feeds])
avgFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds<-tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab = "amount of milk consumed (oz)" )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
dev.off()
source("http://jonsmitchell.com/code/plotFxn02b.R")

<Beren start>
Mass <- which(Data1[,9] == 'trait_mass')
berenMass <- Data1[Mass,]
head(berenMass)
Mass <- which(Data1[,'event'] == 'trait_mass')
Mass<-which(Data1$event == 'trait_mass')
dayID<-apply(Data1, 1, function(x) paste(x[1:3], collapse='-'))
dateID<-sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data1$age<-dateID - dateID[which(Data1$event == 'birth')]
head(Data1)
beren2<-Data1
beren3<-beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Mass <- which(beren3$event == "trait_mass")
avgMASS <- mean(beren3$value[Mass])
avgmass<-tapply(beren3$value[Mass], beren3$age[Mass], mean)
varmass<-tapply(beren3$value[Mass], beren3$age[Mass], var)
totalmass<-tapply(beren3$value[Mass], beren3$age[Mass], sum)
numMass<-tapply(beren3$value[Mass], beren3$age[Mass], length)
cor(beren3$value[Mass], beren3$age[Mass])
cor.test(beren3$value[Mass], beren3$age[Mass])
berenCor <- cor.test(beren3$value[Mass], beren3$age[Mass])
summary(berenCor)
berenANOVA <- aov(beren3$value[Mass] ~ beren3$age[Mass])
#pdf
boxplot( beren3$value[Mass] ~ beren3$age[Mass], xlab= "Beren Age", ylab = "Beren Mass (kg)" )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalmass)), totalmass, type="b", pch=16, xlab="Beren Age", ylab="Beren Mass (kg)")
abline(h=mean(totalmass), lty=2, col='red')
#points
#dev.off()
pdf("r02b-totalMassByAge.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalmass)), totalmass, type="b", pch=16, xlab="Beren Age", ylab="Beren Mass (kg)")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()

<Cyrus start>
cMass <- which(Data2[,9] == 'trait_mass')
cyrusMass <- Data2[cMass,]
head(cyrusMass)
cMass <- which(Data2[,'event'] == 'trait_mass')
cMass<-which(Data2$event == 'trait_mass')
dayID<-apply(Data2, 1, function(x) paste(x[1:3], collapse='-'))
dateID<-sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2022-04-12")
Data2$age<-dateID - dateID[which(Data2$event == 'birth')]
head(Data2)
cyrus2<-Data2
cyrus3<-cyrus2[order(cyrus2$age) ,]
write.csv(cyrus3, 'cyrus_new.csv', quote=F, row.names=FALSE)
cMass <- which(cyrus3$event == "trait_mass")
avgcMASS <- mean(cyrus3$value[cMass])
avgcmass<-tapply(cyrus3$value[cMass], cyrus3$age[cMass], mean)
varmass<-tapply(cyrus3$value[cMass], cyrus3$age[cMass], var)
totalcmass<-tapply(cyrus3$value[cMass], cyrus3$age[cMass], sum)
numcMass<-tapply(cyrus3$value[cMass], cyrus3$age[cMass], length)
cor(cyrus3$value[cMass], cyrus3$age[cMass])
cor.test(cyrus3$value[cMass], cyrus3$age[cMass])
cyrusCor <- cor.test(cyrus3$value[cMass], cyrus3$age[cMass])
summary(cyrusCor)
cyrusANOVA <- aov(cyrus3$value[cMass] ~ cyrus3$age[cMass])
#pdf
boxplot( cyrus3$value[cMass] ~ cyrus3$age[cMass], xlab= "Cyrus Age", ylab = "Cyrus Mass (g)" )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalcmass)), totalcmass, type="b", pch=16, xlab="Cyrus Age", ylab="Cyrus Mass (g)")
abline(h=mean(totalcmass), lty=2, col='blue')
pdf("r02b-totalMassByAgeC.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalcmass)), totalcmass, type="b", pch=16, xlab="Cyrus Age", ylab="Cyrus Mass (g)")
abline(h=mean(totalcmass), lty=2, col='blue')
dev.off()

boxplot( beren3$value[Mass] ~ beren3$age[Mass], xlab= "Beren Age", ylab = "Beren Mass (kg)" )
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalmass)), totalmass, type="b", pch=16, xlab="Beren Age", ylab="Beren Mass (kg)")
abline(h=mean(totalmass), lty=2, col='red')
points(cyrus3$age[cMass], cyrus3$value[cMass] / 1000, pch=16, col='blue' )
dev.off()

