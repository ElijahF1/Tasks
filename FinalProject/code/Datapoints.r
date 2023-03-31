#Note to Dr. Mitchell: Hypothesis and Analysis plan below
#hypothesis: Parakeets Individuals (Psittacula krameri) from different populations of different areas will show a difference in frequency of specific microsatalites.    
#Analysis plan: {The Data I've accumulated consists of microsatalites from the Psittacula krameri species. This type of data is very usefull in making evolutionary observations. 
  #Microsatalites are regions within the genome that do not produce proteins and therefore do not have any affect on phenotypical features. 
  #However, these areas mutate and change sequences at a much faster rate then sequences of DNA the code for phenotypical features. 
  #Therefore, they can provide insight on more recent variability within a population. 
  #In addition this data can be used to make assessments on multiple populations. For example this data can be used to asses migration of the species. 
  #Currently I have have two combines data sets the consist of microsatalite regions of individuals of the species as well as the population and area that individual comes from. 
  #I have ran a ANOVA test and Regression test on my Data. In addition I have created multiple plots the show the frequency of the regions within each population. 
  #I am planning on running a FST test which will allow me to see the genetic differentiation of specific microsatalite regions within each population.}

#NOTES. (please disregard personal notes below)
#NoteToSelf: {Admixture:refers to the process by which two or more previously separated populations interbreed and exchange genetic material.
  #Indivs from 2 seperete pop come in contact = offsprings which could inherit genetic traits from both populations.}
#Data consists of microsatalites. they (microsattilite) are location in the genome which what a loci is. the regions just arnt coding for anything. 
#microsatalite: regions of DNA that does not express as protein, it mutates at a much faster rate but it is mutating genes that are not expressed. Can be tracked because it mutates really fast. it allows you to asses variability among populations that might not be phenotypically diff.
#can be used to asses population: "we can see how this pop changed and became a population based on their microsatilite regions"
#why people look at them: you can look at allele s and alles freq 
#Example. Population:brussels, ID:B1a, Peq01: 199 = this indiv bird at the peq01 loci(microsatalite region) has 199 repeats at that Loci(Peq01). The repeat can be anything. it doesnt really matter as you are just looking at HOW MANY times it reapeated as a proxi for changes in the DNA. 
#BLANK COLUMN: In Data the blank column next to Peq01 the second chromosome. 
#FST test (0-1: 0 indicated no genetic differitation (compleatly similar based of microsatilite data), 1 indicates no similarities
#amova; analysis of molec varience. 


#CODE (below)
#DATA ORGANIZATION
setwd("/Users/efama137/Desktop/Evolution/Tasks/FinalProject/data")

Data1 <- read.csv("ProjectDataPoints1.csv")
head(Data1)

Data2 <- read.csv("ProjectDataPoints2.csv")
head(Data2)

library(ggplot2)
library(phytools)
library(dplyr)
library(hierfstat)
library(adegenet)

#Note: run mean() code with numbers (from data, even if coordinates) to double check that Data is in R correctly 
mean(Data1[,4]) 
mean(Data2[,3])
summary(Data1)
summary(Data2)

colnames(Data1)
colnames(Data2)
Data1<-subset(Data1, select = -c(Sample.site, Peq03, Peq04, Peq06, Peq08, Peq09, Peq11, Peq12, Peq19, Peq20, X.2, X.3, X.5, X.6, X.7, X.9, X.10, X.16, X.17))
Data2<-subset(Data2, select = -c(Peq07, X.3))

Data1<- Data1 %>%
  rename(Peq01a = Peq01,
         Peq01b = X,
         Peq02a = Peq02,
         Peq02b = X.1,
         Peq05a = Peq05,
         Peq05b = X.4,
         Peq10a = Peq10,
         Peq10b = X.8,
         Peq13a = Peq13,
         Peq13b = X.11,
         Peq14a = Peq14,
         Peq14b = X.12,
         Peq15a = Peq15,
         Peq15b = X.13,
         Peq17a = Peq17,
         Peq17b = X.14,
         Peq18a = Peq18,
         Peq18b = X.15)

Data2<- Data2 %>%
  rename(Peq01a = Peq01,
         Peq01b = X,
         Peq02a = Peq02,
         Peq02b = X.1,
         Peq05a = Peq05,
         Peq05b = X.2,
         Peq10a = Peq10,
         Peq10b = X.4,
         Peq13a = Peq13,
         Peq13b = X.5,
         Peq14a = Peq14,
         Peq14b = X.6,
         Peq15a = Peq15,
         Peq15b = X.7,
         Peq17a = Peq17,
         Peq17b = X.8,
         Peq18a = Peq18,
         Peq18b = X.9)

DATA<-rbind(Data1,Data2)
head(DATA)
summary(DATA)
mean(DATA[,4])


#STAT TESTS (Below)
DATAanova<-aov(Peq01a ~ POPULATION, data = DATA)
summary(DATAanova)
DATAlm<-lm(Peq01a ~ POPULATION, data=DATA)
summary(DATAlm)


#GRAPHS/PLOTS (Below)
#### Plot for the 'b' data
pdf("ProjectPeqBplot")
Pops <- unique(DATA$POPULATION)
isB <- grep("b", colnames(DATA))
counter <- 1
Cols <- rainbow(length(isB))

par(mar=c(5,4,1,1))
plot(0, 300, ylim=c(0, 300), xaxt="n", xlab="", ylab="", xlim=c(1,length(Pops)), type="n")
axis(1, at=1:length(Pops), labels=Pops, srt=90)

for (i in isB){
  MeansB <- tapply(DATA[,i], DATA$POPULATION, mean, na.rm=T)
  points(1:length(Pops), MeansB, pch=16, col=Cols[counter])
  counter <- counter + 1
}
dev.off()

#### Plot for the 'a' data
pdf("ProjectPeqAplot")
Pops <- unique(DATA$POPULATION)
isA <- grep("a", colnames(DATA))
counter <- 1
# Edit this to change colors
Cols <- rainbow(length(isA))

par(mar=c(5,4,1,1))
plot(0, 300, ylim=c(0, 300), xaxt="n", xlab="", ylab="", xlim=c(1,length(Pops)), type="n")
axis(1, at=1:length(Pops), labels=Pops, srt=90)

for (i in isA){
  MeansA <- tapply(DATA[,i], DATA$POPULATION, mean, na.rm=T)
  points(1:length(Pops), MeansA, pch=16, col=Cols[counter])
  counter <- counter + 1
}
dev.off()

pdf("ProjectBoxplotPeq01.pdf")
boxplot(Peq01a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
dev.off()

boxplot(Peq01a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq02a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq05a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq10a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq13a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq14a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq15a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq17a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq18a ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))

boxplot(Peq01b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq02b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq05b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq10b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq13b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq14b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq15b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq17b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))
boxplot(Peq18b ~ POPULATION, data = DATA, par(las=3), par(cex.axis=0.5))






#Work space

library(learnPopGen)
coalescent.plot(DATA)




#Below (Disregard below): Stuff prob not used in fianl peper
ggplot(DATA, aes(x=POPULATION, y=Peq01a)) + geom_boxplot()
ggplot(DATA, aes(x=POPULATION, y=Peq02)) + geom_boxplot()
ggplot(DATA, aes(x=POPULATION, y=Peq05)) + geom_boxplot()
ggplot(DATA, aes(x=POPULATION, y=Peq10)) + geom_boxplot()
ggplot(DATA, aes(x=POPULATION, y=Peq13)) + geom_boxplot()
ggplot(DATA, aes(x=POPULATION, y=Peq14)) + geom_boxplot()
ggplot(DATA, aes(x=POPULATION, y=Peq15)) + geom_boxplot()
ggplot(DATA, aes(x=POPULATION, y=Peq17)) + geom_boxplot()
ggplot(DATA, aes(x=POPULATION, y=Peq18)) + geom_boxplot()


ggplot(Data1, aes(x=Population, y=Peq01a)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq01)) + geom_boxplot()

ggplot(Data1, aes(x=Population, y=Peq02)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq02)) + geom_boxplot()

ggplot(Data1, aes(x=Population, y=Peq05)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq05)) + geom_boxplot()

ggplot(Data1, aes(x=Population, y=Peq10)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq10)) + geom_boxplot()

ggplot(Data1, aes(x=Population, y=Peq13)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq13)) + geom_boxplot()

ggplot(Data1, aes(x=Population, y=Peq14)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq14)) + geom_boxplot()

ggplot(Data1, aes(x=Population, y=Peq15)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq15)) + geom_boxplot()

ggplot(Data1, aes(x=Population, y=Peq17)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq17)) + geom_boxplot()

ggplot(Data1, aes(x=Population, y=Peq18)) + geom_boxplot()
ggplot(Data2, aes(x=POPULATION, y=Peq18)) + geom_boxplot()


Data1<-subset(Data1, select = -c(X))
Data1<-subset(Data1, select = -c(Sample.site, Peq03, Peq04, Peq06, Peq08, Peq09, Peq11, Peq12, Peq19, Peq20, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9, X.10, X.11, X.12, X.13, X.14, X.15, X.16, X.17))
Data2<-subset(Data2, select = -c(Peq07, X, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9))


