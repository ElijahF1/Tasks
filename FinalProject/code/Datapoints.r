
#CODE (below)
###DATA ORGANIZATION
setwd("/Users/efama137/Desktop/Evolution/Tasks/FinalProject/data")

Data1 <- read.csv("ProjectDataPoints1.csv")
head(Data1)

Data2 <- read.csv("ProjectDataPoints2.csv")
head(Data2)

library(ggplot2)
library(phytools)
library(dplyr)
library(ape)
library(maps)
library(ade4)
library(hierfstat)
library(adegenet)
library(vegan)
library(poppr)

mean(Data1[,4]) 
mean(Data2[,3])
summary(Data1)
summary(Data2)

#Remove Differing Peqs between dataset
colnames(Data1)
colnames(Data2)
Data1<-subset(Data1, select = -c(Sample.site, Peq03, Peq04, Peq06, Peq08, Peq09, Peq11, Peq12, Peq19, Peq20, X.2, X.3, X.5, X.6, X.7, X.9, X.10, X.16, X.17))
Data2<-subset(Data2, select = -c(Peq07, X.3))

#Rename Peqs
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

#Combine the two data sets
DATA<-rbind(Data1,Data2)
head(DATA)
summary(DATA)
mean(DATA[,4])

#Remove N/A
TrueDATA<-na.omit(DATA)

#Remove 0s
TrueDATA1<-TrueDATA[rowSums(TrueDATA[, grep("Peq", colnames(TrueDATA))]==0)==0,]

#Remove populations less than 10 samples
DATAremovMars<-slice(TrueDATA1, -3, -2, -1)
DATAremovMars2<-slice(DATAremovMars, -531)
DATAremovAlg<-slice(DATAremovMars2, -90,-89,-88)
DATAremovTus<-slice(DATAremovAlg, -527)
TrueData2<-slice(DATAremovTus, -367, -366)


#Combining columns (Peq01a+Peq01b=Peq01)
library(tidyr)
CombCol1<-unite(TrueData2, "Peq01", Peq01a, Peq01b, remove=TRUE, sep = "_")
CombCol2<-unite(CombCol1, "Peq02", Peq02a, Peq02b, remove=TRUE, sep = "_")
CombCol5<-unite(CombCol2, "Peq05", Peq05a, Peq05b, remove=TRUE, sep = "_")
CombCol10<-unite(CombCol5, "Peq10", Peq10a, Peq10b, remove=TRUE, sep = "_")
CombCol13<-unite(CombCol10, "Peq13", Peq13a, Peq13b, remove=TRUE, sep = "_")
CombCol14<-unite(CombCol13, "Peq14", Peq14a, Peq14b, remove=TRUE, sep = "_")
CombCol15<-unite(CombCol14, "Peq15", Peq15a, Peq15b, remove=TRUE, sep = "_")
CombCol17<-unite(CombCol15, "Peq17", Peq17a, Peq17b, remove=TRUE, sep = "_")
CombPeqData<-unite(CombCol17, "Peq18", Peq18a, Peq18b, remove=TRUE, sep = "_")

###Separating Populations###
#SouthParis
DataSP<-slice(CombPeqData, -47:-528)
#NorthParis
Datanp<-slice(CombPeqData, -1:-46)
DataNP<-slice(Datanp, -42:-478)
#Barcelona
Databar<-slice(CombPeqData, -1:-87)
DataBAR<-slice(Databar, -39:-437)
#Heidelberg
Dataheid<-slice(CombPeqData, -1:-125)
Dataheid2<-slice(Dataheid,-30:-96)
DataHEID<-slice(Dataheid2, -128:-332)
#CaptiveStock
Datacs<-slice(CombPeqData, -1:-154)
DataCS<-slice(Datacs, -19:-370)
#Brussels
Databr<-slice(CombPeqData, -1:-172)
DataBR<-slice(Databr, -50:-352)
#Wiesbaden
Datawi<-slice(CombPeqData, -1:-319)
DataWI<-slice(Datawi, -32:-205)
#Bonn
Databo<-slice(CombPeqData, -1:-350)
DataBO<-slice(Databo, -16:-174)
#Seville
Datase<-slice(CombPeqData, -1:-365)
DataSE<-slice(Datase, -35:-159)
#GreaterLondon
Datagl<-slice(CombPeqData, -1:-399)
DataGL<-slice(Datagl, -85:-125)
#Rotterdam
DataRO<-slice(CombPeqData, -1:-483)

#HWE SouthParis
library(HardyWeinberg)
library(pegas)
genotype.dataSP<-DataSP[, grep("Peq", colnames(DataSP))]
class(genotype.dataSP)
if(!is.matrix(genotype.dataSP)){
  genotype.matrixSP<-as.matrix(genotype.dataSP)
} else{
  genotype.matrixSP<-genotype.dataSP
}
colnames(genotype.matrixSP)
colnames(genotype.matrixSP)<-gsub(".","_", colnames(genotype.matrixSP))
sep<-"_"
genindSP<-df2genind(genotype.matrixSP, sep=sep)
summary(genindSP)
hw.test(genindSP)

#HWE NorthParis
genotype.dataNP<-DataNP[, grep("Peq", colnames(DataNP))]
class(genotype.dataNP)
if(!is.matrix(genotype.dataNP)){
  genotype.matrixNP<-as.matrix(genotype.dataNP)
} else{
  genotype.matrixNP<-genotype.dataNP
}
colnames(genotype.matrixNP)
colnames(genotype.matrixNP)<-gsub(".","_", colnames(genotype.matrixNP))
sep<-"_"
genindNP<-df2genind(genotype.matrixNP, sep=sep)
summary(genindNP)
hw.test(genindNP)

#HWE Barcelona
genotype.dataBAR<-DataBAR[, grep("Peq", colnames(DataBAR))]
class(genotype.dataBAR)
if(!is.matrix(genotype.dataBAR)){
  genotype.matrixBAR<-as.matrix(genotype.dataBAR)
} else{
  genotype.matrixNP<-genotype.dataBAR
}
colnames(genotype.matrixBAR)
colnames(genotype.matrixBAR)<-gsub(".","_", colnames(genotype.matrixBAR))
sep<-"_"
genindBAR<-df2genind(genotype.matrixBAR, sep=sep)
summary(genindBAR)
hw.test(genindBAR)

#HWE Heidelberg
genotype.dataHEID<-DataHEID[, grep("Peq", colnames(DataHEID))]
class(genotype.dataHEID)
if(!is.matrix(genotype.dataHEID)){
  genotype.matrixHEID<-as.matrix(genotype.dataHEID)
} else{
  genotype.matrixHEID<-genotype.dataHEID
}
colnames(genotype.matrixHEID)
colnames(genotype.matrixHEID)<-gsub(".","_", colnames(genotype.matrixHEID))
sep<-"_"
genindHEID<-df2genind(genotype.matrixHEID, sep=sep)
summary(genindHEID)
hw.test(genindHEID)

#HWE CaptiveStock
genotype.dataCS<-DataCS[, grep("Peq", colnames(DataCS))]
class(genotype.dataCS)
if(!is.matrix(genotype.dataCS)){
  genotype.matrixCS<-as.matrix(genotype.dataCS)
} else{
  genotype.matrixCS<-genotype.dataCS
}
colnames(genotype.matrixCS)
colnames(genotype.matrixCS)<-gsub(".","_", colnames(genotype.matrixCS))
sep<-"_"
genindCS<-df2genind(genotype.matrixCS, sep=sep)
summary(genindCS)
hw.test(genindCS)

#HWE Brussels
genotype.dataBR<-DataBR[, grep("Peq", colnames(DataBR))]
class(genotype.dataBR)
if(!is.matrix(genotype.dataBR)){
  genotype.matrixBR<-as.matrix(genotype.dataBR)
} else{
  genotype.matrixBR<-genotype.dataBR
}
colnames(genotype.matrixBR)
colnames(genotype.matrixBR)<-gsub(".","_", colnames(genotype.matrixBR))
sep<-"_"
genindBR<-df2genind(genotype.matrixBR, sep=sep)
summary(genindBR)
hw.test(genindBR)

#HWE Wiesbaden
genotype.dataWI<-DataWI[, grep("Peq", colnames(DataWI))]
class(genotype.dataWI)
if(!is.matrix(genotype.dataWI)){
  genotype.matrixWI<-as.matrix(genotype.dataWI)
} else{
  genotype.matrixWI<-genotype.dataWI
}
colnames(genotype.matrixWI)
colnames(genotype.matrixWI)<-gsub(".","_", colnames(genotype.matrixWI))
sep<-"_"
genindWI<-df2genind(genotype.matrixWI, sep=sep)
summary(genindWI)
hw.test(genindWI)


#HWE Bonn
genotype.dataBO<-DataBO[, grep("Peq", colnames(DataBO))]
class(genotype.dataBO)
if(!is.matrix(genotype.dataBO)){
  genotype.matrixBO<-as.matrix(genotype.dataBO)
} else{
  genotype.matrixBO<-genotype.dataBO
}
colnames(genotype.matrixBO)
colnames(genotype.matrixBO)<-gsub(".","_", colnames(genotype.matrixBO))
sep<-"_"
genindBO<-df2genind(genotype.matrixBO, sep=sep)
summary(genindBO)
hw.test(genindBO)

#HWE Seville
genotype.dataSE<-DataSE[, grep("Peq", colnames(DataSE))]
class(genotype.dataSE)
if(!is.matrix(genotype.dataSE)){
  genotype.matrixSE<-as.matrix(genotype.dataSE)
} else{
  genotype.matrixSE<-genotype.dataSE
}
colnames(genotype.matrixSE)
colnames(genotype.matrixSE)<-gsub(".","_", colnames(genotype.matrixSE))
sep<-"_"
genindSE<-df2genind(genotype.matrixSE, sep=sep)
summary(genindSE)
hw.test(genindSE)

#HWE GreaterLondon
genotype.dataGL<-DataGL[, grep("Peq", colnames(DataGL))]
class(genotype.dataGL)
if(!is.matrix(genotype.dataGL)){
  genotype.matrixGL<-as.matrix(genotype.dataGL)
} else{
  genotype.matrixGL<-genotype.dataGL
}
colnames(genotype.matrixGL)
colnames(genotype.matrixGL)<-gsub(".","_", colnames(genotype.matrixGL))
sep<-"_"
genindGL<-df2genind(genotype.matrixGL, sep=sep)
summary(genindGL)
hw.test(genindGL)

#HWE Rotterdam
genotype.dataRO<-DataRO[, grep("Peq", colnames(DataRO))]
class(genotype.dataRO)
if(!is.matrix(genotype.dataRO)){
  genotype.matrixRO<-as.matrix(genotype.dataRO)
} else{
  genotype.matrixRO<-genotype.dataRO
}
colnames(genotype.matrixRO)
colnames(genotype.matrixRO)<-gsub(".","_", colnames(genotype.matrixRO))
sep<-"_"
genindRO<-df2genind(genotype.matrixRO, sep=sep)
summary(genindRO)
hw.test(genindRO)

###HWE PEQ01 all populations
CombPeq01Data<-CombPeqData[,-4:-11]
library(HardyWeinberg)
library(pegas)
genotype.data<-CombPeq01Data[, grep("Peq", colnames(CombPeq01Data))]
genotype.matrix<-as.matrix(genotype.data)
genind<-df2genind(genotype.data, sep = "_")
summary(genind)
class(genotype.data)
if(!is.matrix(genotype.data)){
  genotype.matrix<-as.matrix(genotype.data)
} else{
  genotype.matrix<-genotype.data
}
colnames(genotype.matrix)
colnames(genotype.matrix)<-gsub(".","_", colnames(genotype.matrix))
sep<-"_"
genind<-df2genind(genotype.matrix, sep=sep)
summary(genind)
hw.test(genind)

###HWE all PEQs all populations
genotype.data<-CombPeqData[, grep("Peq", colnames(CombPeqData))]
genotype.matrix<-as.matrix(genotype.data)
genind<-df2genind(genotype.data, sep = "_")
summary(genind)
class(genotype.data)
if(!is.matrix(genotype.data)){
  genotype.matrix<-as.matrix(genotype.data)
} else{
  genotype.matrix<-genotype.data
}
colnames(genotype.matrix)
colnames(genotype.matrix)<-gsub(".","_", colnames(genotype.matrix))
sep<-"_"
genind<-df2genind(genotype.matrix, sep=sep)
summary(genind)
hw.test(genind)

#Heterozygosity across populations
library(knitr)
CombPeqMatrix<-as.matrix(CombPeqData)
row.names(CombPeqMatrix)<-CombPeqData$POPULATION
CombPeqDataDF<-data.frame(CombPeqMatrix)
genotype.data<-CombPeqDataDF[, grep("Peq", colnames(CombPeqDataDF))]
genotype.matrix<-as.matrix(genotype.data)
genind<-df2genind(genotype.matrix, sep = "_", pop=row.names(genotype.matrix))
popNames(genind) = gsub("[^a-zA-z]","", popNames(genind))
popNames(genind)

GenindBasic=basic.stats(genind, diploid =TRUE)
ObsHet=apply(GenindBasic$Ho, MARGIN=2, FUN=mean, na.rm=TRUE)%>%
  round(digits = 2)
ExpHet=apply(GenindBasic$Hs, MARGIN=2, FUN=mean, na.rm=TRUE)%>%
  round(digits = 2)
Heterozygosity<-data.frame(Observed.Heterozygosity = ObsHet, Expected.Heterozygosity = ExpHet)
Heterozygosity$Population<-c("SouthofParis", "NorthofParis", "Barcelona", "Heidelberg", "CaptiveStock", "Brussels", "Hiedelberg", "Wiesbaden", "Bonn", "Seville", "GreaterLondon", "Rotterdam")
pdf("HeterozygosityAllPops.pdf")
ggplot(Heterozygosity, aes(x=Population))+
  geom_bar(aes(y=Expected.Heterozygosity, fill= "Expected"), stat = "identity", position="dodge", alpha=0.5) +
  geom_bar(aes(y=Observed.Heterozygosity, fill="Observed"), stat="identity", position = "dodge", alpha=0.5) +
  labs(x="Populations", y="Heterozygosity", title= "Heterozygosity of Populations", fill="Obs/Exp" )+
  theme(axis.text.x =element_text(angle=90), plot.title = element_text(hjust = 0.5))
dev.off()








#GRAPHS/PLOTS (Below)
#### Plot for the 'b' data
pdf("ProjectPeqBplot.pdf")
Pops <- unique(DATA$POPULATION)
isB <- grep("b", colnames(DATA))
counter <- 1
Cols <- rainbow(length(isB))

par(mar=c(5,4,1,1), las=2)
par(cex.axis=0.5)
plot(0, 300, ylim=c(0, 300), xaxt="n", xlab="", ylab="Avg # Repeats", main="(Peq_b): Avg # Repeats Per Population", xlim=c(1,length(Pops)), type="n")
axis(1, at=1:length(Pops), labels=Pops, srt=90)

for (i in isB){
  MeansB <- tapply(DATA[,i], DATA$POPULATION, mean, na.rm=T)
  points(1:length(Pops), MeansB, pch=16, col=Cols[counter])
  counter <- counter + 1
}
legend("topright", legend=colnames(DATA)[isB], title="PeqB", col=Cols, pch=16, cex=0.8)
dev.off()

#### Plot for the 'a' data
pdf("ProjectPeqAplot.pdf")
Pops <- unique(DATA$POPULATION)
isA <- grep("a", colnames(DATA))
counter <- 1
Cols <- rainbow(length(isA))

par(mar=c(5,4,1,1), las=2)
par(cex.axis=0.5)
plot(0, 300, ylim=c(0, 300), xaxt="n", xlab="", ylab="Avg # Repeats",main="(Peq_a): Avg # Repeats Per Population", xlim=c(1,length(Pops)), type="n")
axis(1, at=1:length(Pops), labels=Pops, srt=90)

for (i in isA){
  MeansA <- tapply(DATA[,i], DATA$POPULATION, mean, na.rm=T)
  points(1:length(Pops), MeansA, pch=16, col=Cols[counter])
  counter <- counter + 1
}
legend("topright", legend=colnames(DATA)[isA], title="PeqA", col=Cols, pch=16, cex=0.8)
dev.off()






