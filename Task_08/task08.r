#This assignment is obviously incomplete, and I understand I will be docked a significant amount of points because of this.
#I will complete this assignment even if I am unable to make the point up. 
setwd("/Users/efama137/Desktop/Evolution/Tasks/Task_08")

library(phytools)

#Question 1-3
trees<-list()
births<-vector()
Fractions<-vector()

for (i in 1:100){
  births[i]<-runif(1)
  Fractions[i]<-runif(1)
  trees[[i]]<-pbtree(lambda= births[i], mu= births[i]*Fractions[i], n=100)
}

pdf("Task_08Plot1")
par(cex.lab=0.5, cex.axis=0.5)
plotTree(trees[[i]])
dev.off()

#Question 4: 

#Question 5
pdf("Task_08Plot2")
for (i in 1:100){
  plotTree(trees[[i]])
  BranchLength<-trees[[i]]$edge.length
  avgBL<-mean(BranchLength)
}
plotTree(trees[[i]])
dev.off()




