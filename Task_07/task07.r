setwd("/Users/efama137/Desktop/Evolution/Tasks/Task_07")
#load phytools and ape
library(phytools)
library(ape)
text.string<-
  "(((((((cow, pig), whale),(bat,(lemur,human))),(robin,iguana)),coelacanth),(gold_fish, trout)),shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)

nodelabels(frame="circle", bg='white', cex=1)
#Question1: The goldfish and Shark are more closely related
  #Between Goldfish and Sharks there are 3 nodes "13","14" and "23"

vert.tree
#It states "No branch length"

str(vert.tree)

tree<-read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree,offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)

tree$tip.label
#finding edge matrix: 
tree$edge

AnolisTree<- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))

par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))

tipEdges<-which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths<-AnolisTree$edge.length
names(Lengths)<-AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]

plot(AnolisTree, cex=0.25)
Labs<-sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)

?plot.phylo
plot(tree, tip.label="")
plot(tree, type="fan")
plot(tree, tip.color="red")

summary(AnolisTree$edge.length)
shortest_edge<-min(AnolisTree$edge.length)
Ntip(AnolisTree)
shortest_edge_which <- which(AnolisTree$edge.length == shortest_edge) 
AnolisTree2<-drop.tip(AnolisTree, tip = 82L)
plot(AnolisTree2, cex = 0.25)
Ntip(AnolisTree2)

ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
fit.bd(AnolisTree, rho = 0.2)

pdf("007_plot01.pdf")
ltt(AnolisTree2)
abline(0, 1, lwd=2, col='red', lty=2)
fit.bd(AnolisTree2, rho = 0.2)
dev.off()

#Here the packet says to look at the last R assignment to see how to us the "read.tree()" function. However R assignments 5 and 6 do not show you how. Not sure if the below is correct.
tree <- read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan", cex=0.25)

data<-read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names=1)
#Question2: The "data" object is svl numeric data. It has 82 observations of 1 variable.
svl<-setNames(data$svl, rownames(data))

Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
#Question3: CI95 is type of variance 
#Question4: ?

par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)

tiplabels(pch=16, cex=0.25*svl[tree$tip.label])

nodelabels(pch=16, cex=0.25*Ancestors$ace)

obj<-contMap(tree, svl, plot=F)
plot(obj, legend=0.7*max(nodeHeights(tree)),sig=2, fsize=c(0.7, 0.9))

fossilData<-data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c(
  "Anolis_anliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii
  ", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorcyanus
  ", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_
  angusticeps", "Anolis_angusticeps"))
#Question5:
fossilNodes<-c()
nodeN<-c()
for(i in 1:nrow(fossilData)){
  Node<-fastMRCA(AnolisTree, fossilData[i, "tip1"], fossilData[i, "tip2"])
  fossilNodes[i]<-fossilData[i, "svl"]
  nodeN[i]<-Node
  names(fossilNodes)<-nodeN 
}


Ancestors_withFossils<-fastAnc(AnolisTree, svl, anc.state=fossilNodes, CI=TRUE, var+TRUE)






