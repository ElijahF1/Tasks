setwd("/Users/efama137/Desktop/Evolution/Tasks/Task_09")

library(diversitree)
library(ape)

?diversitree
?tree.bisse

transition_0to1<-0.4
transition_1to0<-0.1

speciation_0<-0.2
extinction_0<-0.1

speciation_1<-0.2
extinction_1<-0.1

maxN<-1e3
maxT<-50

Pars<-c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree<-tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree)

stateTable<-table(simTree$tip.state)
stateTable / sum(stateTable)
plot(simTree)
plot(stateTable)

#######

transition_0to1<-0.4
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-15

Pars<-c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree2<-tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
stateTable2<-table(simTree2$tip.state)
stateTable2 / sum(stateTable2)
plot(simTree2)
plot(stateTable2)

###

transition_0to1<-0.3
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-27

Pars<-c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree3<-tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
stateTable3<-table(simTree3$tip.state)
stateTable3 / sum(stateTable3)
plot(simTree3)
plot(stateTable3)

#####

transition_0to1<-0.4
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-5

Pars<-c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree4<-tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
stateTable4<-table(simTree4$tip.state)
stateTable4 / sum(stateTable4)
plot(simTree4)
plot(stateTable4)

#####

transition_0to1<-0.4
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<-0.1
speciation_1<-0.2
extinction_1<-0.1
maxN<-1e3
maxT<-60

Pars<-c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree5<-tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
stateTable5<-table(simTree5$tip.state)
stateTable5 / sum(stateTable5)
plot(simTree5)
plot(stateTable5)

#####

StateFreq<-frequency(stateTable5, stateTable4, stateTable3, stateTable2, stateTable)
plot(StateFreq)
pdf("09_plot.pdf")
boxplot(StateFreq ~ maxT, par(las=3), par(cex.axis=0.5))
dev.off


#
