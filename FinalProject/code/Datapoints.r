# 1 sentence hypothesis requested in task_06: Parakeets Individuals (Psittacula krameri) from different populations of different areas will show a difference in frequency of specific loci.    

#NoteToSelf: {Admixture:refers to the process by which two or more previously separated populations interbreed and exchange genetic material.
  #Indivs from 2 seperete pop come in contact = offsprings which could inherit genetic traits from both populations.}
  

setwd("/Users/efama137/Desktop/Evolution/Tasks/FinalProject/data")

Data1 <- read.csv("ProjectDataPoints.csv")
head(Data1)

Data2 <- read.csv("ProjectDataPoints2.csv")
head(Data2)

#run mean() code with numbers (from data, even if coordinates) to double check that Data is in R correctly 
mean(Data2[,3])


