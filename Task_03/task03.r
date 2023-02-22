setwd("/Users/efama137/Desktop/Evolution/Tasks/Task_03")
5
A <- 5
A * 2
B <- c(A, 5, 5)
B

B * 2
'B' * 2

B <- c(A, 5, 5)
B * 2
C <- c(B, 6, 10)
C * 6

D <- c(1, 5, 10, 15, 20)
sum(D)
mean(D)
min(D)

E <- c(1, 5, 10, 15, 20)
#Which Element of E is 8 ?: None "interger(0)"
#Whihc element of E is > 8 ?: 10,15,20 "[1] 3 4 5"
which (E == 8)
which (E > 8)
E[which (E > 8)]

trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
#Q?: what is the meaning of "1e6" and "rnrom"

Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
#Compare samples, were populations different?: Yes, The numbers that popped up in values under samples 1 and 2 are different, However I am not sure what the specific numbers mean
boxplot(Sample1, Sample2)

individual1 <- c("B", "A")
individual2 <- c("A", "A")
Gam1 <- sample(individual1, 1)
Gam2 <- sample(individual2, 1)
#Q?: what is the purpose of the "1"
newBaby <- c(Gam1, Gam2)
#genotype of New Baby?: newBaby = B and A "chr [1:2] "B" "A""

isHet <- c()
for (i in 1:100) {
  Gam1 <- sample(individual1, 1)
  Gam2 <- sample(individual2, 1)
  newBaby <- c(Gam1, Gam2)
  isHet [i] <- Gam1 == Gam2
}
sum(isHet) / length(isHet)
#Is the fraction of the heterozygotes what you expected for ABxAA cross?: I expected .5 and got .49

source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
head(MatGrandma)
nrow(MatGrandma)
head(MatGrandpa)
nrow(MatGrandpa)
head(PatGrandma)
nrow(PatGrandpa)
#Q?: I dont quite understand either response (what are the 2 rows, what does the z mean)(does nrow give "10000" because we are modeling 10,000 loci?)
Alan <- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
nrow(Alan)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
head(Brenda)
nrow(Brenda)
Focus <- makeBaby(Brenda, Alan)
#Q?: Does the Female always go first? How would it effect the response if they were switched?
#Before looking at the ToMom object what should the number be?: around 0.5 "0.5"
ToMom <- length( grep("mom", Focus)) / length(Focus)
ToMomMom <- length(grep("grandma_mom", Focus)) / length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus)) / length(Focus)
#What is ToMomMom added to ToMomDad? What Does it represent?: 0.49, it represent the percent of DNA that is from the Gradparents of the moms side, or the %DNA from the mother.
#
Sibling_01 <- makeBaby(Brenda, Alan)
#
ToSib <- length(intersect(Focus, Sibling_01)) / length(Focus)
#
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan))) / length(Focus))
quantile(ManySiblings)
mean(ManySiblings)
hist(ManySiblings)
pdf("003_relatePlot.pdf")
#With a larger population of siblings from the same parents the range of relativity between children will be higher with more of them sharing similar genes.  
