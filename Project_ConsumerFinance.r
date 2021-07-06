# Title: EL paper analysis script

# Description: This program creates the descriptives and
# conducts the analysis for the EL paper

# Author: Matt Burke

# Last modified: 26/04/2019


#setwd("C:/Users/mattb/Documents/University/PhD/Project_ConsumerFinance")
#setwd("C:/Users/mattb/OneDrive - University of Bradford/UEA/PhD/Project_ConsumerFinance")
setwd("C:/Users/mattb/Documents/GitHub/ConsumerFin_researchpaper")

library(psych)
library(multcomp)

lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}

df1<-read.csv("data.csv", header=TRUE)

df1[3:12] <- lapply(df1[3:12], as.numeric)

df1$TermstoWords <- df1$Terms/df1$WordCount
df1$PCcomplex<-df1$ComplexCount/df1$WordCount
df1$SPCcomplex<-df1$SuperComplexCount/df1$WordCount
df1$WordsperSentence<-df1$WordCount/df1$SentCount

### Table 2 Summary Statistics

payday <- df1[df1$Category==0,]
personal <- df1[df1$Category==1,]
credit <- df1[df1$Category==2,]

table_2 <- rbind( c(  mean(payday$FogIndex)   ,  sd(payday$FogIndex)  ,  min(payday$FogIndex)  ,  max(payday$FogIndex) ) , 
					c(mean(payday$TermstoWords)   ,  sd(payday$TermstoWords)  ,  min(payday$TermstoWords)  ,  max(payday$TermstoWords)) , 
					c(mean(payday$PCcomplex)   ,  sd(payday$PCcomplex)  ,  min(payday$PCcomplex)  ,  max(payday$PCcomplex)) , 
					c(mean(payday$FinancetoComplex)   ,  sd(payday$FinancetoComplex)  ,  min(payday$FinancetoComplex)  ,  max(payday$FinancetoComplex)) , 
					c(mean(payday$WordsperSentence)   ,  sd(payday$WordsperSentence)  ,  min(payday$WordsperSentence)  ,  max(payday$WordsperSentence)) , 
					c(mean(payday$WordCount)   ,  sd(payday$WordCount)  ,  min(payday$WordCount)  ,  max(payday$WordCount)) , 
					c(	mean(personal$FogIndex)   ,  sd(personal$FogIndex)  ,  min(personal$FogIndex)  ,  max(personal$FogIndex)) , 
					c(mean(personal$TermstoWords)   ,  sd(personal$TermstoWords)  ,  min(personal$TermstoWords)  ,  max(personal$TermstoWords)) , 
					c(mean(personal$PCcomplex)   ,  sd(personal$PCcomplex)  ,  min(personal$PCcomplex)  ,  max(personal$PCcomplex)) , 
					c(mean(personal$FinancetoComplex)   ,  sd(personal$FinancetoComplex)  ,  min(personal$FinancetoComplex)  ,  max(personal$FinancetoComplex)) , 
					c(mean(personal$WordsperSentence)   ,  sd(personal$WordsperSentence)  ,  min(personal$WordsperSentence)  ,  max(personal$WordsperSentence)) , 
					c(mean(personal$WordCount)   ,  sd(personal$WordCount)  ,  min(personal$WordCount)  ,  max(personal$WordCount)) , 
					c(	mean(credit$FogIndex)   ,  sd(credit$FogIndex)  ,  min(credit$FogIndex)  ,  max(credit$FogIndex) ) , 
					c(mean(credit$TermstoWords)   ,  sd(credit$TermstoWords)  ,  min(credit$TermstoWords)  ,  max(credit$TermstoWords)) , 
					c(mean(credit$PCcomplex)   ,  sd(credit$PCcomplex)  ,  min(credit$PCcomplex)  ,  max(credit$PCcomplex)) , 
					c(mean(credit$FinancetoComplex)   ,  sd(credit$FinancetoComplex)  ,  min(credit$FinancetoComplex)  ,  max(credit$FinancetoComplex)) , 
					c(mean(credit$WordsperSentence)   ,  sd(credit$WordsperSentence)  ,  min(credit$WordsperSentence)  ,  max(credit$WordsperSentence)) , 
					c(mean(credit$WordCount)   ,  sd(credit$WordCount)  ,  min(credit$WordCount)  ,  max(credit$WordCount)))
options(scipen=2)
table_2 <- as.data.frame(table_2)
table_2

### Table 3 Hypothesis test

payday.t <- t.test(payday$FogIndex, mu = 12)[1]
payday.p <- t.test(payday$FogIndex, mu = 12)[3]
personal.t <- t.test(personal$FogIndex, mu = 12)[1]
personal.p <- t.test(personal$FogIndex, mu = 12)[3]
credit.t <- t.test(credit$FogIndex, mu = 12)[1]
credit.p <- t.test(credit$FogIndex, mu = 12)[3]

table_3 <- rbind( c(payday.t , payday.p),
					c(personal.t, personal.p),
					c(credit.t, credit.p))
rownames(table_3)<-c("payday", "personal", "credit")
table_3

### Table 4 Panel 1

df1$Category = factor(df1$Category)

lm.fit.Fog <- lm(FogIndex~Category, data=df1)
lm.fit.TermstoWords <- lm(TermstoWords~Category, data=df1)
lm.fit.PCcomplex <- lm(PCcomplex~Category, data=df1)
lm.fit.FinancetoComplex <- lm(FinancetoComplex~Category, data=df1)
lm.fit.WordsperSentence <- lm(WordsperSentence~Category, data=df1)
lm.fit.WordCount <- lm(WordCount~Category, data=df1)

FogIndex.F <- summary(lm.fit.Fog)$fstatistic[1]
FogIndex.p <- lmp(lm.fit.Fog)
TermstoWords.F <- summary(lm.fit.TermstoWords)$fstatistic[1]
TermstoWords.p <- lmp(lm.fit.TermstoWords)
PCcomplex.F <- summary(lm.fit.PCcomplex)$fstatistic[1]
PCcomplex.p <- lmp(lm.fit.PCcomplex)
FinancetoComplex.F <- summary(lm.fit.FinancetoComplex)$fstatistic[1]
FinancetoComplex.p <- lmp(lm.fit.FinancetoComplex)
WordsperSentence.F <- summary(lm.fit.WordsperSentence)$fstatistic[1]
WordsperSentence.p <- lmp(lm.fit.WordsperSentence)
WordCount.F <- summary(lm.fit.WordCount)$fstatistic[1]
WordCount.p <- lmp(lm.fit.WordCount)

Table_4_1 <- rbind( c(FogIndex.F, FogIndex.p),    
					c(TermstoWords.F, TermstoWords.p),
					c(PCcomplex.F, PCcomplex.p),
					c(FinancetoComplex.F, FinancetoComplex.p),
					c(WordsperSentence.F, WordsperSentence.p),
					c(WordCount.F, WordCount.p))
Table_4_1

### Table 4 Panel 2

summary(glht(lm.fit.Fog, mcp(Category="Tukey")))
summary(glht(lm.fit.TermstoWords, mcp(Category="Tukey")))
summary(glht(lm.fit.PCcomplex, mcp(Category="Tukey")))
summary(glht(lm.fit.FinancetoComplex, mcp(Category="Tukey")))
summary(glht(lm.fit.WordsperSentence, mcp(Category="Tukey")))
summary(glht(lm.fit.WordCount, mcp(Category="Tukey")))

# Bartlett test for homogeneity of variance

bartlett.test(FogIndex~Category, data=df1)
bartlett.test(TermstoWords~Category, data=df1)
bartlett.test(PCcomplex~Category, data=df1)
bartlett.test(FinancetoComplex~Category, data=df1)
bartlett.test(WordsperSentence~Category, data=df1)
bartlett.test(WordCount~Category, data=df1)

# Principal component analysis and plot




xpca <- data.frame(df1$FogIndex, 
		df1$TermstoWords, 
		df1$PCcomplex, 
		df1$FinancetoComplex, 
		df1$WordsperSentence, 
		df1$WordCount)

princompbjf<-princomp(xpca, cor=TRUE, scores=TRUE)

princompbjf <- princompbjf$rotation

summary(princompbjf)

loadings(princompbjf)

pc1<-princompbjf$scores[,1]

pc2<-princompbjf$scores[,2]

pca.frame <- data.frame(pc1, pc2)

pch <- rep(c("0", "1", "2"), c(31, 31, 31))

with(pca.frame, plot(pc1, pc2, pch=pch, xlab="Score on first principle component", ylab="Score on second principle component"))

legend(0, -4, c("Pay Day Loan", "Personal Loan", "Credit Card"), pch=c("0","1","2"))




pc1<-princompbjf$scores[,1]
pc2<-princompbjf$scores[,2]
pca.frame <- data.frame(pc1, pc2)
pch <- rep(c("0", "1", "2"), c(31, 31, 31))
with(pca.frame, plot(pc1, pc2, pch=pch, xlab="Score on first principle component", ylab="Score on second principle component"))
legend(0, -4, c("Pay Day Loan", "Personal Loan", "Credit Card"), pch=c("0","1","2"))


# Analysis of credit card corpus

credit.frame<-read.csv("credit_corpus.csv", header=TRUE)
ID<-credit.frame[,1]
ComplexCount<-credit.frame[,2]
FinancetoComplex<-credit.frame[,3]
F<-credit.frame[,4]
FK<-credit.frame[,5]
Fog<-credit.frame[,6]
Sentcount<-credit.frame[,7]
Terms<-credit.frame[,8]
Wordcount<-credit.frame[,9]
credit.frame["TermstoWords"]<-Terms/Wordcount
credit.frame["PCcomplex"]<-ComplexCount/Wordcount
credit.frame["SPCcomplex"]<-SuperComplexCount/Wordcount
credit.frame["WordsperSentence"]<-Wordcount/Sentcount
TermstoWords<-Terms/Wordcount
PCcomplex<-ComplexCount/Wordcount
SPCcomplex<-SuperComplexCount/Wordcount
WordsperSentence<-Wordcount/Sentcount
summary(credit.frame)
sd(credit.frame$FogIndex)
sd(credit.frame$TermstoWords)
sd(credit.frame$PCcomplex)
sd(credit.frame$FinancetoComplex)
sd(credit.frame$WordsperSentence)
sd(credit.frame$WordCount)















