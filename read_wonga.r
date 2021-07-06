# Title: EL paper analysis script

# Description: This program creates the descriptives and
# conducts the analysis for the EL paper

# Author: Matt Burke

# Last modified: 26/04/2019


setwd("C:/Users/mattb/Documents/University/PhD/Project_ConsumerFinance")

library(psych)
library(multcomp)

data.frame<-read.csv("data.csv", header=TRUE)

ID<-data.frame[,1]
APRexplain<-data.frame[,2]
ComplexCount<-data.frame[,3]
FinancetoComplex<-data.frame[,4]
F<-data.frame[,5]
FK<-data.frame[,6]
Fog<-data.frame[,7]
Sentcount<-data.frame[,8]
SuperComplexCount<-data.frame[,9]
SuperFinancetoComplex<-data.frame[,10]
Terms<-data.frame[,11]
Wordcount<-data.frame[,12]
Category<-data.frame[,13]
data.frame["TermstoWords"]<-Terms/Wordcount
data.frame["PCcomplex"]<-ComplexCount/Wordcount
data.frame["SPCcomplex"]<-SuperComplexCount/Wordcount
data.frame["WordsperSentence"]<-Wordcount/Sentcount
TermstoWords<-Terms/Wordcount
PCcomplex<-ComplexCount/Wordcount
SPCcomplex<-SuperComplexCount/Wordcount
WordsperSentence<-Wordcount/Sentcount

describeBy(data.frame, group=data.frame$Category, mat=TRUE, digits=4)

Category = factor(Category)

lm.fit.Fog <- lm(Fog~Category)
lm.fit.TermstoWords <- lm(TermstoWords~Category)
lm.fit.PCcomplex <- lm(PCcomplex~Category)
lm.fit.FinancetoComplex <- lm(FinancetoComplex~Category)
lm.fit.WordsperSentence <- lm(WordsperSentence~Category)
lm.fit.Wordcount <- lm(Wordcount~Category)

# Results of the baseline linear model
summary(lm.fit.Fog)
summary(lm.fit.TermstoWords)
summary(lm.fit.PCcomplex)
summary(lm.fit.FinancetoComplex)
summary(lm.fit.WordsperSentence)
summary(lm.fit.Wordcount)

summary(glht(lm.fit.Fog, mcp(Category="Tukey")))
summary(glht(lm.fit.TermstoWords, mcp(Category="Tukey")))
summary(glht(lm.fit.PCcomplex, mcp(Category="Tukey")))
summary(glht(lm.fit.FinancetoComplex, mcp(Category="Tukey")))
summary(glht(lm.fit.WordsperSentence, mcp(Category="Tukey")))
summary(glht(lm.fit.Wordcount, mcp(Category="Tukey")))

# Robustness
# Only doing robustness on the variables that 
# demonstrated significance in the baseline

glm.fit.Fog <- glm(Fog~Category, family=Gamma(link='log'))
glm.fit.TermstoWords <- glm(TermstoWords~Category, family=Gamma(link='log'))
glm.fit.PCcomplex <- glm(PCcomplex~Category, family=Gamma(link='log'))
glm.fit.FinancetoComplex <- glm(FinancetoComplex~Category, family=Gamma(link='log'))
glm.fit.WordsperSentence <- glm(WordsperSentence~Category, family=Gamma(link='log'))
glm.fit.Wordcount <- glm(Wordcount~Category, family=Gamma(link='log'))

summary(glm.fit.Fog)
summary(glm.fit.TermstoWords)
summary(glm.fit.PCcomplex)
summary(glm.fit.FinancetoComplex)
summary(glm.fit.WordsperSentence)
summary(glm.fit.Wordcount)

summary(glht(glm.fit.Fog, mcp(Category="Tukey")))
summary(glht(glm.fit.TermstoWords, mcp(Category="Tukey")))
summary(glht(glm.fit.PCcomplex, mcp(Category="Tukey")))
summary(glht(glm.fit.FinancetoComplex, mcp(Category="Tukey")))
summary(glht(glm.fit.WordsperSentence, mcp(Category="Tukey")))
summary(glht(glm.fit.Wordcount, mcp(Category="Tukey")))

# Tests for the null hypothesis that readability = 12

payday_fog<-(data.frame$Fog[which(data.frame$Category == 0)])
personal_fog<-(data.frame$Fog[which(data.frame$Category == 1)])
credit_fog<-(data.frame$Fog[which(data.frame$Category == 2)])
t.test(payday_fog, mu=12)
t.test(personal_fog, mu=12)
t.test(credit_fog, mu=12)

# Bartlett test for homogeneity of variance

bartlett.test(Fog~Category)
bartlett.test(TermstoWords~Category)
bartlett.test(PCcomplex~Category)
bartlett.test(FinancetoComplex~Category)
bartlett.test(WordsperSentence~Category)
bartlett.test(Wordcount~Category)

# Principal component analysis and plot

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















