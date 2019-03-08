words <- c("Data","Modelling","Analytics","Machine Learning","Bayesian", "Statistics",
           "Science","Coursework","Simulations","KNN","QDA","Distributions","Linear Regression",
           "Coefficient","Cross Validation","Decision Trees","JAGS","BUGS","Framework","Plot","Statisical Models")
num <- c(20,13,11,10,9,10,5,6,7,8,9,6,4,12,7,12,8,12,9,8,13)

length(num)
length(words)

library(wordcloud2)

word_freq<- data.frame(words,num)

wordcloud2(word_freq,size = 0.5,color = 'random-dark',shape = 'circle')


