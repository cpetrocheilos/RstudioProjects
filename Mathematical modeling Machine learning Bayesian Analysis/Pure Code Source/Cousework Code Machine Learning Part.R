#--- Starting Point ------------------------
library(readr)
earthquake <- read.csv("C:/File_Earthequakes.txt_location", sep="")
str(earthquake)
head(earthquake)
dim(earthquake)
attach(earthquake)

table(popn)


############################################################################################################
#--- Visualize data ------------------------
library(ggplot2)
library(scales)

ggplot(earthquake , aes(x = body , y= surface,col = popn))+
  geom_point(size = 3.5)+
  labs(title = "Earthquakes and Explotion Data", x = "Body-wave Magnitube (mb)",y = "Surface-wave Magnitube (Ms)",
       col = "" )+
  theme(legend.position = "bottom")+
  scale_color_manual(labels = c("Earthequake", "Newclear Explotion"),values = c("red", "blue"))


# Crate a vector of diferent colours for the default popn values
#
def.col <- rep("blue",30) # vector of colors
def.col[popn == "equake"] <- "red" # red color for the earthquake variable

# I difine a grid of point that cover the entire ragne of the data clasifier on this grid
len <- 50
xp <- seq(4,7,length = len)
yp <- seq(2,8,length = len)
xygrid <- expand.grid(body = xp , surface = yp)

############################################################################################################
#--- KNN Analysis --------------------------

# Find mean of body waves between the earthquakes and explotions
aggregate(body ~ popn, FUN = mean)
# Find mean of body waves between the earthquakes and explotions
aggregate(surface~ popn, FUN = mean)



library(class)

train.X <- cbind(body , surface) # The matrix of predictors
train.X
k<-6 # default value for k to start the analysis of the classifiers

# Using the knn() function from the class library
#
def.knn <- knn( train = train.X, # Matrix of contranig predictors for training data
                test = train.X,# Matrix containing the data which we wish to make predictions
                cl = popn, # The vector contains the factor(labels) of the training observations
                k = k) # set the k from the variable k above
#
# Create the table of def.knn and popn value to check the accuracy of the rule on the training data 
#
t<-table(def.knn, popn)
t
#
#Propostion of incorect clasified data points
#
incorect_propotion <- (t[2,1]+t[1,2])/30
incorect_propotion

#
# We represent the two classes as 1 (popn = earthquake) and 0 (popn = explotion)
# for ploting the class boundry
#
cl <- rep(0, 30); cl[popn == 'equake'] = 1; cl <- as.factor(cl)

#
# We clasify the point from the grid
#
grid.knn <- knn( train = train.X, # Matrix of contranig predictors for training data
                 test = xygrid, # as test we put the grid we prepared 
                 cl = cl, # as the vector contains the factor put the one we created above
                 k = k)

#
# We prepated the vector of colours that we plot
#
col1 <- rep("lightblue", len*len) 
for (i in 1:(len*len)) if(grid.knn[i] == "1") col1[i] <- "indianred1"


plot(xygrid, col = col1, main = "KNN classifier with K=2", xlab = "Body-wave", ylab = "Surface-wave") 
contour(xp, yp, matrix(grid.knn, len), levels = 0.5, add = TRUE, lwd = 2)
points(body,surface, col = def.col)


# choosing the best k 
# We dont needed we have a very small sample
# Question we need to create a sub set for every test with KNN or it is not needed ???? 
# Split the data into training set and test set 
set.seed(10)
def.subset <- sample(30,15)
train.X.sub <- train.X[-def.subset, ]
cl.sub <- cl[-def.subset]
test.X <- train.X[def.subset, ]
test.cl <- cl[def.subset]



# we dont need the create a sample because we have only 30 observations
test.error1 <- function(k){
  def.knn.k <- knn( train = train.X.sub, test = test.X, cl = cl.sub, k = k)
  tab <- table(def.knn.k, test.cl) 
  error1 <- (tab[1,2] + tab[2,1]) / sum(tab) 
  return(error1)
}
errors1 <- rep(1,15)
for (i in 1:15) errors1[i] <- test.error1(k = i) 

#-----KNN plot function
knn.plot <- function(k){
  
  len <- 50
  
  def.knn <- knn( train = train.X, # Matrix of contranig predictors for training data
                  test = train.X,# Matrix containing the data which we wish to make predictions
                  cl = popn, # The vector contains the factor(labels) of the training observations
                  k = k)
  
  cl <- rep(0, 30); cl[popn == 'equake'] = 1; cl <- as.factor(cl)
  
  grid.knn <- knn( train = train.X, # Matrix of contranig predictors for training data
                   test = xygrid, # as test we put the grid we prepared 
                   cl = cl, # as the vector contains the factor put the one we created above
                   k = k)
  
  col1 <- rep("lightblue", len*len) 
  for (i in 1:(len*len)) if(grid.knn[i] == "1") col1[i] <- "indianred1"
  

              
plot <- plot(xygrid, col = col1, main = paste("KNN classifier with K=",(k)), xlab = "Body-wave", ylab = "Surface-wave") 
boundry<-contour(xp, yp, matrix(grid.knn, len), levels = 0.5, add = TRUE, lwd = 2)
points <-points(body,surface, col = def.col)
              
return(list(plot,boundry,points))
}


par(mfrow=c(2,2))
knn.plot(1)
knn.plot(3)
knn.plot(5)
knn.plot(7)
dev.off()
library(grid)
library(scales)
library(gridExtra)

# here i have to write the ggplot



# Create the function to check the errors of the test to find the best k
test.error <- function(k){
  def.knn <- knn( train = train.X, test = train.X, cl = popn, k = k)
  tab <- table(def.knn, popn)
  error <- (tab[1,2] + tab[2,1]) / 30 
  return(error)
}
# creating a variable to hold the errors and then use the function to calculate the errors
errors <- rep(0,30)
for (i in 1:30) errors[i] <- test.error(k = i) 

errors_label = seq(1,30 , by =1)
  
error_table = data.frame(errors,errors_label)

ggplot(error_table[1:10], aes(x=errors_label, y= errors))+
  geom_point()


plot(errors,xlab = "K",ylab = "Test error") # plot the errors
plot(errors[1:8],xlab = "K",ylab = "Test error") # plot 20 first errors

plot(train.X[,1], train.X[,2], col = def.col, xlab = 'Std Balance', ylab = 'Std Income')


############################################################################################################
#--- QDA -----------------------------------
library(MASS)
# Fit the qda model from the MASS library on the data.
def.qda <- qda(popn ~ body + surface , data = earthquake)
def.qda
names(def.qda)
# clasifiy the point on the grid with the predict function
grid.qda <- predict(def.qda , xygrid)
names(grid.qda)
#preparing the vector of to be plotted
col2 <- rep("lightblue",len*len)
for (i in 1:(len*len)) if(grid.qda$class[i] == 'equake') col2[i] <- "indianred1" 

# preparring the boundry
zp <- grid.qda$post[ ,1] - grid.qda$post[ ,2]
grid.qda$posterior


plot(xygrid, col = col2 , main = "QDA classifier",xlab = "Body-wave",ylab = "Surface-wave")
contour(xp,yp,matrix(zp,len),levels = c(0),add = TRUE , lwd =3)
points(body, surface,col = def.col , lwd = 2)

# Training error for QDA 
qda.pred <- predict(def.qda) # prediction for the training data
qda.class <- qda.pred$class # class prediction for training data

qda.tab <- table(qda.class,popn)
qda.tab

# The overall fraction of incorect classified data
qda.test.error <- (qda.tab[1,2]+qda.tab[2,1])/sum(qda.tab)

# Thefraction of incorrect cassification for the individuals who do not default
qda.tab[2,1]/(qda.tab[1,1]+qda.tab[2,1])

# The fraction of incorect clasification who default
qda.tab[1,2]/(qda.tab[1,2]+qda.tab[2,2])

# Training error for midified QDA
new.class <- ifelse(qda.pred$post[ ,2]<0.74,"equake","explosn")


new.tab <-table(new.class,popn)
new.tab

# The overall fraction of incorect classified data
(new.tab[1,2]+new.tab[2,1])/sum(new.tab)

# Thefraction of incorrect cassification for the individuals who do not default
new.tab[2,1]/(new.tab[1,1]+new.tab[2,1])

# The fraction of incorect clasification who default
new.tab[1,2]/(new.tab[1,2]+new.tab[2,2])

############################################################################################################
#--- Decision Trees ------------------------
library(tree)
# I fit the tree function to the data set 
tree.fit <- tree(popn ~ ., data = earthquake)

plot(tree.fit)
text(tree.fit,pretty = 0)

summary(tree.fit)

tree.fit # Not runing why ?


tree.pred <- predict(tree.fit,type= "class")
tab.tree <- table(tree.pred,popn)
tab.tree

# test error 
tree.test.error <- (tab.tree[1,2]+tab.tree[2,1])/sum(tab.tree)
tree.test.error # 0.06666667


# Way to plot the tree rules in a scater plot
popnCol <- rep("blue",30)
popnCol[earthquake$popn=="equake"] <- "red"

plot(x = earthquake$body , y = earthquake$surface ,col= popnCol, pch = 16 , cex = 1.5,
     main = "Tree scatter plot with Clasification Boarders",
     xlab = "Body wave",ylab = "Surface wave")
legend("topleft", legend=c("Earthequake", "Explotion"),
       col=c("red", "blue"),  pch = 16 ,cex=1)
partition.tree(tree.fit, add = TRUE, cex = 2)



############################################################################################################
#--- Leave-one out cross validation ------

# CV for QDA
n <- nrow(earthquake)
cv.predictions.qda <- rep("equake", n)

for(i in 1:n){
  fit.qda <- qda(popn ~ . , data = earthquake[-i, ])
  hold <- predict(fit.qda ,newdata = earthquake[i,] , type = "class")
  cv.predictions.qda[i] <- hold$class
}
tab.cv.qda <- table(cv.predictions.qda, popn)
tab.cv.qda

cv.qda.error = (tab.cv.qda[1,2] + tab.cv.qda[2,1]) / sum(tab.cv.qda) 
cv.qda.error

# CV for KNN

# First way of calculate the knn cross validation error 
cv.training.error <- function(k){
  # This function return the cross validation error for each selected k
  
  # Use knn.cv function from class library which does automaticaly 
  # cross validation for each selected k
  knn.cv.fit <- knn.cv(train.X , popn , k = i, l = 0, prob = TRUE)
  tab.knn <- table(knn.cv.fit,popn)
  cv.knn.error = (tab.knn[1,2] + tab.knn[2,1]) / sum(tab.knn)
  return(cv.knn.error)
}
cv.error.store <- rep(12,29)
for(i in 1:29){
  cv.error.store[i] <- cv.training.error(i)
}



plot(cv.error.store,xlab = "K",ylab = "Cross validation error")

knn.cv.plot <- data.frame(cv.error = cv.knn.error,k = seq(1:29))
ggplot(knn.cv.plot,aes(x = k,y = cv.error))+
         geom_point()
dim(knn.cv.plot)

# CV for Clasification Trees

n <- nrow(earthquake) 
cv.predictions.tree <- rep('equake', n)

for(i in 1:n) {
  tree.fit <- tree(popn ~ ., data = earthquake[-i, ])
  cv.predictions.tree[i] <- predict(tree.fit, newdata = earthquake[i,], type = "class")
}

tab.cv.tree <- table(cv.predictions.tree, popn) 
tab.cv.tree

cv.tree.error = (tab.cv.tree[1,2] + tab.cv.tree[2,1]) / sum(tab.cv.tree) 
cv.tree.error # 0.2 grater than the test error 

# Tree complexity with cv.tree Not needed in the report 
set.seed(123)
cv.equake.tree <- cv.tree(tree.fit, FUN = prune.misclass)
plot(cv.equake.tree$size, cv.equake.tree$dev,type = "b")


prune.equake <- prune.misclass(tree.fit, best = 2)
plot(prune.equake)
text(prune.equake, pretty = 0)

############################################################################################################
#--- Support Vector Machines ---------------
library(e1071)

svm.fit <- svm(popn ~ body + surface, data = earthquake , kernel = "linear",cost = 10 , scale = FALSE)

plot(svm.fit , earthquake)

svm.fit$index

summary(svm.fit)

svm.fit <- svm(popn ~ body + surface, data = earthquake , kernel = "linear",cost = 10 , scale = FALSE)
plot(svm.fit,earthquake)

# Finding the best tunning parameter
set.seed(1)
tune.out <- tune(svm , popn ~ body + surface ,data = earthquake , kernel = "linear",
                 ranges = c(0.001 , 0.01 , 0.1 , 1 , 5 , 10 , 100))

summary(tune.out)
names(tune.out)
tune.out$performances

plot(tune.out$performances$Var1 , tune.out$performances$error)
bestmod = tune.out$best.model

summary(bestmod)

ypred <- predict(bestmod,earthquake)
table(predict = ypred , truth = earthquake$popn)

# SVM polynomial kernel
svmfit.pol <- svm(popn ~ body + surface , kernel = "polynomial",degree = 3, cost = 1 ,scale = FALSE)
plot(svmfit.pol,earthquake)

# SVM radial kernel
svmfit.rad <- svm(popn ~ body + surface , kernel = "radial",gama = 1, cost = 1 ,scale = FALSE)
plot(svmfit.rad,earthquake)

summary(svmfit.rad)

############################################################################################################