rm(list=ls())
load("var select.RData")
library(rminer);suppressMessages(library(dplyr))

X = data %>% select(-last_col())                                  # covariates
Y = data %>% select(last_col()) %>% unlist %>% rmnames %>% factor # target
n  = nrow(X)                                                      # sample size
C  = length(unique(Y))                                            # number of classes
m  = 100                                                          # mc replicas

k    = 10   # number of partitions (k=n leave one out)
# FOR LEAVE ONE OUT m MUST BE EQUAL TO ONE
# SINCE THERE IS NO VARIABILITY IN THE ERROR RATE
CV = rep(1:k,each=trunc(n/k),length.out=n) # partitions
methods = c("5NN","10NN","15NN","LDA","DT","SVM","MULTINOM")
lmeth = length(methods) # number of methods used

tx_erro = matrix(NA, m, lmeth, # stores error rate
                 dimnames = list(1:m,methods))
# progress bar
pb <- txtProgressBar(min = 0,max = m,style = 3,width = 50,char = "=") 

tm = proc.time()[3]
set.seed(1)
# mc loop
for(i in 1:m){
 # randomizes the CV partitions
 idCV = sample(CV) 
 
 Ypred5NN  = NULL
 Ypred10NN = NULL
 Ypred15NN = NULL
 YpredLDA  = NULL
 YpredDT   = NULL
 YpredSVM  = NULL
 YpredMULTINOM  = NULL
 
 pb2 <- txtProgressBar(min = 0,max = k,style = 3,width = 25,char = "-") 
 # sets each partition as training set w/ the others as test sets
 for(g in 1:k){
  
  Xtr = X[idCV!=g,]
  Xts = X[idCV==g,]
  Ytr = Y[idCV!=g]
  Yts = Y[idCV==g]
  
  # check for variance in the class
  if(length(unique(Y[idCV!=g])) != C){
   break
  }
  # training set
  dbtreino = cbind.data.frame(Xtr, classe = Ytr)
  # test set
  dbteste  = cbind.data.frame(Xts, classe = Yts)
  
  # model fit
  mod5NN   = fit(classe ~ ., data = dbtreino[,c(58,regress[[1]])],
                 model = "knn", task = "c", k =5)
  mod10NN   = fit(classe ~ ., data = dbtreino[,c(58,regress[[2]])],
                  model = "knn", task = "c", k =10)
  mod15NN   = fit(classe ~ ., data = dbtreino[,c(58,regress[[3]])],
                  model = "knn", task = "c", k =15)
  modLDA   = fit(classe ~ ., data = dbtreino[,c(58,regress[[4]])],
                 model = "lda", task = "c")
  modDT   = fit(classe ~ ., data = dbtreino[,c(58,regress[[5]])],
                model = "dt", task = "c")
  modSVM   = fit(classe ~ ., data = dbtreino[,c(58,regress[[6]])],
                 model = "svm", task = "c")
  modMULTINOM   = fit(classe ~ ., data = dbtreino[,c(58,regress[[7]])],
                      model = "multinom", task = "c")
  
  # predicting values
  Ypred5NN[idCV==g]  = predict(mod5NN, dbteste[,regress[[1]]])
  Ypred10NN[idCV==g] = predict(mod10NN, dbteste[,regress[[2]]])
  Ypred15NN[idCV==g] = predict(mod15NN, dbteste[,regress[[3]]])
  YpredLDA[idCV==g]  = predict(modLDA, dbteste[,regress[[4]]])
  YpredDT[idCV==g]   = predict(modDT, dbteste[,regress[[5]]])
  YpredSVM[idCV==g]  = predict(modSVM, dbteste[,regress[[6]]])
  YpredMULTINOM[idCV==g]  = predict(modMULTINOM, dbteste[,regress[[7]]])
  setTxtProgressBar(pb2, g)
 };close(pb2)
 # error rate
 tx_erro[i,"5NN"]  = (1 - sum(diag(prop.table(table(Y, Ypred5NN))))) * 100
 tx_erro[i,"10NN"] = (1 - sum(diag(prop.table(table(Y, Ypred10NN))))) * 100
 tx_erro[i,"15NN"] = (1 - sum(diag(prop.table(table(Y, Ypred15NN))))) * 100
 tx_erro[i,"LDA"]  = (1 - sum(diag(prop.table(table(Y, YpredLDA))))) * 100
 tx_erro[i,"DT"]   = (1 - sum(diag(prop.table(table(Y, YpredDT))))) * 100
 tx_erro[i,"SVM"]  = (1 - sum(diag(prop.table(table(Y, YpredSVM))))) * 100
 tx_erro[i,"MULTINOM"] = (1 - sum(diag(prop.table(table(Y, YpredMULTINOM))))) * 100
 setTxtProgressBar(pb, i)
};close(pb);beepr::beep()

print(proc.time()[3] - tm)

# mc estimate
colMeans(tx_erro)
apply(tx_erro, 2, FUN = sd)

boxplot(tx_erro)
