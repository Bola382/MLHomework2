rm(list=ls())
load("var select.RData")
library(rminer);suppressMessages(library(dplyr))

dat_class = dat_class$class %>% factor # target
cnst_n  = nrow(dat_features)           # sample size
temp_C  = length(unique(dat_class))    # number of classes
temp_M  = 1                            # mc replicas
temp_regress = dat.err_regress         # selected vars

temp_k    = cnst_n   # number of partitions (temp_k=cnst_n leave one out)
# FOR LEAVE ONE OUT temp_M MUST BE EQUAL TO ONE
# SINCE THERE IS NO VARIABILITY IN THE METRICS
temp_CV = rep(1:temp_k,each=trunc(cnst_n/temp_k),length.out=cnst_n) # partitions
temp_methods = c("LDA","QDA","NBB")
temp_lmeth = length(temp_methods) # number of temp_methods used

dat_txerr = matrix(NA, temp_M, temp_lmeth, # stores error rate
                 dimnames = list(1:temp_M,temp_methods))
# progress bar
temp_pb <- txtProgressBar(min = 0,max = temp_M,style = 3,width = 50,char = "=") 

tm = proc.time()[3]
set.seed(1)
# mc loop
for(i in 1:temp_M){
 # randomizes the temp_CV partitions
 temp_idCV = sample(temp_CV) 
 
 temp_YpredLDA  = NULL
 temp_YpredQDA  = NULL
 temp_YpredNBB  = NULL
 
 pb2 <- txtProgressBar(min = 0,max = temp_k,style = 3,width = 25,char = "-") 
 # sets each partition as training set w/ the others as test sets
 for(g in 1:temp_k){
  
  temp_Xtr = dat_features[temp_idCV!=g,]
  temp_Xts = dat_features[temp_idCV==g,]
  temp_Ytr = dat_class[temp_idCV!=g]
  temp_Yts = dat_class[temp_idCV==g]
  
  # check for variance in the class
  if(length(unique(dat_class[temp_idCV!=g])) != temp_C){
   break
  }
  # training set
  dbtreino = cbind.data.frame(temp_Xtr, classe = temp_Ytr)
  # test set
  dbteste  = cbind.data.frame(temp_Xts, classe = temp_Yts)
  
  # model fit
  temp_modLDA   = fit(classe ~ ., data = dbtreino[,c(7,temp_regress[[1]])],
                 model = "lda", task = "c")
  temp_modQDA   = fit(classe ~ ., data = dbtreino[,c(7,temp_regress[[2]])],
                 model = "qda", task = "c")
  temp_modNBB   = fit(classe ~ ., data = dbtreino[,c(7,temp_regress[[3]])],
                 model = "naiveBayes", task = "c")
  
  
  # predicting values
  temp_YpredLDA[temp_idCV==g]  = predict(temp_modLDA, dbteste[,temp_regress[[1]]])
  temp_YpredQDA[temp_idCV==g]  = predict(temp_modQDA, dbteste[,temp_regress[[2]]])
  temp_YpredNBB[temp_idCV==g]  = predict(temp_modNBB, dbteste[,temp_regress[[3]]])
  setTxtProgressBar(pb2, g)
 };close(pb2)
 # error rate
 dat_txerr[i,"LDA"]  = (1 - sum(diag(prop.table(table(dat_class, temp_YpredLDA))))) * 100
 dat_txerr[i,"QDA"]  = (1 - sum(diag(prop.table(table(dat_class, temp_YpredQDA))))) * 100
 dat_txerr[i,"NBB"]  = (1 - sum(diag(prop.table(table(dat_class, temp_YpredNBB))))) * 100
 setTxtProgressBar(temp_pb, i)
};close(temp_pb);beepr::beep()

print(proc.time()[3] - tm)

# mc estimate
colMeans(dat_txerr)
apply(dat_txerr, 2, FUN = sd)

boxplot(dat_txerr)

rm(list = ls(pattern="temp_"))

save.image("loo CV for errselec vars.RData")
