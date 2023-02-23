rm(list=ls())
library(rminer);suppressMessages(library(dplyr))
load("organized.RData")

temp_x = cbind.data.frame(dat_features,dat_class)


temp_methods = c("LDA","QDA","NNB")
temp_lmeth = length(temp_methods)

names(temp_x)

temp_M     = 100                            # n of monte carlo inter 
temp_prop  = .5                             # prop of train samples 
temp_nsamp = trunc(temp_prop*nrow(temp_x))  # n of train samples 

# Algorithm 

# Partitioning training and test sets for MC
set.seed(1)
temp_id  = matrix(0, nrow = temp_M, ncol = temp_nsamp) 
for(i in 1:temp_M){temp_id[i,] = sample(1:nrow(temp_x),temp_nsamp)}
Xtr = Xts = Y = list()

for(i in 1:temp_M){
 Xtr[[i]]=temp_x[temp_id[i,],]  # training set
 Xts[[i]]=temp_x[-temp_id[i,],] # testing set
 Y[[i]] = Xts[[i]][,"class"] # target variable
}

# Forward and model fitting  -----------------------------

pool        = lapply(1:temp_lmeth,          # 1:p covariates for each of the lmeth methods  
                     function(a) 1:cnst_p) 
regress     = vector("list",temp_lmeth)     # selected vars
continue    = rep(TRUE,temp_lmeth)          # halt criteria
prsnt_txerr = rep(100,temp_lmeth)           # current error rate
mod         = vector("list",temp_lmeth)     # stores models for each step
Yhat        = vector("list",temp_lmeth)     # predicted values for each step
classf      = c("lda","qda","naiveBayes")   # list of methods used accoring to rminer syntax

cc = 1 # for progress tracking

t1 = Sys.time()
set.seed(1)
# while any of the continues is true, loop:
while(any(continue)){
 cat("\t\t\t\t\t\t","Iteraction: ",cc,"\n")
 # stores the error rate for each model still in pool
 tx_err = lapply(1:temp_lmeth, function(a) matrix(NA,nrow=temp_M,ncol=length(pool[[a]])))
 pbmthd <- txtProgressBar(min = 0,max = temp_lmeth,style = 3, width = 100,char = "=")
 # loop for each method
 for(ii in 1:temp_lmeth){
  if(continue[ii]==T){
   # MC loop
   cat("\n\t\t\tMC loop for", temp_methods[[ii]], "\n")
   pbmc <- txtProgressBar(min = 0,max = temp_M,style = 3, width = 50,char = "-")
   for(j in 1:temp_M){
    # loop for each var still in pool w/ regress from last step
    for(i in 1:length(pool[[ii]])){
     k = c(regress[[ii]], pool[[ii]][i])
     
     mod[[ii]] = fit(class ~ .,
                     data = Xtr[[j]][,c("class", names(temp_x)[k])],
                     model = classf[[ii]], task = "c")
     
     Yhat[[ii]]    = predict(mod[[ii]], Xts[[j]][,c("class", names(temp_x)[k])])
     tx_err[[ii]][j,i] = (1 - sum(diag(prop.table(table(Y[[j]], Yhat[[ii]]))))) * 100
    }
    setTxtProgressBar(pbmc, j)
   };close(pbmc)
   # calc min error rate for current step
   temp_txerr = colMeans(tx_err[[ii]])
   id_min  = which.min(temp_txerr)
   cat("  | Last step error rate for", temp_methods[[ii]], ":", prsnt_txerr[[ii]], "\n")
   cat("  | Current step error rate for", temp_methods[[ii]], ":", temp_txerr[id_min], "\n")
   if(prsnt_txerr[[ii]] > temp_txerr[id_min]){
    prsnt_txerr[[ii]] = temp_txerr[id_min]
    regress[[ii]] = c(regress[[ii]], pool[[ii]][id_min])
    pool[[ii]]    = pool[[ii]][-id_min]
   }else{
    continue[[ii]] = FALSE
    cat("  | Done!\n")
   }
   cat("  | A reduced", temp_methods[[ii]], "model includes:\n  |", names(temp_x)[regress[[ii]]],
       "\n  |--------------------------------------------------|\n")
  }else{cat("\n  | Method", temp_methods[[ii]],"done! \n  | Model includes:\n  |", 
            names(temp_x)[regress[[ii]]],
            "\n  | Min error rate:", prsnt_txerr[[ii]],
            "\n  |--------------------------------------------------|\n")}
  setTxtProgressBar(pbmthd, ii)
 };close(pbmthd)
 cc = cc+1
};beepr::beep();t2=Sys.time();print(t2-t1)
