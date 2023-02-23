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

# --------------------------------------------------------
# Partitioning training and test sets for MC
# --------------------------------------------------------

set.seed(1)
temp_id  = matrix(0, nrow = temp_M, ncol = temp_nsamp) 
for(i in 1:temp_M){temp_id[i,] = sample(1:nrow(temp_x),temp_nsamp)}
temp_Xtr = temp_Xts = temp_Y = list()

for(i in 1:temp_M){
 temp_Xtr[[i]]=temp_x[temp_id[i,],]  # training set
 temp_Xts[[i]]=temp_x[-temp_id[i,],] # testing set
 temp_Y[[i]] = temp_Xts[[i]][,"class"] # target variable
}

# ==============================================================================
#                               Error rate
# ==============================================================================

# --------------------------------------------------------
# Forward and model fitting  
# --------------------------------------------------------

temp.err_pool        = lapply(1:temp_lmeth,          # 1:p covariates for each of the lmeth methods  
                     function(a) 1:cnst_p) 
dat.err_regress   = vector("list",temp_lmeth)     # selected vars
temp.err_continue    = rep(TRUE,temp_lmeth)          # halt criteria
dat.err_prsntMtrc  = rep(100,temp_lmeth)           # current error rate
temp.err_mod         = vector("list",temp_lmeth)     # stores models for each step
temp.err_Yhat        = vector("list",temp_lmeth)     # predicted values for each step
temp.err_classf      = c("lda","qda","naiveBayes")   # list of methods used accoring to rminer syntax

temp.err_cc = 1 # for progress tracking

t1 = Sys.time()
set.seed(1)
# while any of the continues is true, loop:
while(any(temp.err_continue)){
 cat("\t\t\t\t\t\t","Iteraction: ",temp.err_cc,"\n")
 # stores the error rate for each model still in temp.err_pool
 temp.err_mtrc = lapply(1:temp_lmeth, function(a) matrix(NA,nrow=temp_M,ncol=length(temp.err_pool[[a]])))
 temp.err_pbmthd <- txtProgressBar(min = 0,max = temp_lmeth,style = 3, width = 100,char = "=")
 # loop for each method
 for(ii in 1:temp_lmeth){
  if(temp.err_continue[ii]==T){
   # MC loop
   cat("\n\t\t\tMC loop for", temp_methods[[ii]], "\n")
   temp.err_pbmc <- txtProgressBar(min = 0,max = temp_M,style = 3, width = 50,char = "-")
   for(j in 1:temp_M){
    # loop for each var still in temp.err_pool w/ dat.err_regress from last step
    for(i in 1:length(temp.err_pool[[ii]])){
     k = c(dat.err_regress[[ii]], temp.err_pool[[ii]][i])
     
     temp.err_mod[[ii]] = fit(class ~ .,
                     data = temp_Xtr[[j]][,c("class", names(temp_x)[k])],
                     model = temp.err_classf[[ii]], task = "c")
     
     temp.err_Yhat[[ii]]    = predict(temp.err_mod[[ii]], temp_Xts[[j]][,c("class", names(temp_x)[k])])
     temp.err_mtrc[[ii]][j,i] = (1 - sum(diag(prop.table(table(temp_Y[[j]], temp.err_Yhat[[ii]]))))) * 100
    }
    setTxtProgressBar(temp.err_pbmc, j)
   };close(temp.err_pbmc)
   # calc min error rate for current step
   temp.err_txerr = colMeans(temp.err_mtrc[[ii]])
   temp.err_idMin  = which.min(temp.err_txerr)
   cat("  | Last step error rate for", temp_methods[[ii]], ":", dat.err_prsntMtrc[[ii]], "\n")
   cat("  | Current step error rate for", temp_methods[[ii]], ":", temp.err_txerr[temp.err_idMin], "\n")
   if(dat.err_prsntMtrc[[ii]] > temp.err_txerr[temp.err_idMin]){
    dat.err_prsntMtrc[[ii]] = temp.err_txerr[temp.err_idMin]
    dat.err_regress[[ii]] = c(dat.err_regress[[ii]], temp.err_pool[[ii]][temp.err_idMin])
    temp.err_pool[[ii]]    = temp.err_pool[[ii]][-temp.err_idMin]
   }else{
    temp.err_continue[[ii]] = FALSE
    cat("  | Done!\n")
   }
   cat("  | A reduced", temp_methods[[ii]], "model includes:\n  |", names(temp_x)[dat.err_regress[[ii]]],
       "\n  |--------------------------------------------------|\n")
  }else{cat("\n  | Method", temp_methods[[ii]],"done! \n  | Model includes:\n  |", 
            names(temp_x)[dat.err_regress[[ii]]],
            "\n  | Min error rate:", dat.err_prsntMtrc[[ii]],
            "\n  |--------------------------------------------------|\n")}
  setTxtProgressBar(temp.err_pbmthd, ii)
 };close(temp.err_pbmthd)
 temp.err_cc = temp.err_cc+1
};beepr::beep();t2=Sys.time();print(t2-t1)

rm(list = ls(pattern="temp.err_"));rm("i","ii","j","k","t1","t2")

# ==============================================================================
#                                    AUC
# ==============================================================================

temp.auc_pool     = lapply(1:temp_lmeth,          # 1:p covariates for each of the lmeth methods  
                           function(a) 1:cnst_p) 
dat.auc_regress   = vector("list",temp_lmeth)     # selected vars
temp.auc_continue = rep(TRUE,temp_lmeth)          # halt criteria
dat.auc_prsntMtrc = rep(0,temp_lmeth)             # current area under ROC curve (AUC)
temp.auc_mod      = vector("list",temp_lmeth)     # stores models for each step
temp.auc_classf   = c("lda","qda","naiveBayes")   # list of methods used accoring to rminer syntax

temp.auc_cc = 1 # for progress tracking

t1 = Sys.time()
set.seed(1)
# while any of the continues is true, loop:
while(any(temp.auc_continue)){
 cat("\t\t\t\t\t\t","Iteraction: ",temp.auc_cc,"\n")
 # stores the AUC for each model still in temp.auc_pool
 temp.auc_mtrc = lapply(1:temp_lmeth, function(a) matrix(NA,nrow=temp_M,ncol=length(temp.auc_pool[[a]])))
 temp.auc_pbmthd <- txtProgressBar(min = 0,max = temp_lmeth,style = 3, width = 100,char = "=")
 # loop for each method
 for(ii in 1:temp_lmeth){
  if(temp.auc_continue[ii]==T){
   # loop for each var still in temp.auc_pool w/ dat.auc_regress from last step
   for(i in 1:length(temp.auc_pool[[ii]])){
    k = c(dat.auc_regress[[ii]], temp.auc_pool[[ii]][i])
    
    temp.auc_mod[[ii]] = mining(class ~ .,
                                data = temp_x[,c("class", names(temp_x)[k])],
                                Runs = temp_M,
                                method = "kfold",
                                model = temp.auc_classf[[ii]], 
                                task = "p")
    
    temp.auc_mtrc[[ii]][,i] = mmetric(temp.auc_mod[[ii]],metric="AUC")$AUC
    
   }
   # calc max AUC for current step
   temp.auc_MCmtrc = colMeans(temp.auc_mtrc[[ii]])
   temp.auc_idMax  = which.max(temp.auc_MCmtrc)
   cat("\n  | Last step AUC for", temp_methods[[ii]], ":", dat.auc_prsntMtrc[[ii]], "\n")
   cat("  | Current step AUC for", temp_methods[[ii]], ":", temp.auc_MCmtrc[temp.auc_idMax], "\n")
   if(dat.auc_prsntMtrc[[ii]] < temp.auc_MCmtrc[temp.auc_idMax]){
    dat.auc_prsntMtrc[[ii]] = temp.auc_MCmtrc[temp.auc_idMax]
    dat.auc_regress[[ii]] = c(dat.auc_regress[[ii]], temp.auc_pool[[ii]][temp.auc_idMax])
    temp.auc_pool[[ii]]    = temp.auc_pool[[ii]][-temp.auc_idMax]
   }else{
    temp.auc_continue[[ii]] = FALSE
    cat("  | Done!\n")
   }
   cat("  | A reduced", temp_methods[[ii]], "model includes:\n  |", names(temp_x)[dat.auc_regress[[ii]]],
       "\n  |--------------------------------------------------|\n")
  }else{cat("\n  | Method", temp_methods[[ii]],"done! \n  | Model includes:\n  |", 
            names(temp_x)[dat.auc_regress[[ii]]],
            "\n  | Max AUC rate:", dat.auc_prsntMtrc[[ii]],
            "\n  |--------------------------------------------------|\n")}
  setTxtProgressBar(temp.auc_pbmthd, ii)
 };close(temp.auc_pbmthd)
 temp.auc_cc = temp.auc_cc+1
};beepr::beep();t2=Sys.time();print(t2-t1)

rm(list = ls(pattern="temp.auc_"));rm("i","ii","k","t1","t2")
rm(list = ls(pattern="temp_"))

save.image("var select.RData")

# aa=rminer::mining(class~.,data=temp_x,Runs=100,method="kfold",model="lda",task="p")
# rminer::mmetric(aa,metric="AUC")$AUC