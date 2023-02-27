rm(list=ls())
load("organized.RData")
library(rminer);library(ggplot2)

dat_class   = dat_class$class %>% factor       # target variable
tmp_methods = c("LDA","QDA","NBB","LR")        # list of methods used
tmp_lmeth   = length(tmp_methods)              # number of methods
tmp_classf  = c("lda","qda","naiveBayes","lr") # list of methods used accoring to rminer syntax
tmp_M       = 100                              # monte carlo reps
tmp_nRatio  = 5                                # number of ratios to be tested + 1
tmp_x       = cbind.data.frame(dat_features,   # temporary dataset
                               class=dat_class)

# stores the metrics for each method
# array, 1st dimension gives the method, 2nd gives the ratio of samples in test
# set, 3rd gives the metric and 4th gives the mc rep
dat_mtrc = array(NA,dim = c(tmp_lmeth,tmp_nRatio-1,2,tmp_M),
                 dimnames = list(tmp_methods,paste0(trunc(1:(tmp_nRatio-1)/tmp_nRatio*100),"%"),c("Taxa de erro","AUC"),1:tmp_M))

set.seed(1)
# loop for each ratio
for(k in 1:(tmp_nRatio-1)){
 cat("Training ratio: ",k/tmp_nRatio*100,"%\n")
 
 tmp_pb <- txtProgressBar(min = 0,
                          max = tmp_M,
                          style = 3,
                          width = 50,
                          char = "=") # progress bar
 # mc loop
 for(i in 1:tmp_M){
  
  # creates training and test sets
  tmp_index = holdout(tmp_x$class, ratio = k/tmp_nRatio)  
  tmp_Xtr   = tmp_x[tmp_index$tr,]
  tmp_Xts   = tmp_x[tmp_index$ts,]
  
  # loop for each method
  for(j in 1:tmp_lmeth){
   
   # model fit
   tmp_mod1    = fit(class ~ ., data = tmp_Xtr, model = tmp_classf[j],
                     task = "c")
   tmp_mod2    = fit(class ~ ., data = tmp_Xtr, model = tmp_classf[j],
                         task = "p")
   
   # predictions
   tmp_yhat    = predict(tmp_mod1, tmp_Xts[,1:cnst_p])
   tmp_phat    = predict(tmp_mod2, tmp_Xts[,1:cnst_p])
   
   # saves metrics
   dat_mtrc[j,k,1,i] = mmetric(tmp_Xts[,cnst_p+1],tmp_yhat, metric = "CE")
   dat_mtrc[j,k,2,i] = mmetric(tmp_Xts[,cnst_p+1],tmp_phat, metric = "AUC")
  }
  setTxtProgressBar(tmp_pb, i)
 };close(tmp_pb)
};beepr::beep()


apply(dat_mtrc,c(1,2,3),mean)
apply(dat_mtrc,c(1,2,3),sd)
