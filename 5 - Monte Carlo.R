rm(list=ls())
load("var select.RData")
library(rminer);suppressMessages(library(dplyr));library(ggplot2)

dat_class    = dat_class$class %>% factor  # target variable
tmp_methods = c("KNN","DT","SVM","LR")     # list of methods used accoring to rminer syntax
tmp_lmeth   = length(tmp_methods)          # number of methods
tmp_classf  = c("knn","dt","svm","lr")     # list of methods used accoring to rminer syntax
tmp_M       = 1000                         # monte carlo reps

# ---------------------------
# training and test sets
# ---------------------------

tmp_Xtr = tmp_Xts = tmp.ica_Xtr = tmp.ica_Xts = tmp_Ytr = tmp_Yts = list()
set.seed(1)
for(i in 1:tmp_M){
 tmp_index = holdout(dat_class,ratio=1/3) # 1/3 in training set i.e 30%
 tmp_Xtr[[i]]     = dat_features[tmp_index$tr,]
 tmp_Xts[[i]]     = dat_features[tmp_index$ts,]
 tmp.ica_Xtr[[i]] = dat_ICAfeatures[tmp_index$tr,]
 tmp.ica_Xts[[i]] = as.data.frame(dat_ICAfeatures[tmp_index$ts,])
 colnames(tmp.ica_Xts[[i]]) = 1:6
 tmp_Ytr[[i]]     = dat_class[tmp_index$tr]
 tmp_Yts[[i]]     = dat_class[tmp_index$ts]
}

# ==============================================================================
#                             Original features
# ==============================================================================

# stores the metrics for each method
# array, 1st dimension gives the method, 2nd gives the metric and 3rd gives the 
# mc rep
dat.org_mtrc = array(NA,dim = c(tmp_lmeth,2,tmp_M),dimnames=list(tmp_methods,c("Taxa de erro","AUC"),1:tmp_M))#lapply(1:tmp_lmeth, function(a) matrix(NA,nrow=tmp_M,ncol=2,dimnames = list(1:tmp_M,c("errorRt","AUC"))))

# ---------------------------
# holdout with MC
# ---------------------------

tmp_pb <- txtProgressBar(min = 0,max = tmp_M,style = 3,width = 50,char = "=") # progress bar
for(i in 1:tmp_M){
 tmp.org_x = cbind.data.frame(tmp_Xtr[[i]],class=tmp_Ytr[[i]])
 for(j in 1:tmp_lmeth){
  tmp.org_mod  = fit(class~.,data=tmp.org_x,model=tmp_classf[j],task="c")
  tmp.org_mod2 = fit(class~.,data=tmp.org_x,model=tmp_classf[j],task="p")
  
  tmp.org_yhat = predict(tmp.org_mod,tmp_Xts[[i]])  # class predictions
  tmp.org_phat = predict(tmp.org_mod2,tmp_Xts[[i]]) # prob predictions
  
  dat.org_mtrc[j,1,i] = mmetric(tmp_Yts[[i]],tmp.org_yhat,metric="CE")  # error rate 
  dat.org_mtrc[j,2,i] = mmetric(tmp_Yts[[i]],tmp.org_phat,metric="AUC") # AUC
 }
 setTxtProgressBar(tmp_pb, i)
};close(tmp_pb);beepr::beep()

# ---------------------------
# MC estimates
# ---------------------------

apply(dat.org_mtrc,c(1,2),mean)              # means
apply(dat.org_mtrc,c(1,2),sd)                # standard deviation
apply(dat.org_mtrc,c(1,2),moments::skewness) # skewness
apply(dat.org_mtrc,c(1,2),moments::kurtosis) # kurtosis


# producing a dataframe indicating the method on the 1st column,
# CE on the 2nd and AUC on the 3rd. With tmp_M reps for each method
dat.org_forgraphs = t(dat.org_mtrc[1,,])

for(i in 2:tmp_lmeth){dat.org_forgraphs=rbind(dat.org_forgraphs,t(dat.org_mtrc[i,,]))}

dat.org_forgraphs = cbind.data.frame(Modelos=rep(tmp_methods,each=tmp_M),dat.org_forgraphs,row.names = 1:(tmp_lmeth*tmp_M))

rm(list = ls(pattern="tmp.org_"),"i","j")

# ==============================================================================
#                          ICA transformed features
# ==============================================================================

# stores the metrics for each method
# array, 1st dimension gives the method, 2nd gives the metric and 3rd gives the 
# mc rep
dat.ica_mtrc = array(NA,dim = c(tmp_lmeth,2,tmp_M),dimnames=list(tmp_methods,c("Taxa de erro","AUC"),1:tmp_M))#lapply(1:tmp_lmeth, function(a) matrix(NA,nrow=tmp_M,ncol=2,dimnames = list(1:tmp_M,c("errorRt","AUC"))))

# ---------------------------
# holdout with MC
# ---------------------------

tmp_pb <- txtProgressBar(min = 0,max = tmp_M,style = 3,width = 50,char = "=") # progress bar
for(i in 1:tmp_M){
 tmp.ica_x = cbind.data.frame(tmp.ica_Xtr[[i]],class=tmp_Ytr[[i]])
 for(j in 1:tmp_lmeth){
  tmp.ica_mod  = fit(class~.,data=tmp.ica_x,model=tmp_classf[j],task="c")
  tmp.ica_mod2 = fit(class~.,data=tmp.ica_x,model=tmp_classf[j],task="p")
  
  tmp.ica_yhat = predict(tmp.ica_mod,tmp.ica_Xts[[i]])  # class predictions
  tmp.ica_phat = predict(tmp.ica_mod2,tmp.ica_Xts[[i]]) # prob predictions
  
  dat.ica_mtrc[j,1,i] = mmetric(tmp_Yts[[i]],tmp.ica_yhat,metric="CE")  # error rate 
  dat.ica_mtrc[j,2,i] = mmetric(tmp_Yts[[i]],tmp.ica_phat,metric="AUC") # AUC
 }
 setTxtProgressBar(tmp_pb, i)
};close(tmp_pb);beepr::beep()

# ---------------------------
# MC estimates
# ---------------------------

apply(dat.ica_mtrc,c(1,2),mean)              # means
apply(dat.ica_mtrc,c(1,2),sd)                # standard deviation
apply(dat.ica_mtrc,c(1,2),moments::skewness) # skewness
apply(dat.ica_mtrc,c(1,2),moments::kurtosis) # kurtosis


# producing a dataframe indicating the method on the 1st column,
# CE on the 2nd and AUC on the 3rd. With tmp_M reps for each method
dat.ica_forgraphs = t(dat.ica_mtrc[1,,])

for(i in 2:tmp_lmeth){dat.ica_forgraphs=rbind(dat.ica_forgraphs,t(dat.ica_mtrc[i,,]))}

dat.ica_forgraphs = cbind.data.frame(Modelos=rep(tmp_methods,each=tmp_M),dat.ica_forgraphs,row.names = 1:(tmp_lmeth*tmp_M))

rm(list = ls(pattern="tmp.ica_"),"i","j")

# ==============================================================================
#                         LDA err selected features
# ==============================================================================

# stores the metrics for each method
# array, 1st dimension gives the method, 2nd gives the metric and 3rd gives the 
# mc rep
dat.lda_mtrc = array(NA,dim = c(tmp_lmeth,2,tmp_M),dimnames=list(tmp_methods,c("Taxa de erro","AUC"),1:tmp_M))#lapply(1:tmp_lmeth, function(a) matrix(NA,nrow=tmp_M,ncol=2,dimnames = list(1:tmp_M,c("errorRt","AUC"))))

# ---------------------------
# holdout with MC
# ---------------------------

tmp_pb <- txtProgressBar(min = 0,max = tmp_M,style = 3,width = 50,char = "=") # progress bar
for(i in 1:tmp_M){
 tmp.lda_x = cbind.data.frame(tmp_Xtr[[i]],class=tmp_Ytr[[i]])
 for(j in 1:tmp_lmeth){
  tmp.lda_mod  = fit(class~.,data=tmp.lda_x[,c(7,dat.err_regress[[1]])],model=tmp_classf[j],task="c")
  tmp.lda_mod2 = fit(class~.,data=tmp.lda_x[,c(7,dat.err_regress[[1]])],model=tmp_classf[j],task="p")
  
  tmp.lda_yhat = predict(tmp.lda_mod,tmp_Xts[[i]][,dat.err_regress[[1]]])  # class predictions
  tmp.lda_phat = predict(tmp.lda_mod2,tmp_Xts[[i]][,dat.err_regress[[1]]]) # prob predictions
  
  dat.lda_mtrc[j,1,i] = mmetric(tmp_Yts[[i]],tmp.lda_yhat,metric="CE")  # error rate 
  dat.lda_mtrc[j,2,i] = mmetric(tmp_Yts[[i]],tmp.lda_phat,metric="AUC") # AUC
 }
 setTxtProgressBar(tmp_pb, i)
};close(tmp_pb);beepr::beep()

# ---------------------------
# MC estimates
# ---------------------------

apply(dat.lda_mtrc,c(1,2),mean)              # means
apply(dat.lda_mtrc,c(1,2),sd)                # standard deviation
apply(dat.lda_mtrc,c(1,2),moments::skewness) # skewness
apply(dat.lda_mtrc,c(1,2),moments::kurtosis) # kurtosis


# producing a dataframe indicating the method on the 1st column,
# CE on the 2nd and AUC on the 3rd. With tmp_M reps for each method
dat.lda_forgraphs = t(dat.lda_mtrc[1,,])

for(i in 2:tmp_lmeth){dat.lda_forgraphs=rbind(dat.lda_forgraphs,t(dat.lda_mtrc[i,,]))}

dat.lda_forgraphs = cbind.data.frame(Modelos=rep(tmp_methods,each=tmp_M),dat.lda_forgraphs,row.names = 1:(tmp_lmeth*tmp_M))

rm(list = ls(pattern="tmp.lda_"),"i","j")

# ==============================================================================
#                                       Graphs
# ==============================================================================

# ---------------------------
# Original
# ---------------------------

# violin plot for error rate
ggplot(dat.org_forgraphs, aes(x=Modelos, y=`Taxa de erro`,fill=Modelos)) + 
 geom_violin(trim=F) + geom_boxplot(width=.1) + ylim(5,31) + theme_light()
# violin plot for AUC
ggplot(dat.org_forgraphs, aes(x=Modelos, y=AUC,fill=Modelos)) + 
 geom_violin(trim=F) + geom_boxplot(width=.1) + ylim(.5,1) + theme_light()

# ---------------------------
# ICA transformed
# ---------------------------

# violin plot for error rate
ggplot(dat.ica_forgraphs, aes(x=Modelos, y=`Taxa de erro`,fill=Modelos)) + 
 geom_violin(trim=F) + geom_boxplot(width=.1) + ylim(5,45) + theme_light()
# violin plot for AUC
ggplot(dat.ica_forgraphs, aes(x=Modelos, y=AUC,fill=Modelos)) + 
 geom_violin(trim=F) + geom_boxplot(width=.1) + ylim(.5,1) + theme_light()

# ---------------------------
# LDA selected
# ---------------------------

# violin plot for error rate
ggplot(dat.lda_forgraphs, aes(x=Modelos, y=`Taxa de erro`,fill=Modelos)) + 
 geom_violin(trim=F) + geom_boxplot(width=.1) + ylim(5,35) + theme_light()
# violin plot for AUC
ggplot(dat.lda_forgraphs, aes(x=Modelos, y=AUC,fill=Modelos)) + 
 geom_violin(trim=F) + geom_boxplot(width=.1) + ylim(.5,1) + theme_light()

# ---------------------------
# All on same graph
# ---------------------------

dat_forgraphs = rbind(dat.org_forgraphs,dat.ica_forgraphs,dat.lda_forgraphs)

dat_forgraphs = cbind.data.frame(Variaveis = rep(c("Original","ICA","Sel. LDA"),each=tmp_lmeth*tmp_M),
                                 dat_forgraphs)

# violin plot for error rate
ggplot(dat_forgraphs, aes(x=Variaveis, y=`Taxa de erro`,fill=Modelos)) + 
 geom_violin(trim=F) +geom_boxplot(width=.1,position=position_dodge(.9))+ ylim(5,45) + theme_light()

# violin plot for AUC
ggplot(dat_forgraphs, aes(x=Variaveis, y=AUC,fill=Modelos)) + 
 geom_violin(trim=F) +geom_boxplot(width=.1,position=position_dodge(.9))+ ylim(.5,1) + theme_light()

rm(list = ls(pattern="tmp_"))

save.image("Monte Carlo holdout.RData")

# ver o de multiple groups dps
# http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
