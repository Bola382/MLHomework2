rm(list=ls())
load("var select.RData")
library(rminer);suppressMessages(library(dplyr))

dat_class = dat_class$class %>% factor # target

# ==============================================================================
#                             Original features
# ==============================================================================

X = data %>% select(-last_col())
Y = data %>% select(last_col()) %>% unlist %>% rmnames %>% factor
n = nrow(X)
m = 100 # reps monte carlo

methods = c("KNN","DT","SVM","MULTINOM")
lmeth = length(methods)

tx_erro = matrix(NA, m, lmeth,
                 dimnames = list(1:m,methods))

# Balanced holdout
pp = .5

id_tr = matrix(NA,nrow=m,ncol=round(pp*n, 0))

Xtr = Xts = Ytr = Yts = vector(mode="list",length = m)

set.seed(1)
for(i in 1:m){
 id_tr[i,] = sample(1:n, size = trunc(pp*n), replace = FALSE)
 
 # 1 - Conjunto de Treino e Teste
 
 Xtr[[i]] = X[ id_tr[i,],]  # Treino
 Xts[[i]] = X[-id_tr[i,],]  # Teste
 Ytr[[i]] = Y[ id_tr[i,]]
 Yts[[i]] = Y[-id_tr[i,]]
}

# barra de probresso
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = m, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar
tm = proc.time()[3]
for(k in 1:m){
 # 2 - Model
 
 dbtreino = cbind.data.frame(Xtr[[k]], classe = Ytr[[k]])
 dbteste  = cbind.data.frame(Xts[[k]], classe = Yts[[k]])
 
 mod5NN   = fit(classe ~ ., data = dbtreino,
                model = "knn", task = "c",k=5)
 mod10NN   = fit(classe ~ ., data = dbtreino,
                 model = "knn", task = "c",k=10)
 mod15NN   = fit(classe ~ ., data = dbtreino,
                 model = "knn", task = "c",k=15)
 modNNB   = fit(classe ~ ., data = dbtreino,
                model = "naiveBayes", task = "c")
 modLDA   = fit(classe ~ ., data = dbtreino,
                model = "lda", task = "c")
 #modQDA   = fit(classe ~ ., data = dbtreino,
 #               model = "qda", task = "c")
 modDT   = fit(classe ~ ., data = dbtreino,
               model = "dt", task = "c")
 modSVM   = fit(classe ~ ., data = dbtreino,
                model = "svm", task = "c")
 modMULTINOM   = fit(classe ~ ., data = dbtreino,
                     model = "multinom", task = "c")
 # 3 - Avaliation
 
 Ypred5NN   = predict(mod5NN, dbteste)
 Ypred10NN   = predict(mod10NN, dbteste)
 Ypred15NN  = predict(mod15NN, dbteste)
 YpredNNB   = predict(modNNB, dbteste)
 YpredLDA   = predict(modLDA, dbteste)
 #YpredQDA  = predict(modQDA, dbteste)
 YpredDT    = predict(modDT, dbteste)
 YpredSVM   = predict(modSVM, dbteste)
 YpredMULTINOM  = predict(modMULTINOM, dbteste)
 
 tx_erro[k,"5NN"] = (1 - sum(diag(prop.table(table(Yts[[k]], Ypred15NN))))) * 100
 tx_erro[k,"10NN"] = (1 - sum(diag(prop.table(table(Yts[[k]], Ypred10NN))))) * 100
 tx_erro[k,"15NN"] = (1 - sum(diag(prop.table(table(Yts[[k]], Ypred15NN))))) * 100
 tx_erro[k,"NNB"] = (1 - sum(diag(prop.table(table(Yts[[k]], YpredNNB))))) * 100
 tx_erro[k,"LDA"] = (1 - sum(diag(prop.table(table(Yts[[k]], YpredLDA))))) * 100
 #tx_erro[k,"QDA"] = (1 - sum(diag(prop.table(table(Yts[[k]], YpredQDA))))) * 100
 tx_erro[k,"DT"] = (1 - sum(diag(prop.table(table(Yts[[k]], YpredDT))))) * 100
 tx_erro[k,"SVM"] = (1 - sum(diag(prop.table(table(Yts[[k]], YpredSVM))))) * 100
 tx_erro[k,"MULTINOM"] = (1 - sum(diag(prop.table(table(Yts[[k]], YpredMULTINOM))))) * 100
 setTxtProgressBar(pb, k)
};print(proc.time()[3]-tm);beepr::beep()


# MC estimate

colMeans(tx_erro)
apply(tx_erro, 2, FUN = sd)
apply(tx_erro, 2, FUN = function(a) sd(a)/mean(a)*100)
apply(tx_erro, 2, FUN = moments::skewness)

boxplot(tx_erro)
boxplot(tx_erro[,-4])

mt = rep(methods,each=100)
tx_erro2 = cbind.data.frame(rate=as.vector(tx_erro),Modelo=mt)

ggplot(tx_erro2, aes(x=rate, color=Modelo, fill=Modelo)) +
 geom_histogram(aes(y=after_stat(density)), position="identity", alpha=0.5)+
 geom_density(alpha=0.6)+
 labs(title="",x="Taxa de erro", y = "Densidade")+
 theme_classic()

tx_erro3 = tx_erro2[-which(tx_erro2=="NNB",arr.ind = T)[,1],]

ggplot(tx_erro3, aes(x=rate, color=Modelo, fill=Modelo)) +
 #geom_histogram(aes(y=after_stat(density)), position="identity", alpha=0.5)+
 geom_density(alpha=0.6)+
 labs(title="",x="Taxa de erro", y = "Densidade")+
 theme_classic()

# ==============================================================================
#                          ICA transformed features
# ==============================================================================

# ==============================================================================
#                         LDA err selected features
# ==============================================================================