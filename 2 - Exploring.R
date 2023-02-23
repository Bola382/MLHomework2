rm(list=ls())
load("Data.RData")
library(ggplot2);suppressWarnings({library(dplyr) 
 library(GGally)})
temp_rmnames = function(a) {names(a)=NULL;  return(a)}

# ==============================================================================
#                               Actual variables
# ==============================================================================

# ----------------------------------
# proportion of classes in target
# ----------------------------------

raw_data %>% select(last_col()) %>% table %>% prop.table()

# ----------------------------------
# normality tests
# ----------------------------------

dat_features = raw_data %>% select(-last_col())
dat_class = raw_data %>% select(last_col()) 

MVN::mvn(dat_features[raw_data$class=="AB",])$multivariateNormality
MVN::mvn(dat_features[raw_data$class=="NO",])$multivariateNormality

# ----------------------------------
# plots
# ----------------------------------

ggpairs(as.data.frame(dat_features),aes(color = dat_class %>% unlist %>% temp_rmnames %>% factor)) + theme_bw()

# ==============================================================================
#                               PCA transformed
# ==============================================================================

# ----------------------------------
# PCA
# ----------------------------------

mod_pca = princomp(dat_features)
dat_PCAfeatures = mod_pca$scores

# ----------------------------------
# plots
# ----------------------------------

ggpairs(as.data.frame(dat_PCAfeatures),aes(color = dat_class %>% unlist %>% temp_rmnames %>% factor)) + theme_bw()

# ==============================================================================
#                               ICA transformed
# ==============================================================================

# ----------------------------------
# ICA
# ----------------------------------

mod_ica = ica::icafast(dat_features,nc=cnst_p)
dat_ICAfeatures = mod_ica$S

# ----------------------------------
# plots
# ----------------------------------

ggpairs(as.data.frame(dat_ICAfeatures),aes(color = dat_class %>% unlist %>% temp_rmnames %>% factor)) + theme_bw()

rm(list = ls(pattern="temp_"))
rm(list = ls(pattern="raw_"))

save.image("organized.RData")
