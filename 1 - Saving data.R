setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
# reading dataset

raw_data = read.table("data/column_2C.dat",h=F,sep=" ",dec='.')
View(raw_data)

colnames(raw_data) = c("pelIncidence",
                   "pelTilt",
                   "lumbLordAngle",
                   "sacrSlope", 
                   "pelRadius", 
                   "grdSpondy",
                   "class")

cnst_n = nrow(raw_data) # n obs
cnst_p = ncol(raw_data)-1 # n features

# dataset about vertebral column deformities
# description given on https://archive.ics.uci.edu/ml/datasets/Vertebral+Column
# target variable on last column

save.image("Data.RData")
