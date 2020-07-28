library(tidyverse)

# The tf-idf plots data
load("../Modeling/models_tfidf_plots.RData")

# The tf-idf top terms 

tf_idf_terms2 <- read_csv('../Prelim_Results/tf_idf_class2.csv', col_types = c('cciddd'))
tf_idf_terms3 <- read_csv('../Prelim_Results/tf_idf_class3.csv', col_types = c('cciddd'))
tf_idf_terms4 <- read_csv('../Prelim_Results/tf_idf_class4.csv', col_types = c('cciddd'))

# Tags and Functions

nTags <- read_csv('../Modeling/nTags.csv')

# kNN

knn_fit_k2 <- read_csv('../Modeling/knnfit_k2.csv', col_types = c('fdi'))
knn_fit_k3 <- read_csv('../Modeling/knnfit_k3.csv', col_types = c('fdi'))
knn_fit_k4 <- read_csv('../Modeling/knnfit_k4.csv', col_types = c('fdi'))


# boosted tree

bst_fit_k2 <- read_csv('../Modeling/boost_k2.csv', col_types = c('fd'))
bst_fit_k3 <- read_csv('../Modeling/boost_k3.csv', col_types = c('fd'))
bst_fit_k4 <- read_csv('../Modeling/boost_k4.csv', col_types = c('fd'))

bst_mscls <- read_csv('../Modeling/bst_miscls.csv', col_types = c('fd'))
