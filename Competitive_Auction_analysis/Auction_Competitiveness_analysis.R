library(rpart)
library(rpart.plot)
library(caret)

# read ebay datafile
ebay=read.csv('eBayAuctions.csv',stringsAsFactors = FALSE)

# show head
head(ebay)
# structure of data
str(ebay)

# get currency as factor
ebay$currency = as.factor(ebay$currency)

# get no of rows in 
n_row=nrow(ebay)

# sample row index randomly from ebay's row index. Size = 60% of total number of rows in ebay's data
train_row_sample = sample(1:n_row,size = 0.6*n_row,replace=FALSE)

# subset training set
ebay_train = ebay[train_row_sample,]

#subset test set
ebay_test = ebay[-train_row_sample,]

# create a model
fit = rpart(Competitive.~.,data=ebay_train,method='class')

# display tree
prp(fit, type = 1, extra = 1, under = TRUE, split.font = 0.5, varlen = -5)

# check prediction on training set itself
fit_train=predict(fit,ebay_train,type = 'class')

# check accuracy on training set
confusionMatrix(fit_train, ebay_train$Category)

# prediction on test set
fit_test=predict(fit,ebay_test,type = 'class')

# check accuracy on test set
confusionMatrix(fit_test, as.factor(ebay_test$Competitive.))

#####################################################################################################
##########  DEEPER TREE  DEEPER TREE  DEEPER TREE  DEEPER TREE  DEEPER TREE  DEEPER TREE ############
#####################################################################################################

# create deeper tree
deeper_fit <- rpart(Competitive. ~ ., data = ebay_train, method = "class", cp = 0, minsplit = 1)

# display deeper tree
prp(deeper_fit, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -3,
    box.col=ifelse(deeper_fit$frame$var == "<leaf>", 'gray', 'white'))

# prediction on test set
deep_predict_train = predict(deeper_fit,ebay_train,type='class')

# check accuracy on training set
confusionMatrix(deep_predict_train, as.factor(ebay_train$Competitive.))

# check accuracy on test set
confusionMatrix(predict(deeper_fit,ebay_test,type='class'), as.factor(ebay_test$Competitive.))

#####################################################################################################
##########  PRUNED TREE  PRUNED TREE  PRUNED TREE  PRUNED TREE  PRUNED TREE  PRUNED TREE ############
#####################################################################################################

pruned_tree <- rpart(Competitive. ~ ., data = ebay_train, method = "class",
                     cp = 0.00001, minsplit = 5, xval = 5)

# statistics related to error of pruned tree
# check which cp to choose. Acceptable error deviation with respect to cp is
# is within 1 standard deviation
printcp(pruned_tree)

# show pruned tree
prp(pruned_tree, type = 1, extra = 1, split.font = 1, varlen = -3)

pruned.ct <- prune(pruned_tree,
                   cp = pruned_tree$cptable[which.min(pruned_tree$cptable[,"xerror"]),"CP"])

# show pruned tree
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = 3)

# predict on training set
pruned_fit_train = predict(pruned.ct,ebay_train)

# accuracy on training set
confusionMatrix(pruned_fit_train, as.factor(ebay_train$Competitive.))

# predict on test set
pruned_fit_test = predict(pruned.ct,ebay_test,type='class')

# accuracy on test set
confusionMatrix(pruned_fit_test, as.factor(ebay_test$Competitive.))

#####################################################################################################
##########  CONTROLLING DEPTH  CONTROLLING DEPTH  CONTROLLING DEPTH  CONTROLLING DEPTH ##############
#####################################################################################################


# controlling depth
max_deep_fit = rpart(Competitive.~.,data=ebay_train,control = rpart.control(maxdepth = 7),method='class')

# display the tree
prp(max_deep_fit, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = 3)

max_deep_train = predict(max_deep_fit,ebay_train,type='class')

confusionMatrix(max_deep_train, as.factor(ebay_train$Competitive.))

max_deep_test = predict(max_deep_fit,ebay_test,type='class')

confusionMatrix(max_deep_test, as.factor(ebay_test$Competitive.))

#####################################################################################################
############  CONTROLLING DEPTH AND BUCKET SIZE |  CONTROLLING DEPTH AND BUCKET SIZE  ###############
#####################################################################################################

# controlling depth and bucket_size
max_deep_fit_bucket = rpart(Competitive.~.-ClosePrice,
                            data=ebay_train,
                            control = rpart.control(maxdepth = 7, 
                                                    minbucket = 50),
                            method='class')

# display tree
prp(max_deep_fit_bucket, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = 10)

max_deep_bucket_test = predict(max_deep_fit_bucket,ebay_test,type='class')

confusionMatrix(max_deep_test, as.factor(ebay_test$Competitive.))

# check if we do need model! 
sum(ebay$Competitive.)/nrow(ebay)

# is our accuracy better than this? Yes, Way better

####################################################################################################
####  CROSS VALIDATION AND HYPERPARAMETER TUNING | CROSS VALIDATION AND HYPERPARAMETER TUNING  #####
####################################################################################################

# blew it haha

names(ebay)
#[1] "Category"     "currency"     "sellerRating" "Duration"     "endDay"      
#[6] "ClosePrice"   "OpenPrice"    "Competitive."
# Category: type of product for auction
# currency: country that currency belongs to
# sellerRating: 
# Duration: time the auction ran for
# endDay: day the auction ended
# ClosePrice: price that auction ended in
# OpenPrice: price that auction started in
# Competitive.: if the auction was competitive

# controlling depth and bucket_size
max_deep_fit_bucket = rpart(Competitive.~.-ClosePrice,
                            data=ebay_train,
                            control = rpart.control(maxdepth = 7, 
                                                    minbucket = 50),
                            method='class')

prp(max_deep_fit_bucket, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = 10)

# removing CLosePrice and endDay
excluded_vars = c("ClosePrice","endDay")
max_deep_fit_bucket = rpart(Competitive.~.,
  data=ebay_train[,c(#"Category",
                     "currency",
                     "sellerRating",
                     "Duration",
                     "endDay",
                     #"ClosePrice",
                     "OpenPrice",
                     "Competitive.")],
  control = rpart.control(maxdepth = 7, 
                          minbucket = 50),
  method='class')

prp(max_deep_fit_bucket, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = 10)

max_deep_bucket_test = predict(max_deep_fit_bucket,ebay_test[,c("Category",
                        "currency",
                        "sellerRating",
                        "Duration",
                        "endDay",
                        "ClosePrice",
                        "OpenPrice",
                        "Competitive.")],type='class')

confusionMatrix(max_deep_bucket_test, as.factor(ebay_test$Competitive.))
