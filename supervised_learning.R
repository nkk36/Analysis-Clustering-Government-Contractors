# Setup ====
# Load packages
library(caret)
library(data.table)
library(glmnet)
library(nnet)
library(randomForest)

# Load data
df = fread("data/naics2D_13clusters.csv")

# Train/Test Split ====
set.seed(123)
trainIndex = createDataPartition(df$cluster, p = .7, 
                                 list = FALSE, 
                                 times = 1)

df = as.data.frame(df)
df$cluster = as.factor(df$cluster)

df_Train <- df[trainIndex,]
df_Test  <- df[-trainIndex,]

remove(df)

# Train Model ====
model_multinom = multinom(formula = cluster ~ .,
                          data = df_Train)
temp_train = as.matrix(df_Train[,1:24])
rownames(temp_train) = 1:nrow(temp_train)
model_multinom_ridge = glmnet(x = temp_train, 
                              y = df_Train$cluster,
                              family = "multinomial",
                              alpha = 0)
model_multinom_lasso = glmnet(x = temp_train, 
                              y = df_Train$cluster,
                              family = "multinomial",
                              alpha = 1)
model_svm_linear = train(form = cluster ~ ., 
                         data = df_Test,
                         method = "svmLinear")


# Predict on Test Set ====
p_multinom = predict(model_multinom, 
                     newdata = df_Test,
                     type = "class")
temp_test = as.matrix(df_Test[,1:24])
p_multinom_ridge = predict(object = model_multinom_ridge, 
                                  newx = temp_test,
                                  type = "class")
p_multinom_lasso = predict.glmnet(object = model_multinom_lasso, 
                                  newx = temp_test,
                                  type = "class")
p_svm_linear = predict(model_svm_linear, 
                       newdata = df_Test,
                       type = "raw")

# Evaluate test set ====
confMatrix_multinom = confusionMatrix(data = p_multinom, 
                                      reference = df_Test$cluster)
confMatrix_multinom_ridge = confusionMatrix(data = p_multinom_ridge, 
                                            reference = df_Test$cluster)
confMatrix_multinom_lasso = confusionMatrix(data = p_multinom_lasso, 
                                            reference = df_Test$cluster)
confMatrix_svm_linear = confusionMatrix(data = p_svm_linear, 
                                        reference = df_Test$cluster)

















