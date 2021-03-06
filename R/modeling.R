library(xgboost)
library(caret)
library(dplyr)
library(Matrix)

titanic <- readxl::read_excel("data/titanic3.xls") %>%
  dplyr::select(survived, pclass, sex, age, sibsp, fare) %>%
  dplyr::mutate(pclass = factor(pclass),
                sex = factor(sex),
                sibsp = ifelse(sibsp == 0, 0, 1),
                sibsp = factor(sibsp)) %>%
  as.data.frame()

titanic <- titanic[complete.cases(titanic), ]
set.seed(32)
train_rows<- createDataPartition(y = titanic$survived, p=0.8, list = FALSE)
titanic_train <- titanic[train_rows,] 
titanic_test <- titanic[-train_rows,]

titanic_train_sparse <- sparse.model.matrix(survived ~ .-1, 
                                            data = titanic_train
                                            )
titanic_test_sparse <- sparse.model.matrix(survived ~ .-1, 
                                           data = titanic_test
                                           )

bst <- xgboost(data = titanic_train_sparse, label = titanic_train$survived,
               max.depth = 4, eta = 1, nthread = 2, nrounds = 10,
               objective = "binary:logistic")

test_pred <- predict(bst, newdata = titanic_test_sparse)

confusionMatrix(factor(round(test_pred)), 
                factor(titanic_test$survived), 
                mode = "everything")

save(bst, test_pred, titanic_test, file = "data/xgboost.rda")


