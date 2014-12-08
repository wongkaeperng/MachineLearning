Executive Summary
==================
This project involves predicting the manner in which participant did the exercise (classe dependent variable) using data from accelerometers. In order to prevent overfitting, the training data set is split into 70% training, 30% for cross validation. 

Random forest is used to build the model and an accuracy of 96.3% is achieved on the training data set and an accuracy of 97.8% is achieved on the testing data set.

Preprocessing Data
==================

```r
# Download file into Working Directory and Load them.
training <- read.csv(file = "pml-training.csv",header = TRUE, sep = ",")
testing <- read.csv(file = "pml-testing.csv",header = TRUE, sep = ",")
# Delete entire NA columns
del_na_cols <- function(x) { x[ , colSums( is.na(x) ) < nrow(x) ] }
training <- del_na_cols(training)
# Remove variables with NA
remove_na <- function(x) {x[,sapply(x, function(y) !any(is.na(y)))] }
training <- remove_na(training)
```

Prepare Cross Validation Data
=============================

```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.2
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
inTrain <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
training2 <- training[inTrain, ]
cross_validation <- training[-inTrain, ]
```

Data Exploration
=================

```r
# Explore Correlations of Independent Variables vs Dependant Variables using Feature Plot
featurePlot(x=training2[,c("roll_belt","pitch_forearm","yaw_belt","magnet_dumbbell_y","pitch_belt", "magnet_dumbbell_z", "roll_forearm", "accel_dumbbell_y", "roll_dumbbell", "magnet_dumbbell_x")],y=training$classe,plot="pairs")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Random Forest Prediction Model
==============================
From the data exploration, select the independent variables that show relationship / association with classe as a subset. This is so as putting all variables into the rain forest model took too much time.


```r
subsetTrainingData <- subset(training2,select=c(roll_belt, pitch_forearm, yaw_belt, magnet_dumbbell_y, pitch_belt, magnet_dumbbell_z, roll_forearm, accel_dumbbell_y, roll_dumbbell, magnet_dumbbell_x,classe))
```

From the small subset, build the random forest model:

```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.1.2
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
# setting control variable to build the Random Forest
ctrl <- trainControl(method="cv", number=2)
# building the model
set.seed(1234)
Model <- train(classe ~ ., data=subsetTrainingData[inTrain,], model="rf", trControl=ctrl)
Model
```

```
## Random Forest 
## 
## 13737 samples
##    10 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (2 fold) 
## 
## Summary of sample sizes: 4808, 4810 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa   Accuracy SD  Kappa SD
##    2    0.9631    0.9534  0.005135     0.006493
##    6    0.9638    0.9543  0.004401     0.005558
##   10    0.9610    0.9507  0.003664     0.004637
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 6.
```

Using mtry=6, accuracy achieved 96.3%. 

Run Model with Cross Validation
================================
Run Model with Cross Validation dataset (30% of training data)


```r
testCrossVal <- predict(Model, cross_validation)
confusionMatrix(cross_validation$classe, testCrossVal)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1662    7    0    4    1
##          B   15 1088   23   13    0
##          C    6   14  996   10    0
##          D    0    0    9  955    0
##          E    4   17    9    5 1047
## 
## Overall Statistics
##                                        
##                Accuracy : 0.977        
##                  95% CI : (0.973, 0.98)
##     No Information Rate : 0.287        
##     P-Value [Acc > NIR] : < 2e-16      
##                                        
##                   Kappa : 0.971        
##  Mcnemar's Test P-Value : 2.39e-09     
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.985    0.966    0.960    0.968    0.999
## Specificity             0.997    0.989    0.994    0.998    0.993
## Pos Pred Value          0.993    0.955    0.971    0.991    0.968
## Neg Pred Value          0.994    0.992    0.992    0.993    1.000
## Prevalence              0.287    0.191    0.176    0.168    0.178
## Detection Rate          0.282    0.185    0.169    0.162    0.178
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       0.991    0.978    0.977    0.983    0.996
```

Accuracy is 97.8%.

Predict with Test Data
======================
Next, we run the model with test data.

```r
predictTest <- predict(Model, testing)
predictTest
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

Write the Predictions to File
=============================

```r
# Function to write a vector to files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_", i ,".txt")
    write.table(x[i], file = filename, quote = FALSE,
                row.names = FALSE, col.names = FALSE)
  }
}
```

Conclusion
==========
The accuracy of the prediction model on the cross validation dataset is high at 97.91%. This suggests a very accurate model. To further improve the model, perhaps one can impute the missing values instead of removing them like what we had done.

