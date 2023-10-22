######################################################
######################################################
# Data Visualizations and Analytics - Part 3
######################################################
######################################################

#######################
# Load Libraries
#######################
library(readr)
library(data.table)
library(caret)
library(dplyr)
library(randomForest)

# Clear the workspace
rm(list=ls())

#######################
# 1) Explain features data set
#######################

# file path for one file to explore.
## Laptop Folder
# file_path <- "C:/Users/brian/OneDrive/CIT MSc Data Science Modules/DATA9005 Data Analytics and Visualization/Project Two - Neural Networks/Assignmnet 2 - Question 3/kvasir-dataset-v2-features/dyed-lifted-polyps/00cf9508-6ad1-4db9-840a-519c1d515c30.features"

## Desktop Folder
file_path <- "C:/Users/Kolobane/OneDrive/CIT MSc Data Science Modules/DATA9005 Data Analytics and Visualization/Project Two - Neural Networks/Assignmnet 2 - Question 3/kvasir-dataset-v2-features/dyed-lifted-polyps/00cf9508-6ad1-4db9-840a-519c1d515c30.features"

file_contents <- readLines(file_path)
file_contents
## The feature file has multiple feature sets (JCD, Tamura, ColorLayout, etc)
## Each Feature has different lengths.

# Create an empty named list to store the features
feature_list <- list()

# Process each line in the file
for(line in file_contents){
  # split the line by the : (colon) seperator
  feature_parts <- strsplit(line, ":")[[1]]
  
  # get the feture names and feature values
  feature_name <- feature_parts[1]
  feature_values <- strsplit(feature_parts[2], ",")[[1]]
  
  # Change the feature vales to a numeric value
  feature_values_numeric <- as.numeric(feature_values)
  
  # add feature to the feature_list
  feature_list[[feature_name]] <- feature_values_numeric
}

# Print a the output of the feature list
print(feature_list)
class(feature_list) # class of type list

######################
# Object to store the amount of elements
total_count_elements <- 0

# For loop to itearte though the 
for (feature in feature_list){
  total_count_elements <- total_count_elements + length(feature)
  }

print(total_count_elements)
# I get 1185 but still short 1.

# get the feature names
print(names(feature_list))


#######################
# 2) Using R construct your own 1186 feature vector for one of these 
# feature files, i.e. for one image.
#######################

#######################
# Join the feature list into one vector

# join the feature_list parts together to one list.
single_vector <- unlist(feature_list, use.names=FALSE)

# Find the number of elements in the new vector
print(length(single_vector)) ## 1185

## So short the one element
## Will add this below.

#######################
# Additional information

# Print out the global features and their elements
print(feature_list)

# Get the global feature names
print(names(feature_list))

# Get a count of the number of elements per global feature.
global_features <- list("JCD", "Tamura", "ColorLayout", "EdgeHistogram",
                        "AutoColorCorrelogram", "PHOG") 
count_of_elements <- vector("integer", length(feature_list))
for (i in 1:length(feature_list)) {
  count_of_elements[i] <- length(feature_list[[i]])
}
print(count_of_elements) # for each global feature
# [1] 168  18  33  80 256 630

#######################
# 3) Now create R code to construct your own dataset of 1186 
# feature vectors for all these feature/image files. 
# Make sure that if one feature file has a different number 
# of values that your code will deal with this correctly.
#######################

# Set the main directory
path_dir <- "kvasir-dataset-v2-features"

# Sub Folders for all the feature files
sub_folders <- c("dyed-lifted-polyps", "dyed-resection-margins", 
                "esophagitis", "normal-cecum", "normal-pylorus", 
                "normal-z-line", "polyps", "ulcerative-colitis")

# List of all the global feature names to make sure they are in the right order
global_feature_list <- c("JCD", "Tamura", "ColorLayout", "EdgeHistogram",
                         "AutoColorCorrelogram", "PHOG")
global_feature_element_list <- c(168,  18,  33,  80, 256, 630)

# List to store all the single_vectors
combined_vector_list <-list()

# Steps:
# 1. Access all the feature files
# 2. Check if the Global Feature names are in the same order AND the global feature
## element amount is the same 
# 3. If this is the case, create a single vector and store this value in a data_frame

# Iterate though each subfolder
for (subfolder in sub_folders){
  # List all the files in the subfolder
  feature_files <- list.files(file.path(path_dir, subfolder), full.names=TRUE)
  
  # Now, iterate though each feature_file
  for (feature_file in feature_files) {
    # Read the contents of each file
    file_contents <- readLines(feature_file)
    
    # Empty list
    feature_list <- list()
    
    # Process each line in the file
    for(line in file_contents){
      feature_parts <- strsplit(line, ":")[[1]]
      feature_name <- feature_parts[1]
      feature_value <- strsplit(feature_parts[2], ",")[[1]]
      feature_value_numeric <-as.numeric(feature_value)
      feature_list[[feature_name]] <- feature_value_numeric
    }
      
    # Now check the conditions
    feature_names_correct <- all(names(feature_list) == global_feature_list)
    feature_counts_correct <-all(sapply(feature_list, length) == global_feature_element_list)
      
    # Need to know if this is working so print messages to screen
    cat("Processing Subfolder:", subfolder, "\n")
    cat("Processing feature file:", basename(feature_file), "\n")
      
    # If both conditions are met save, create a new single_vector and save it.
    if (feature_names_correct && feature_counts_correct) {
      combined_vector <- unlist(feature_list, use.names = FALSE)
        
      # Append the single_vector to the data frame
      combined_vector_list[[length(combined_vector_list) + 1]] <- combined_vector
      cat(" Saving single vector .\n")
      cat("\n")
    }
  } 
}

# print(combined_vector) 
# class(combined_vector_list)
# print(combined_vector_list)

#######################
# Count the number of elements in the list
num_elements <- length(combined_vector_list)
num_elements # 11964 is wrong
## This was an initial mistake it is ok now. 8000

# count the number of files
total_file_count <- 0
for (subfolder in sub_folders){
  feature_files <- list.files(file.path(path_dir, subfolder), full.names=TRUE)
  file_count <- length(feature_files)
  total_file_count <- total_file_count + file_count
}
print(total_file_count) # Exactly there should be 8000 files
## NOTE: So we know that all 8000 files are in the correct format.


#######################
# Interception of the 1186 single vector and arranging for Random Forrest training 
# The research paper methods 1186 elements but does not state now the extra element
# is added. Because later we will be using a Random Forrest we need to have a target variable.
# So a first method is to have the single vector 1185 associated to the each of the 
# classes (folders of images). With this method I am deliberately.

# A second method is to assign an index to each element and but then when i need to have
# a data frame I would have 1187 elements and this is beyond the 1186. The research paper
# does not offer detail. 


#######################
# Method 1
# 1186 with a unique value at the beginning.
# 1186 element data frame, target is first column

# create empty list
# combined_vector_list_unique_id <- list()

# Add a value to the beginning of the files to make it up to 1186
# for (i in seq_along(combined_vector_list)){
#   combined_vector_list_unique_id[[i]] <- c(i, combined_vector_list[[i]])
# }

# print(combined_vector_list_unique_id[80])
# print(combined_vector_list_unique_id[674])
# print(combined_vector_list_unique_id[2896])
# print(combined_vector_list_unique_id[7654])

# Create a data frame with this 1186 elements.
# method_1_unique_id_df <- data.frame(combined_vector_list)
# method_1_unique_id_df[1]

## NOTE: May not use this but have it for later.

#######################
# Method 2
# 1186 elements with each of the classes at the beginning. 1 to 8
# 1187 element data frame. Unique value at the beginning and last column is one of the
# 8 classes

# create image values from the folders to create a classification value
image_values <- list (
  "dyed-lifted-polyps" =1,
  "dyed-resection-margins" = 2, 
  "esophagitis" =3, 
  "normal-cecum" = 4, 
  "normal-pylorus" = 5,
  "normal-z-line" = 6,
  "polyps"=7,
  "ulcerative-colitis"=8)

# Don't need to make this complicated. Just add a 1 to each 1000 elements in the list

# create empty new list
combined_vector_list_classified <- list()

# Iterate thought the 8000 files and add a classification value 
for (i in 1:8000){
  class <- ceiling(i/1000)
  combined_vector_list_classified[[i]] <-c(class, combined_vector_list[[i]])
}

combined_vector_list_classified[999]
combined_vector_list_classified[1000]
combined_vector_list_classified[1001]

# place this new list into a data frame
data_df <- do.call(rbind, combined_vector_list_classified)
print(head(data_df))

# save data frame
write.csv(data_df, "data_df.csv", row.names = FALSE)

# load dataframe
data_df <- read.csv("data_df.csv")

#######################
# 4) Explain the steps how you can reproduce the ‘6 GF Random Forrest’ results 
# from Table 1 of the paper above. Try and reproduce the results using R. 
# Can you improve these results?
#######################

print(dim(data_df))
# 8000 1186
## 8000 rows and 1186 columns.

#######################
# Research paper split 50:50

# Ensure that the target variable is a factor
data_df$V1 <- factor(data_df$V1, levels=1:8)

# set seed to student ID 
set.seed(790) # Last three digits of student ID

# indicies to randomly even out the classes using Strafied sampling
indices <- createDataPartition(data_df$V1, p=0.5, list =FALSE)

# Create train, val and test databases
train_data_research <- data_df[indices,]
test_data_research <- data_df[-indices,]

x_train_research <- train_data_research[,-1]
y_train_research <- train_data_research[,1]

x_test_research <- test_data_research[,-1]
y_test_research <- test_data_research[,1]

# Caret needs column names to work
x_train_df_research <-as.data.frame(x_train_research)
x_test_df_research <-as.data.frame(x_test_research)

colnames(x_train_df_research) <- paste("Var",1:ncol(x_train_df_research), sep = "")
colnames(x_test_df_research) <- paste("Var",1:ncol(x_test_df_research), sep = "")

#######################
# Run research model with 100ntree and CV2 on 50:50 split as in the paper

#######################
# Research Model 1
# Try K-fold CV as a sampleing method and re-run model
model_caret_research_paper <- train(x = x_train_df_research,
                                    y = y_train_research,
                                    method = "rf",
                                    ntree = 100, 
                                    trControl = trainControl(method = "cv", number = 2))


model_caret_research_paper
## Results: 
## mtry  Accuracy   Kappa    
##   2  0.6950000  0.6514286
##  48  0.7648214  0.7312245
##  1185  0.7583929  0.7238776

# make predication on the test set
test_predications_research_paper <- predict(model_caret_research_paper , x_test_df_research)
research_paper_metrics <- confusionMatrix(test_predications_research_paper , y_test_research)
## Accuracy 77.08%

## Get other vales seen in paper:
PREC <- research_paper_metrics$byClass[,'Pos Pred Value']
mean_PREC <- mean(PREC)
REC <- research_paper_metrics$byClass[,'Sensitivity']
mean_REC <- mean(REC)
SPEC <- research_paper_metrics$byClass[,'Specificity']
mean_SPEC <- mean(SPEC)
ACC <- research_paper_metrics$overall['Accuracy']
F1 <- research_paper_metrics$byClass[,'F1']
mean_F1 <- mean(F1)
# FPS <- research_paper_metrics$byClass['False Positive Rate']

cat("PREC:", round(mean_PREC,3), "\n",
    "REC:", round(mean_REC,3), "\n",
    "SPEC:", round(mean_SPEC,3), "\n",
    "ACC:", round(ACC,3), "\n",
    "F1:", round(mean_F1,3), "\n"
    )
## Results:
## PREC: 0.77 
## REC: 0.771 
## SPEC: 0.967 
## ACC: 0.771 
## F1: 0.769 

#######################
# Research Model 2

# Try K-fold CV as a sampling method and re-run model
model_caret_research_paper_250 <- train(x = x_train_df_research,
                                    y = y_train_research,
                                    method = "rf",
                                    ntree = 250, 
                                    trControl = trainControl(method = "cv", number = 2))


model_caret_research_paper_250
## Results: 
## 2  0.7137500  0.6728571
## 48  0.7585714  0.7240816
## 1185  0.7512500  0.7157143

# make predication on the test set
test_predications_research_paper_250 <- predict(model_caret_research_paper_250 , x_test_df_research)
research_paper_metrics_250 <- confusionMatrix(test_predications_research_paper_250 , y_test_research)
## Accuracy 77.17%

## Get other vales seen in paper:
PREC <- research_paper_metrics_250$byClass[,'Pos Pred Value']
mean_PREC <- mean(PREC)
REC <- research_paper_metrics_250$byClass[,'Sensitivity']
mean_REC <- mean(REC)
SPEC <- research_paper_metrics_250$byClass[,'Specificity']
mean_SPEC <- mean(SPEC)
ACC <- research_paper_metrics_250$overall['Accuracy']
F1 <- research_paper_metrics_250$byClass[,'F1']
mean_F1 <- mean(F1)
# FPS <- research_paper_metrics$byClass['False Positive Rate']

cat("PREC:", round(mean_PREC,3), "\n",
    "REC:", round(mean_REC,3), "\n",
    "SPEC:", round(mean_SPEC,3), "\n",
    "ACC:", round(ACC,3), "\n",
    "F1:", round(mean_F1,3), "\n"
)
## Results:
## PREC: 0.771 
## REC: 0.772 
## SPEC: 0.967 
## ACC: 0.772 
## F1: 0.77

#######################
# Research Model 3

# Try K-fold CV as a sampling method and re-run model
model_caret_research_paper_500 <- train(x = x_train_df_research,
                                        y = y_train_research,
                                        method = "rf",
                                        ntree = 500, 
                                        trControl = trainControl(method = "cv", number = 2))


model_caret_research_paper_500
## Results: 
##   2  0.7094643  0.6679592
##  48  0.7646429  0.7310204
## 1185  0.7471429  0.7110204

# make predication on the test set
test_predications_research_paper_500 <- predict(model_caret_research_paper_500 , x_test_df_research)
research_paper_metrics_500 <- confusionMatrix(test_predications_research_paper_500 , y_test_research)
## Accuracy 77.33%

## Get other vales seen in paper:
PREC <- research_paper_metrics_500$byClass[,'Pos Pred Value']
mean_PREC <- mean(PREC)
REC <- research_paper_metrics_500$byClass[,'Sensitivity']
mean_REC <- mean(REC)
SPEC <- research_paper_metrics_500$byClass[,'Specificity']
mean_SPEC <- mean(SPEC)
ACC <- research_paper_metrics_500$overall['Accuracy']
F1 <- research_paper_metrics_500$byClass[,'F1']
mean_F1 <- mean(F1)
# FPS <- research_paper_metrics$byClass['False Positive Rate']

cat("PREC:", round(mean_PREC,3), "\n",
    "REC:", round(mean_REC,3), "\n",
    "SPEC:", round(mean_SPEC,3), "\n",
    "ACC:", round(ACC,3), "\n",
    "F1:", round(mean_F1,3), "\n"
)
## Results:
## PREC: 0.772 
## REC: 0.773 
## SPEC: 0.968 
## ACC: 0.773 
## F1: 0.772


#######################
#######################
# Model 1: My first: 70:15:15 split

# Split data into Train, Val and Test
# Use strafied sampling to make sure each has the rigth amount of classes.

# Ensure that the target variable is a factor
data_df$V1 <- factor(data_df$V1, levels=1:8)

# set seed to student ID 
set.seed(790) # Last three digits of student ID

# create indices for strafied sampling
indices <- createDataPartition(data_df$V1, p=0.7, list =FALSE)

# Create train, val and test databases
train_data <- data_df[indices,]
temp_data <- data_df[-indices,]
## Count the right amount of Train and val/test
dim(train_data) # 5600 1186
dim(temp_data) # 2400 1186

# Split the temp data into validation
val_test_indices <- createDataPartition(temp_data$V1, p=0.5, list=FALSE)
val_data <- temp_data[val_test_indices ,]
test_data <- temp_data[-val_test_indices ,]


#######################
# Split the data into features and target variable

x_train <- train_data[,-1]
y_train <- train_data[,1]

x_val <- val_data[,-1]
y_val <- val_data[,1]

x_test <- test_data[,-1]
y_test <- test_data[,1]

# Caret needs column names to work
x_train_df <-as.data.frame(x_train)
x_val_df <-as.data.frame(x_val)
x_test_df <-as.data.frame(x_test)

colnames(x_train_df) <- paste("Var",1:ncol(x_train_df), sep = "")
colnames(x_val_df) <- paste("Var",1:ncol(x_val_df), sep = "")
colnames(x_test_df) <- paste("Var",1:ncol(x_test_df), sep = "")

#######################
# 5) Try to improve your results; comment on your results and how they 
# compare with the paper and the deep learning model.
#######################
# RF model 0
#######################
# sample dataset so quick model
small_train_size <- round(0.1 * nrow(x_train_df)) # Use only 10% of the training data
small_train_indices <- sample(1:nrow(x_train_df), small_train_size)
small_x_train_df <- x_train_df[small_train_indices,]
small_y_train <- y_train[small_train_indices]

sample_rf_model_caret <- train(x = small_x_train_df,
                               y = small_y_train,
                               method = "rf",
                               ntree = 1)

sample_rf_model_caret
## Failed first time with error about missing values. I checked and none.
## Ran a second time and it worked. Now. Weird.


#######################
# indicated null values
sum(is.na(small_train_size)) ## 0
sum(is.na(small_train_indices)) ## 0
sum(is.na(small_x_train_df)) ## 0
sum(is.na(small_y_train)) ## 0
## No. There is no na vales

# check length of variables
length(unique(small_train_size)) ## 1
length(unique(small_train_indices)) ## 560
length(unique(small_x_train_df)) ## 1185
length(unique(small_y_train)) ## 8

# Try K-fold CV as a sampleing method and re-run model
quick_rf_model_caret <- train(x = small_x_train_df,
                              y = small_y_train,
                              method = "rf",
                              ntree = 1, # Use only 1 tree
                              trControl = trainControl(method = "cv", number = 5))
quick_rf_model_caret ## Sucuss
## Why? 


#######################
# test main data for NA values
# indicated null values
sum(is.na(train_size)) ## 0
sum(is.na(train_indices)) ## 0
sum(is.na(x_train_df)) ## 0
sum(is.na(y_train)) ## 0
## No. There is no na vales

# check length of variables
length(unique(train_size)) ## 1
length(unique(train_indices)) ## 5600
length(unique(x_train_df)) ## 1185
length(unique(y_train)) ## 8
## looks ok. Just full size, ten times bigger than sample

#######################
# RF Model 0: Testing with 1 ntree and CV 5
# See what happens while models train
options(randomForest.verbose=TRUE)

# test the same model from the sample with the full data
full_data_test_rf_model_caret <- train(x = x_train_df,
                                       y = y_train,
                                       method = "rf",
                                       ntree = 1, # Use only 1 tree
                                       trControl = trainControl(method = "cv", number = 5))
## Results:
full_data_test_rf_model_caret
## Working Now
## Accuracy = 59%


#######################
# My Model 1: default ntree:10 and CV=10
full_data_test_rf_model_caret <- train(x = x_train_df,
                                       y = y_train,
                                       method = "rf",
                                       trControl = trainControl(method = "cv", number = 10),
                                       ntree = 10)

full_data_test_rf_model_caret 
## mtry  Accuracy   Kappa    
## 2     0.6139286  0.5587755
## 48    0.7132143  0.6722449
## 1185  0.7217857  0.6820408

# make predication on the validation set
val_predictions <- predict(full_data_test_rf_model_caret, x_val_df)
# evaluate the model performance
print(confusionMatrix(val_predictions, y_val))
## Results: Accuracy = 72.17%

# make predication on the test set
test_predications <- predict(full_data_test_rf_model_caret , x_test_df)
confusionMatrix(test_predications , y_test)
## Results:
## Accuracy = 73.33%

test_predications_factor <- factor(test_predications, levels = levels(y_test))
conf_mat <- confusionMatrix(test_predications_factor, y_test)

## Get other vales seen in paper:
PREC <- conf_mat$byClass[,'Pos Pred Value']
mean_PREC <- mean(PREC)
REC <- conf_mat$byClass[,'Sensitivity']
mean_REC <- mean(REC)
SPEC <- conf_mat$byClass[,'Specificity']
mean_SPEC <- mean(SPEC)
ACC <- conf_mat$overall['Accuracy']
F1 <- conf_mat$byClass[,'F1']
mean_F1 <- mean(F1)
# FPS <- research_paper_metrics$byClass['False Positive Rate']

cat("PREC:", round(mean_PREC,3), "\n",
    "REC:", round(mean_REC,3), "\n",
    "SPEC:", round(mean_SPEC,3), "\n",
    "ACC:", round(ACC,3), "\n",
    "F1:", round(mean_F1,3), "\n"
)

## PREC: 0.736 
## REC: 0.738 
## SPEC: 0.963 
## ACC: 0.738 
## F1: 0.737 


#######################
# RF model 2: ntree= 100, CV=10
full_data_test_rf_model_caret_2 <- train(x = x_train_df,
                                       y = y_train,
                                       method = "rf",
                                       trControl = trainControl(method = "cv", number = 10),
                                       ntree = 100)


full_data_test_rf_model_caret_2 
## mtry  RMSE      Rsquared   MAE      
##  2  0.7042857  0.6620408
##  48  0.7730357  0.7406122
#  1185  0.7666071  0.7332653

# make predication on the validation set
val_predictions_2 <- predict(full_data_test_rf_model_caret_2 , x_val_df)
val_predictions_2 <- factor(val_predictions_2, levels = levels(y_val))
# evaluate the model performance
print(confusionMatrix(val_predictions_2, y_val))
## Results: Accuracy = 76.25%

# make predication on the test set
test_predications_2 <- predict(full_data_test_rf_model_caret_2  , x_test_df)
confusionMatrix(test_predications_2 , y_test)
## Results: Accuracy = 77.92%

conf_mat_2 <- confusionMatrix(test_predications_2, y_test)

## Get other vales seen in paper:
PREC <- conf_mat_2$byClass[,'Pos Pred Value']
mean_PREC <- mean(PREC)
REC <- conf_mat_2$byClass[,'Sensitivity']
mean_REC <- mean(REC)
SPEC <- conf_mat_2$byClass[,'Specificity']
mean_SPEC <- mean(SPEC)
ACC <- conf_mat_2$overall['Accuracy']
F1 <- conf_mat_2$byClass[,'F1']
mean_F1 <- mean(F1)
# FPS <- research_paper_metrics$byClass['False Positive Rate']

cat("PREC:", round(mean_PREC,3), "\n",
    "REC:", round(mean_REC,3), "\n",
    "SPEC:", round(mean_SPEC,3), "\n",
    "ACC:", round(ACC,3), "\n",
    "F1:", round(mean_F1,3), "\n"
)

## PREC: 0.774 
## REC: 0.775 
## SPEC: 0.968 
## ACC: 0.775 
## F1: 0.773 

#######################
# RF_model_3: ntree= 250, CV=10
full_data_test_rf_model_caret_3 <- train(x = x_train_df,
                                         y = y_train,
                                         method = "rf",
                                         trControl = trainControl(method = "cv", number = 10),
                                         ntree = 250)


full_data_test_rf_model_caret_3 
## mtry  Accuracy   Kappa    
##  2   0.7258929  0.6867347
## 48   0.7732143  0.7408163
##1185  0.7769643  0.7451020

# make predications on the validation set
val_predictions_3 <- predict(full_data_test_rf_model_caret_3   , x_val_df)
# evaluate the model performance
print(confusionMatrix(val_predictions_3, y_val))
## Results: Accuracy = 76.83%

# make predications on the test set
test_predications_3 <- predict(full_data_test_rf_model_caret_3   , x_test_df)
confusionMatrix(test_predications_3 , y_test)
## Results: Accuracy = 79.33%

conf_mat_3 <- confusionMatrix(test_predications_3, y_test)

## Get other vales seen in paper:
PREC <- conf_mat_3$byClass[,'Pos Pred Value']
mean_PREC <- mean(PREC)
REC <- conf_mat_3$byClass[,'Sensitivity']
mean_REC <- mean(REC)
SPEC <- conf_mat_3$byClass[,'Specificity']
mean_SPEC <- mean(SPEC)
ACC <- conf_mat_3$overall['Accuracy']
F1 <- conf_mat_3$byClass[,'F1']
mean_F1 <- mean(F1)
# FPS <- research_paper_metrics$byClass['False Positive Rate']

cat("PREC:", round(mean_PREC,3), "\n",
    "REC:", round(mean_REC,3), "\n",
    "SPEC:", round(mean_SPEC,3), "\n",
    "ACC:", round(ACC,3), "\n",
    "F1:", round(mean_F1,3), "\n"
)


## PREC: 0.791 
## REC: 0.793 
## SPEC: 0.97 
## ACC: 0.793 
## F1: 0.792



#######################
# RF_model  4: ntree= 500, CV=10
full_data_test_rf_model_caret_4 <- train(x = x_train_df,
                                         y = y_train,
                                         method = "rf",
                                         trControl = trainControl(method = "cv", number = 10),
                                         ntree = 500)





full_data_test_rf_model_caret_4 
##  mtry  Accuracy   Kappa
##    2   0.7217857  0.6820408
##   48   0.7776786  0.7459184
##  1185  0.7703571  0.7375510



## make predications on the validation set
val_predictions_4 <- predict(full_data_test_rf_model_caret_4   , x_val_df)
## evaluate the model performance
print(confusionMatrix(val_predictions_4, y_val))
## Accuracy = 76.75%


## make predications on the test set
test_predications_4 <- predict(full_data_test_rf_model_caret_4   , x_test_df)
confusionMatrix(test_predications_4 , y_test)
## Accuracy = 78.58%

summarystats_4 <- confusionMatrix(test_predications_4 , y_test)

## Get other vales seen in paper:
PREC <- summarystats_4$byClass[,'Pos Pred Value']
mean_PREC <- mean(PREC)
REC <- summarystats_4$byClass[,'Sensitivity']
mean_REC <- mean(REC)
SPEC <- summarystats_4$byClass[,'Specificity']
mean_SPEC <- mean(SPEC)
ACC <- summarystats_4$overall['Accuracy']
F1 <- summarystats_4$byClass[,'F1']
mean_F1 <- mean(F1)
# FPS <- research_paper_metrics$byClass['False Positive Rate']

cat("PREC:", round(mean_PREC,3), "\n",
    "REC:", round(mean_REC,3), "\n",
    "SPEC:", round(mean_SPEC,3), "\n",
    "ACC:", round(ACC,3), "\n",
    "F1:", round(mean_F1,3), "\n"
)

## PREC: 0.785 
## REC: 0.786 
## SPEC: 0.969 
## ACC: 0.786 
## F1: 0.784

#######################
# Model 5 : Using Grid search

# set up cross validation
train_control = trainControl(method="cv", number=10)
# set up RF turning grid
tuning_grid <- expand.grid(.mtry=seq(2,10,2))

# Train the model
rf_model_2 <- train(x = x_train_df,
                    y = y_train,
                    method="rf",
                    trControl = train_control,
                    tuneGrid = tuning_grid,
                    metrix="Accuracy",
                    ntree=250)


rf_model_2
## mtry  RMSE      Rsquared   MAE      
##  2    1.321790  0.7300559  1.1200392
##  4    1.193557  0.7563703  0.9852783
##  6    1.139292  0.7716493  0.9318609
##  8    1.114164  0.7784177  0.9027631
##  10   1.092928  0.7850039  0.8810759

#Select the best mode
best_rf_model_rf <- rf_model_2$finalModel
best_rf_model
## Number of trees: 250
## No. of variables tried at each split: 10
## Mean of squared residuals: 1.195771
## Accuracy 78.5%

# make predications on the validation set
val_predictions_rf_2 <- predict(best_rf_model_rf, x_val_df)
# evaluate the model performance
print(confusionMatrix(val_predictions_rf_2, y_val))
## Results: Accuracy = 75.58%

##Reference
##Prediction   1   2   3   4   5   6   7   8
##         1  96  50   0   1   0   0   0   1
##         2  50 100   0   0   0   0   0   0
##         3   0   0  94   0   2  29   0   0
##         4   0   0   0 137   0   0  16  20
##         5   0   0   0   0 148   0   6   3
##         6   0   0  56   0   0 121   0   0
##         7   4   0   0  10   0   0 101  16
##         8   0   0   0   2   0   0  27 110

## Model classifies class 1 96 times correctly. It mis-classifies class 2-50 times, class 2-1 time, class 8-1 time
## Model classifies class 2 100 times correctly. It mis-classifies class 2-50 times
## Model classifies class 3 94 times correctly. IT mis-classifies class 5-2 times, class 6-29 times.
## Model classifies class 4 137 times correctly. It mis-classifies class 7-16 times, class 8-20 times
## Model classifies class 5 148 times correctly. It mis-classifies class 7-6 times, class 8-3 times.
## Model classifies class 6 121 times correctly. It mis-classifies class 3-56 times
## Model classifies class 7 101 times correctly. It mis-classifies class 1-4 times, class 4-10 times, class 8-16 times
## Model classifies class 8 110 times correctly. It mis-classifies class 4-2 times, class 7 -27 times

# make predications on the test set
test_predications_rf_2 <- predict(best_rf_model_rf, x_test_df)
summarystats_2 <- confusionMatrix(test_predications_rf_2 , y_test)
## Results: Accuracy = 76.25%


## Get other vales seen in paper:
PREC <- summarystats_2$byClass[,'Pos Pred Value']
mean_PREC <- mean(PREC)
REC <- summarystats_2$byClass[,'Sensitivity']
mean_REC <- mean(REC)
SPEC <- summarystats_2$byClass[,'Specificity']
mean_SPEC <- mean(SPEC)
ACC <- summarystats_2$overall['Accuracy']
F1 <- summarystats_2$byClass[,'F1']
mean_F1 <- mean(F1)
# FPS <- research_paper_metrics$byClass['False Positive Rate']

cat("PREC:", round(mean_PREC,3), "\n",
    "REC:", round(mean_REC,3), "\n",
    "SPEC:", round(mean_SPEC,3), "\n",
    "ACC:", round(ACC,3), "\n",
    "F1:", round(mean_F1,3), "\n"
)

## PREC: 0.761 
## REC: 0.762 
## SPEC: 0.966 
## ACC: 0.762 
## F1: 0.759 









