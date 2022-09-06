#Importing the dataset
raw_dataset <- read.csv("dataset.csv")

#Duplicating the dataset
working_rawset <- raw_dataset

#Importing essential libraries
library(rpart)
library(rpart.plot)
library(caret)

# Drop ID and zip code columns
working_rawset <- working_rawset[ , -c(1, 5)]

#Drop NA values
na.omit(working_rawset)

#Viewing the dataset
View(working_rawset)

#Split the data into training and test data
set.seed(1)
train_index <- sample(c(1:dim(working_rawset)[1]), dim(working_rawset)[1]*0.8)  #randomly selecting 80% of dataset
train_data <- working_rawset[train_index, ]
test_data <- working_rawset[-train_index, ]

#Checking proportion of train data and test data to verify if the randomization process is correct
prop.table(table(train_data$Personal.Loan))
prop.table(table(test_data$Personal.Loan))

#Construct a decision tree model
decision_tree <- rpart(Personal.Loan ~ ., method = "class",
                       data = train_data,
                       control = rpart.control(minsplit = 1),
                       parms = list(split = "information"))

#Review the decision tree
summary(decision_tree)

#Plot the decision tree
prp(decision_tree, type = 1, extra = 1, split.font = 2, varlen = 0, tweak = 1.1, box.palette = "GnBu" )

#Decision tree model evaluation/ Making a prediction
rpart_pred_c <- predict(decision_tree, test_data, type = "class") 

#Measuring performance
resultant_matrix_c <- confusionMatrix(table(rpart_pred_c, test_data$Personal.Loan))
resultant_matrix_c
