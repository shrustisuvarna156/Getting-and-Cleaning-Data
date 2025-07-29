library(dplyr)
run_analysis <- function()
setwd("UCI HAR Dataset")


traindata <- read.table("./train/X_train.txt")
testdata  <- read.table("./test/X_test.txt")
joindata  <- rbind(traindata, testdata) 

dim(traindata) ##  (7352, 561)
dim(testdata)  ##  (2947, 561)
dim(joindata)  ## (10299, 561)

####### read and merge label-data set pair ##############################
trainlabel <- read.table("./train/y_train.txt")
testlabel  <- read.table("./test/y_test.txt")
joinlabel  <- rbind(trainlabel, testlabel)


## cross-check dimensions: (observations/rows, variables/columns)
dim(trainlabel) ## (7352, 1)
dim(testlabel)  ## (2947, 1)
dim(joinlabel)  ##(10299, 1)


### read and merge subject-data set pair #################################
trainsubject <- read.table("./train/subject_train.txt")
testsubject  <- read.table("./test/subject_test.txt")
joinsubject  <- rbind(trainsubject, testsubject)


##  cross-check dimensions: meanStdIndex <- grep("-mean\\(\\) | -std\\(\\)", features[, 2])
dim(trainsubject) ##  (7352, 1)
dim(testsubject)  ##  (2947, 1)
dim(joinsubject)  ## (10299, 1)

## Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
## cross-check dimensions: (rows, columns)
dim(features) ## (561, 2)

## locate column names with "-mean()" or "-std()" in any rows in column 2
meanstdindex <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])

##cross-check length(meanstdindex)
length(meanstdindex) ## 66 variables

## only select those columns/measurements on the mean and standard deviation
## make a new data frame called joindatanew
joindatanew <- joindata[, meanstdindex] 

## cross-check dimensions: (rows, columns)
dim(joindatanew)  ##(10299,66)

## put the column names into the new data frame joindatanew
colnames(joindatanew) <- features[meanstdindex, 2] 

## remove the bad characters such as "()", "-" in colnames, also lower the case if possible
##   in order to avoid any unnecessary errors in later analysis
colnames(joindatanew) <- gsub("\\(|\\)", "", colnames(joindatanew)) 
colnames(joindatanew) <- gsub("-", ".", colnames(joindatanew))
colnames(joindatanew) <- tolower(colnames(joindatanew))

## Step 3: Uses descriptive activity names to name the activities in the data set.
## Firstly, it is necessary to load/read the file containing full activity names
activity <- read.table("activity_labels.txt")

## Remove bad characters such as "_" and also lower case in the activity names/row names
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))

## Create a new activitylabel vector containing descriptive activity names with
## the length of rows of joinlabel[, 1] 
activitylabel <- activity[joinlabel[, 1], 2]

## Replace the 'activity numbers' to descriptive activity names in joinlabel data frame
joinlabel[, 1] <- activitylabel 

## Give a column name to the column in the joinlabel data frame (one column data frame)
colnames(joinlabel) <- "activity"


## Step 4: Appropriately labels the data set with descriptive activity names.
##      (i.e.: create a "clean" data set with labels (colnames and rownames))
##  Firstly, give a column name to the column of the joinsubject data frame (one column data frame)
colnames(joinsubject) <- "subject"

##  Combine three working dataframes (joinsubject, joinlabel and joindatanew) into one 
##  single data frame via command cbind

cleandata <- cbind(joinsubject, joinlabel, joindatanew)

## Cross-check dimensions: (nrows, ncolumns)
dim(cleandata) ## (10299    68)


## Step 5: Creates a second, independent tidy data set with the average of each variable
##for each activity and each subject. 
library(reshape2)
## Melt the cleadata data set before decasting
meltdfrm <- melt(cleandata, id=c("activity", "subject"))

## Cast the melt dataset based on the average of each variable for each activity and each subject
tidydfrm <- dcast(meltdfrm, activity + subject ~ variable, mean)

## Create a file containing the tidy data set
write.table(tidydfrm, "tidy_average_data.txt", row.names = F, col.names= T, sep = "\t")

cat("DONE: a tidy data file has been created in the working directory!\n")