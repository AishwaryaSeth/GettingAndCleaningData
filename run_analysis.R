#This file is used to clean the fitness data collected. 
#Further description of operation is in the README file, and the information about
#variables, along with their description is given in the CodeBook. 

# download zip file containing data
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"
download.file(zipUrl, zipFile, mode = "wb")
dataPath <- "UCI HAR Dataset"

if (!file.exists(dataPath)) {
       unzip(zipFile)}
    
# Read training data
    # read training data
    trainSubj <- read.table(file.path(dataPath, "train", "subject_train.txt"))
    trainVal <- read.table(file.path(dataPath, "train", "X_train.txt"))
    trainAct <- read.table(file.path(dataPath, "train", "y_train.txt"))
    
    # read test data
    testSubj <- read.table(file.path(dataPath, "test", "subject_test.txt"))
    testVal <- read.table(file.path(dataPath, "test", "X_test.txt"))
    testAct <- read.table(file.path(dataPath, "test", "y_test.txt"))
    
    # read features, don't convert text labels to factors
    ft <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
    
    # read activity labels
    act<- read.table(file.path(dataPath, "activity_labels.txt"))
    colnames(act) <- c("activityId", "activityLabel")
    
    #Merging Data Sets
    mergeData<- rbind(
        cbind(trainSubj, trainVal, trainAct),
        cbind(testSubj, testVal, testAct)
    )
    
    # assign column names
    colnames(mergeData) <- c("subject", ft[, 2], "activity")
    ###NOTE: Values in ft[,2 ]are not unique
    
    # determine columns of data set to keep based on column name...
    columnsToKeep <- grepl("subject|activity|mean|std", colnames(mergeData))
    
mergeData <- mergeData[, columnsToKeep]

# replace activity values with named factor levels
mergeData$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

#Clean up Column Names
# get column names
humanActivityCols <- colnames(mergeData)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(mergeData) <- humanActivityCols

# group by subject and activity and summarise using mean
DataMeans <- mergeData %>% 
    group_by(subject, activity) %>%
    summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(DataMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
