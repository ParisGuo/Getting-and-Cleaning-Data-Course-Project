fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "getdata_dataset.zip"

if(!file.exists(zipfile)){dir.create(zipfile)}
download.file(fileUrl, zipfile, method="curl")

datafile <- "UCI HAR Dataset"
if (!file.exists("datafile")) {
  unzip(zipfile)}

#read test data
testsubject <- read.table(file.path(datafile,"test","subject_test.txt"))
testdata_x <- read.table(file.path(datafile,"test","X_test.txt"))
testdata_y <- read.table(file.path(datafile,"test","y_test.txt"))
test <- cbind(testsubject,testdata_x,testdata_y)

#read training data
trainingsubject <- read.table(file.path(datafile,"train","subject_train.txt"))
training_x <- read.table(file.path(datafile,"train","X_train.txt"))
training_y <- read.table(file.path(datafile,"train","y_train.txt"))
train <- cbind(trainingsubject,training_x,training_y)

#read features
features <- read.table(file.path(datafile, "features.txt"),as.is = TRUE)
features[,2] <- as.character(features[,2])
#read activity_labels
activity_labels <-read.table(file.path(datafile, "activity_labels.txt"))
activity_labels[,2] <- as.character(activity_labels[,2])

#Merges the training and the test sets to create one data set
alldata <- rbind(test,train)
#assign column names
colnames(alldata) <- c("subject", features[, 2], "activity")

#Extracts only the measurements on the mean and standard deviation for each measurement
column.names.filtered <- grep("std\\(\\)|mean\\(\\)|activity|subject", colnames(alldata))
alldata <- alldata[, column.names.filtered]

#Uses descriptive activity names to name the activities in the data set
alldata$activity <- factor(alldata$activity, labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

#Appropriately labels the data set with descriptive variable names
alldatacols <- colnames(alldata)
alldatacols <- gsub("[\\(\\)-]", "", alldatacols)

alldatacols <- gsub("^f", "frequencyDomain", alldatacols)
alldatacols <- gsub("^t", "timeDomain", alldatacols)
alldatacols <- gsub("Acc", "Accelerometer", alldatacols)
alldatacols <- gsub("Gyro", "Gyroscope", alldatacols)
alldatacols <- gsub("Mag", "Magnitude", alldatacols)
alldatacols <- gsub("Freq", "Frequency", alldatacols)
alldatacols <- gsub("mean", "Mean", alldatacols)
alldatacols <- gsub("std", "StandardDeviation", alldatacols)
alldatacols <- gsub("BodyBody", "Body", alldatacols)

colnames(alldata) <- alldatacols

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
alldataMeans <- alldata %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(alldataMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)