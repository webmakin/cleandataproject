library(dplyr)
features <- read.table("features.txt", col.names = c("n", "functions"))
activities <- read.table("activity_labels.txt", col.names = c("code", "activity"))
subject_test_data <- read.table("test/subject_test.txt", col.names = "subject")
x_test_data <- read.table("test/X_test.txt", col.names = features$functions)
y_test_data <- read.table("test/y_test.txt", col.names = "code")
subject_train_data <- read.table("train/subject_train.txt", col.names = "subject")
x_train_data <- read.table("train/X_train.txt", col.names = features$functions)
y_train_data <- read.table("train/y_train.txt", col.names = "code")

## Merges the training and the test sets to create one data set.
X <- rbind(x_train_data, x_test_data)
Y <- rbind(y_train_data, y_test_data)
Subject <- rbind(subject_train_data, subject_test_data)
Merged<- cbind(Subject, Y, X)

## Extracts only the measurements on the mean and standard deviation for each measurement. 
Cleaned <- Merged %>% select(subject, code, contains("mean"), contains("std"))

## Uses descriptive activity names to name the activities in the data set
Cleaned$code <- activities[Cleaned$code, 2]

##Appropriately labels the data set with descriptive variable names. 
names(Cleaned)[2] = "activity"
names(Cleaned)<-gsub("Acc", "Accelerometer", names(Cleaned))
names(Cleaned)<-gsub("Gyro", "Gyroscope", names(Cleaned))
names(Cleaned)<-gsub("BodyBody", "Body", names(Cleaned))
names(Cleaned)<-gsub("Mag", "Magnitude", names(Cleaned))
names(Cleaned)<-gsub("^t", "Time", names(Cleaned))
names(Cleaned)<-gsub("^f", "Frequency", names(Cleaned))
names(Cleaned)<-gsub("tBody", "TimeBody", names(Cleaned))
names(Cleaned)<-gsub("-mean()", "Mean", names(Cleaned), ignore.case = TRUE)
names(Cleaned)<-gsub("-std()", "STD", names(Cleaned), ignore.case = TRUE)
names(Cleaned)<-gsub(".std.", "STD", names(Cleaned), ignore.case = TRUE)
names(Cleaned)<-gsub("-freq()", "Frequency", names(Cleaned), ignore.case = TRUE)
names(Cleaned)<-gsub("Freq", "Frequency", names(Cleaned), ignore.case = TRUE)
names(Cleaned)<-gsub("angle", "Angle", names(Cleaned))
names(Cleaned)<-gsub("gravity", "Gravity", names(Cleaned))

## From the data set in step 4, creates a second, independent tidy data set with 
## the average of each variable for each activity and each subject.
Result <- Cleaned %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
View(Result)
