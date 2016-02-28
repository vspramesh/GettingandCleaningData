library(plyr)
# step1
# create a folder Project and download files
# Method is libcurl not curl as I am using windows
##############################################################################

if(!file.exists("./project")){dir.create("./project")}
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url,destfile="./project/Dataset.zip",method="libcurl")

# Unzip file

unzip(zipfile="./project/Dataset.zip",exdir="./project")

path_rf <- file.path("./project" , "UCI HAR Dataset")

# Assigning files to the Variable

y_test  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
y_train <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)

subject_train <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
subject_test  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)

x_test  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
x_train <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)

# create 'x' data set
x_data <- rbind(x_train, x_test)

# create 'y' data set
y_data <- rbind(y_train, y_test)

# create 'subject' data set
subject_data <- rbind(subject_train, subject_test)

#Step2
#Extract only the measurements on the mean and standard deviation for each measurement
########################################################################

features <- read.table(file.path(path_rf, "features.txt"),head=FALSE)

mean_and_std_features <- grep("-(mean|std)\\(\\)", features[, 2])
x_data <- x_data[, mean_and_std_features]
names(x_data) <- features[mean_and_std_features, 2]


 str(x_data)

#Step3
#Uses descriptive activity names to name the activities in the data set
###########################################################################

activity <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)
y_data[, 1] <- activity[y_data[, 1], 2]
names(y_data) <- "activity"

#Step4
# Appropriately label the data set with descriptive variable names
###############################################################################

names(subject_data) <- "subject"
# bind all the data in a single data set
all_data <- cbind(x_data, y_data, subject_data)

colNames <- colnames(all_data)
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(all_data) <- colNames

#Step5
# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject
###############################################################################

averages_data <-aggregate(. ~subject + activity, all_data, mean)
averages_data <-averages_data[order(averages_data$subject,averages_data$activity),]
write.table(averages_data , file = "tidydata.txt",row.name=FALSE)








