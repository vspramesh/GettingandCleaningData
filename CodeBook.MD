# Getting and Cleaning Data Code Book

# Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured
3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been 
video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 
70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in 
fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which
has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration
and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz 
cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and 
frequency domain.

# Data Dictionary

Section 1

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.


-  x_data : stores x_train ,x_test text data

-  y_data : stores y_train ,y_test text data

-  subject_data : stores subject_train ,subject_test text data

Section 2

-  features : stores features.txt data

-  x_data : Extracts only the measurements on the mean and standard deviation for each measurement

Section 3

-  activity : stores activity_labels.txt

Section 4

Use gsub function for pattern replacement to clean up the data labels.

Section 5

Per the project instructions, we need to produce only a data set with the average of each variable for each activity and subject

-  averages_data : stores the average of each variable for each activity

-  tidydata.txt : output the tidy dataset 
 