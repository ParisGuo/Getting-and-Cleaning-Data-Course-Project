Code Book for Getting-and-Cleaning-Data-Course-Project
======================================================

Data
----

This code book summarize the tidy.txt file located in the repository and
the original dataset is UCI HAR Dataset, Human Activity Recognition
Using Smartphones Dataset.

Identifiers
-----------

-   Subject: ID of the test subject, range from 1 to 30.
-   Activity: The type of activity performed when the corresponding
    measurements were taken. The activities lables include: WALKING,
    WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING, STANDING, LAYING.

Measurements
------------

The features selected for this database come from the accelerometer and
gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain
signals (prefix 't' to denote time) were captured at a constant rate of
50 Hz. Then they were filtered using a median filter and a 3rd order low
pass Butterworth filter with a corner frequency of 20 Hz to remove
noise. Similarly, the acceleration signal was then separated into body
and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ)
using another low pass Butterworth filter with a corner frequency of 0.3
Hz.

Subsequently, the body linear acceleration and angular velocity were
derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and
tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional
signals were calculated using the Euclidean norm (tBodyAccMag,
tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

Finally a Fast Fourier Transform (FFT) was applied to some of these
signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ,
fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to
indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for
each pattern:\
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

-   `tBodyAccMeanX`
-   `tBodyAccMeanY`
-   `tBodyAccMeanZ`
-   `tBodyAccStdX`
-   `tBodyAccStdY`
-   `tBodyAccStdZ`
-   `tGravityAccMeanX`
-   `tGravityAccMeanY`
-   `tGravityAccMeanZ`
-   `tGravityAccStdX`
-   `tGravityAccStdY`
-   `tGravityAccStdZ`
-   `tBodyAccJerkMeanX`
-   `tBodyAccJerkMeanY`
-   `tBodyAccJerkMeanZ`
-   `tBodyAccJerkStdX`
-   `tBodyAccJerkStdY`
-   `tBodyAccJerkStdZ`
-   `tBodyGyroMeanX`
-   `tBodyGyroMeanY`
-   `tBodyGyroMeanZ`
-   `tBodyGyroStdX`
-   `tBodyGyroStdY`
-   `tBodyGyroStdZ`
-   `tBodyGyroJerkMeanX`
-   `tBodyGyroJerkMeanY`
-   `tBodyGyroJerkMeanZ`
-   `tBodyGyroJerkStdX`
-   `tBodyGyroJerkStdY`
-   `tBodyGyroJerkStdZ`
-   `tBodyAccMagMean`
-   `tBodyAccMagStd`
-   `tGravityAccMagMean`
-   `tGravityAccMagStd`
-   `tBodyAccJerkMagMean`
-   `tBodyAccJerkMagStd`
-   `tBodyGyroMagMean`
-   `tBodyGyroMagStd`
-   `tBodyGyroJerkMagMean`
-   `tBodyGyroJerkMagStd`
-   `fBodyAccMeanX`
-   `fBodyAccMeanY`
-   `fBodyAccMeanZ`
-   `fBodyAccStdX`
-   `fBodyAccStdY`
-   `fBodyAccStdZ`
-   `fBodyAccMeanFreqX`
-   `fBodyAccMeanFreqY`
-   `fBodyAccMeanFreqZ`
-   `fBodyAccJerkMeanX`
-   `fBodyAccJerkMeanY`
-   `fBodyAccJerkMeanZ`
-   `fBodyAccJerkStdX`
-   `fBodyAccJerkStdY`
-   `fBodyAccJerkStdZ`
-   `fBodyAccJerkMeanFreqX`
-   `fBodyAccJerkMeanFreqY`
-   `fBodyAccJerkMeanFreqZ`
-   `fBodyGyroMeanX`
-   `fBodyGyroMeanY`
-   `fBodyGyroMeanZ`
-   `fBodyGyroStdX`
-   `fBodyGyroStdY`
-   `fBodyGyroStdZ`
-   `fBodyGyroMeanFreqX`
-   `fBodyGyroMeanFreqY`
-   `fBodyGyroMeanFreqZ`
-   `fBodyAccMagMean`
-   `fBodyAccMagStd`
-   `fBodyAccMagMeanFreq`
-   `fBodyBodyAccJerkMagMean`
-   `fBodyBodyAccJerkMagStd`
-   `fBodyBodyAccJerkMagMeanFreq`
-   `fBodyBodyGyroMagMean`
-   `fBodyBodyGyroMagStd`
-   `fBodyBodyGyroMagMeanFreq`
-   `fBodyBodyGyroJerkMagMean`
-   `fBodyBodyGyroJerkMagStd`
-   `fBodyBodyGyroJerkMagMeanFreq`

Data Transformation
-------------------

Here are the data for the project: -
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

-   Created one R script called run\_analysis.R that does the following.

1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation
    for each measurement.
3.  Uses descriptive activity names to name the activities in the data
    set
4.  Appropriately labels the data set with descriptive variable names.
5.  From the data set in step 4, creates a second, independent tidy data
    set with the average of each variable for each activity and each
    subject. © 2021 GitHub, Inc. Terms Privacy Security Status Docs
    Contact GitHub Pricing API Training Blog About

