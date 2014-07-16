This codebook describes the variables, the data, and any transformations or work that was performed to clean up the data.

##Data source : http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## Dataset :https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

##The run_analysis.R script performs the following steps to clean the data:

1.	Read X_train.txt, y_train.txt and subject_train.txt from the "./data/train" folder and store them in trainDta, trainLbl and trainSubj variables respectively.
2.	Read X_test.txt, y_test.txt and subject_test.txt from the "./data/test" folder and store them in testDta, testLbl and testsubj variables respectively.
3.	Concatenate testDta to trainDta to generate a 10299x561 data frame, joinDta; concatenate testLbl to trainLbl to generate a 10299x1 data frame, joinLbl; concatenate testSubj to trainSubj to generate a 10299x1 data frame, joinSubj.
4.	Read the features.txt file from the "/data" folder and store the data in a variable called features. We only extract the measurements on the mean and standard deviation. This results in a 66 indices list. We get a subset of joinDta with the 66 corresponding columns.
5.	Clean the column names of the subset. We remove the "()" and "-" symbols in the names, as well as make the first letter of "mean" and "std" a capital letter "M" and "S" respectively.
6.	Read the activity_labels.txt file from the "./data"" folder and store the data in a variable called activity.
7.	Format the activity names in the second column of activity. We first make all names to lower cases. If the name has an underscore between letters, we remove the underscore and capitalize the letter immediately after the underscore.
8.	Transform the values of joinLbl according to the activity data frame.
9.	Combine the joinSub, joinLbl and joinDta by column to get a new 10299x68 data frame, tidyDta. Properly name the first two columns, "subject" and "activity". The "subject" column contains integers that range from 1 to 30 inclusive; the "activity" column contains 6 kinds of activity names; the last 66 columns contain measurements that range from -1 to 1 exclusive.
10.	Create the tidyDta out to "tidy_data.txt" file in current working directory.
11.	To finish, generate a second independent tidy data set with the average of each measurement for each activity and each subject. We have 30 unique subjects and 6 unique activities, which result in a 180 combinations of the two. Then, for each combination, we calculate the mean of each measurement with the corresponding combination. Hence, after initializing the result data frame and performing the two for-loops, we get a 180x68 data frame.
12.	Create the result into "data_with_means.txt" file in current working directory.
