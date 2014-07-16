# Step1. Merges the training and the test data sets into one.
# setwd("~/Desktop/Getting_and_Cleaning_Data/")
trainDta <- read.table("./data/train/X_train.txt")
dim(trainDta) # 7352*561
head(trainDta)
trainLbl <- read.table("./data/train/y_train.txt")
table(trainLbl)
trainSubj <- read.table("./data/train/subject_train.txt")
testDta <- read.table("./data/test/X_test.txt")
dim(testDta) # 2947*561
testLbl <- read.table("./data/test/y_test.txt") 
table(testLbl) 
testSubj <- read.table("./data/test/subject_test.txt")
joinDta <- rbind(trainDta, testDta)
dim(joinDta) # 10299*561
joinLbl <- rbind(trainLbl, testLbl)
dim(joinLbl) # 10299*1
joinSubj <- rbind(trainSubj, testSubj)
dim(joinSubj) # 10299*1

# Step2. Parse the measurements on the mean and stdev for each measurement. 
features <- read.table("./data/features.txt")
dim(features)  # 561*2
mnStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(mnStdIndices) # 66
joinDta <- joinDta[, mnStdIndices]
dim(joinDta) # 10299*66
#strip "()" & "-" & capitalize "M" & "S"
names(joinDta) <- gsub("\\(\\)", "", features[mnStdIndices, 2])
names(joinDta) <- gsub("mean", "Mean", names(joinDta))
names(joinDta) <- gsub("std", "Std", names(joinDta))
names(joinDta) <- gsub("-", "", names(joinDta))

# Step3. Uses descriptive activity names to name the activities in the data set.
activity <- read.table("./data/activity_labels.txt")
# Formats the activity names
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLbl <- activity[joinLbl[, 1], 2]
joinLbl[, 1] <- activityLbl
names(joinLbl) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubj) <- "subject"
tidyDta <- cbind(joinSubj, joinLbl, joinDta)
dim(tidyDta) # 10299*68
write.table(tidyDta, "tidy_data.txt") # creates the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjLen <- length(table(joinSubj)) # 30
activityLen <- dim(activity)[1] # 6
colLen <- dim(tidyDta)[2]
result <- matrix(NA, nrow=subjLen*activityLen, ncol=colLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(tidyDta)
row <- 1
for(i in 1:subjLen) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(joinSubj)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        boolean1 <- i == tidyDta$subject
        boolean2 <- activity[j, 2] == tidyDta$activity
        result[row, 3:colLen] <- colMeans(tidyDta[boolean1&boolean2, 3:colLen])
        row <- row + 1
    }
}
head(result)
write.table(result, "data_with_means.txt") # creates the 2nd dataset

# data <- read.table("./data_with_means.txt")
# data[1:12, 1:5]