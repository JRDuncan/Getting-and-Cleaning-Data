
## 1. Merges the training and the test sets to create one data set.

#Merge X_train and X_test into X
X1 <- read.table("train/X_train.txt")
X2 <- read.table("test/X_test.txt")
X <- rbind(X1, X2)

#Merge subject_train and subject_test into S
subject1 <- read.table("train/subject_train.txt")
subject2 <- read.table("test/subject_test.txt")
S <- rbind(subject1, subject2)

#Merge y_train and y_test into Y
Y1 <- read.table("train/y_train.txt")
Y2 <- read.table("test/y_test.txt")
Y <- rbind(Y1, Y2)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.


features <- read.table("features.txt")
mean_std_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, mean_std_features]
names(X) <- features[mean_std_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X)) 

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
tidyData <- cbind(S, Y, X)
 
# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(tidyData)[2]
result = tidyData[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
	for (a in 1:numActivities) {
		result[row, 1] = uniqueSubjects[s]
		result[row, 2] = activities[a, 2]
		tmp <- tidyData[tidyData$subject==s & tidyData$activity==activities[a, 2], ]
		result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
		row = row+1
	}
}
write.table(result, "tidyDataAverages.txt",row.name=FALSE)