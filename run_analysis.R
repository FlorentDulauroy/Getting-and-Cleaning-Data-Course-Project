xtest <- read.csv("test/X_test.txt", header=FALSE, stringsAsFactors = FALSE)
#Remove occurance of souble space
xtest[,1] = gsub("  ", " ", xtest[,1])
#Remove Leading space
xtest[,1] <- substring(xtest[,1], 2)
#Split by spaces
newtest<-data.frame(do.call('rbind', strsplit(as.character(xtest[,1]), ' ')))

xtrain <- read.csv("train/X_train.txt", header=FALSE, stringsAsFactors = FALSE)
#Remove occurance of souble space
xtrain[,1] = gsub("  ", " ", xtrain[,1])
#Remove Leading space
xtrain[,1] <- substring(xtrain[,1], 2)
#Split by spaces
newtrain<-data.frame(do.call('rbind', strsplit(as.character(xtrain[,1]), ' ')))

features <- read.delim("features.txt", header=FALSE, sep = " ")
colnames(newtest) <- features$V2
colnames(newtrain) <- features$V2

subjecttrain <- read.delim("train/subject_train.txt", header=FALSE, sep = " ")
newtrain <- cbind(subjecttrain, newtrain)
colnames(newtrain)[colnames(newtrain)=="V1"] <- "Subject"

subjecttest <- read.delim("test/subject_test.txt", header=FALSE, sep = " ")
newtest <- cbind(subjecttest, newtest)
colnames(newtest)[colnames(newtest)=="V1"] <- "Subject"

ytest <- read.csv("test/y_test.txt", header=FALSE)
newtest <- cbind(ytest, newtest)
colnames(newtest)[colnames(newtest)=="V1"] <- "Activity"

ytrain <- read.csv("train/y_train.txt", header=FALSE)
newtrain <- cbind(ytrain, newtrain)
colnames(newtrain)[colnames(newtrain)=="V1"] <- "Activity"

Fulldf <- rbind(newtest, newtrain)

Fulldf$Activity = gsub("1", "WALKING", Fulldf$Activity)
Fulldf$Activity = gsub("2", "WALKING_UPSTAIRS", Fulldf$Activity)
Fulldf$Activity = gsub("3", "WALKING_DOWNSTAIRS", Fulldf$Activity)
Fulldf$Activity = gsub("4", "SITTING", Fulldf$Activity)
Fulldf$Activity = gsub("5", "STANDING", Fulldf$Activity)
Fulldf$Activity = gsub("6", "LAYING", Fulldf$Activity)

MeanStd <- Fulldf[,c("Subject", "Activity","tBodyAcc-mean()-X",
          "tBodyAcc-mean()-Y",
          "tBodyAcc-mean()-Z",
          "tBodyAcc-std()-X",
          "tBodyAcc-std()-Y",
          "tBodyAcc-std()-Z",
          "tGravityAcc-mean()-X",
          "tGravityAcc-mean()-Y",
          "tGravityAcc-mean()-Z",
          "tGravityAcc-std()-X",
          "tGravityAcc-std()-Y",
          "tGravityAcc-std()-Z",
          "tBodyAccJerk-mean()-X",
          "tBodyAccJerk-mean()-Y",
          "tBodyAccJerk-mean()-Z",
          "tBodyAccJerk-std()-X",
          "tBodyAccJerk-std()-Y",
          "tBodyAccJerk-std()-Z",
          "tBodyGyro-mean()-X",
          "tBodyGyro-mean()-Y",
          "tBodyGyro-mean()-Z",
          "tBodyGyro-std()-X",
          "tBodyGyro-std()-Y",
          "tBodyGyro-std()-Z",
          "tBodyGyroJerk-mean()-X",
          "tBodyGyroJerk-mean()-Y",
          "tBodyGyroJerk-mean()-Z",
          "tBodyGyroJerk-std()-X",
          "tBodyGyroJerk-std()-Y",
          "tBodyGyroJerk-std()-Z",
          "tBodyAccMag-mean()",
          "tBodyAccMag-std()",
          "tGravityAccMag-mean()",
          "tGravityAccMag-std()",
          "tBodyAccJerkMag-mean()",
          "tBodyAccJerkMag-std()",
          "tBodyGyroMag-mean()",
          "tBodyGyroMag-std()",
          "tBodyGyroJerkMag-mean()",
          "tBodyGyroJerkMag-std()",
          "fBodyAcc-mean()-X",
          "fBodyAcc-mean()-Y",
          "fBodyAcc-mean()-Z",
          "fBodyAcc-std()-X",
          "fBodyAcc-std()-Y",
          "fBodyAcc-std()-Z",
          "fBodyAccJerk-mean()-X",
          "fBodyAccJerk-mean()-Y",
          "fBodyAccJerk-mean()-Z",
          "fBodyAccJerk-std()-X",
          "fBodyAccJerk-std()-Y",
          "fBodyAccJerk-std()-Z",
          "fBodyGyro-mean()-X",
          "fBodyGyro-mean()-Y",
          "fBodyGyro-mean()-Z",
          "fBodyGyro-std()-X",
          "fBodyGyro-std()-Y",
          "fBodyGyro-std()-Z",
          "fBodyAccMag-mean()",
          "fBodyAccMag-std()",
          "fBodyBodyAccJerkMag-mean()",
          "fBodyBodyAccJerkMag-std()",
          "fBodyBodyGyroMag-mean()",
          "fBodyBodyGyroMag-std()",
          "fBodyBodyGyroJerkMag-mean()",
          "fBodyBodyGyroJerkMag-std()")]

MeanStd %>% mutate_all(as.character) %>% mutate_all(as.numeric) %>% colMeans
MeanStd[, 3:68] <- sapply(MeanStd[, 3:68], as.character)
MeanStd[, 3:68] <- sapply(MeanStd[, 3:68], as.numeric)
MeanByActivitySubject <- aggregate(MeanStd[-c(1:2)],by=list(MeanStd$Subject, MeanStd$Activity),FUN=mean)
colnames(MeanByActivitySubject)[colnames(MeanByActivitySubject)=="Group.1"] <- "Subject"
colnames(MeanByActivitySubject)[MeanByActivitySubject(newtest)=="Group.2"] <- "Activity"

MeanByActivitySubject
