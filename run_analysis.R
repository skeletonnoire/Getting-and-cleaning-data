## Merges the training and the test sets to create one data set
# Load all the test and train data into R
data_test<-read.table("./Desktop/UCI HAR Dataset/test/X_test.txt", sep="", header = FALSE)
label_test<-read.table("./Desktop/UCI HAR Dataset/test/Y_test.txt", sep="", header = FALSE)
subject_test<-read.table("./Desktop/UCI HAR Dataset/test/subject_test.txt", sep="", header = FALSE)
 
data_train<-read.table("./Desktop/UCI HAR Dataset/train/X_train.txt", sep="", header = FALSE)
label_train<-read.table("./Desktop/UCI HAR Dataset/train/Y_train.txt", sep="", header = FALSE)
subject_train<-read.table("./Desktop/UCI HAR Dataset/train/subject_train.txt", sep="", header = FALSE)

# Combine test and train data sets 
data<-rbind(data_train, data_test)
data_activity<-rbind(label_train, label_test)
data_subject<-rbind(subject_train, subject_test)


# Give the variables in each of the data sets column names, but first we need to get the 'features' file
features<-read.table("./Desktop/UCI HAR Dataset/features.txt", header=FALSE, stringsAsFactors=FALSE) 
names(data_activity)<-c("activity")
names(data_subject)<-c("subject")
names(data)<-features$V2
 
data_subjectandactivity<-cbind(data_subject, data_activity)
data_all<-cbind(data, data_subjectandactivity)

## Extracts only the measurements on the mean and standard deviation for each measurement 
data.feat.mean.std<-features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
featNames<-c(as.character(data.feat.mean.std), "subject", "activity")
data.mean.std<-subset(data_all,select=featNames)

## Uses descriptive activity names to name the activities in the data set
activity_labels<-read.table("./Desktop/UCI HAR Dataset/activity_labels.txt")   
names(activity_labels)<-c("activity", "ActivityType")
require(plyr)
data.mean.std.wlabel<-join(data.mean.std, activity_labels, by="activity")

## Appropriately label the data set with descriptive names. 
names(data.mean.std.wlabel) <- gsub('\\(|\\)',"", names(data.mean.std.wlabel), perl = TRUE)
names(data.mean.std.wlabel) <- make.names(names(data.mean.std.wlabel))

names(data.mean.std.wlabel) <- gsub("\\.mean",".Mean", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("\\.std", ".Std.", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("Freq\\.","Frequency.", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("Freq$","Frequency", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("Gyro", "Gyroscope", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("tGravity", "Time.Gravity", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("tBody", "Time.Body", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("fGravity", "FFT.Gravity", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("^t", "time", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("Acc", "Accelerometer", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("Mag", "Magnitude", names(data.mean.std.wlabel))
names(data.mean.std.wlabel) <- gsub("BodyBody", "Body", names(data.mean.std.wlabel))

##Creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidydata<-aggregate(. ~subject + activity, data.mean.std.wlabel, mean)
tidydata<-tidydata[order(tidydata$subject, tidydata$activity),]
write.table(tidydata, file = "./Desktop/UCI HAR Dataset/data_mean.std.avgbysubject.txt")
