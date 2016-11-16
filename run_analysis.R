#1.Merges the training and the test sets to create one data set.
setwd('/Users/yushujing/Downloads/Data Science Specialization/R for coursera/Course 3/project/UCI HAR Dataset/')
features=read.table("features.txt") 
activity_labels=read.table("activity_labels.txt")
col_names(activity_labels)=c("activity_id","activity_type")

x_train=read.table("train/X_train.txt")   #training set. Each feature vector is a row on the text file.
y_train=read.table("train/y_train.txt")  #train activity labels(1-6)
subject_train=read.table("train/subject_train.txt")  #the subject who performed the activity (1-30 volunteers)
col_names(x_train)=features[,2]
col_names(y_train)=c("activity_id")
col_names(subject_train)=c("subject_id")
train_data=cbind(subject_train, y_train, x_train)

x_test=read.table("test/X_test.txt")
y_test=read.table("test/y_test.txt")    #test activity labels(1-6)
subject_test=read.table("test/subject_test.txt")
col_names(x_test)=features[,2]
col_names(y_test)=c("activity_id")
col_names(subject_test)=c("subject_id")
test_data=cbind(subject_test, y_test, x_test)

all_data=rbind(train_data, test_data)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
# mean(), std()
sub=all_data[grep("mean|std",col_names(all_data))]
subject_id=all_data$subject_id
activity=all_data$activity_id
sub_data=cbind(subject_id, activity, sub)


#3.Uses descriptive activity names to name the activities in the data set
sub_data$activity=activity_labels[sub_data$activity, 2]

#4.Appropriately labels the data set with descriptive variable names.
col_names=colnames(sub_data)
for (i in 1:length(col_names)) 
{
    col_names[i] = gsub("\\()","",col_names[i])
    col_names[i] = gsub("-std$","StdDev",col_names[i])
    col_names[i] = gsub("-mean","Mean",col_names[i])
    col_names[i] = gsub("^(t)","time",col_names[i])
    col_names[i] = gsub("^(f)","freq",col_names[i])
    col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i])
    col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
    col_names[i] = gsub("[Gg]yro","Gyro",col_names[i])
    col_names[i] = gsub("AccMag","AccMagnitude",col_names[i])
    col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
    col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i])
    col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i])
};
colnames(sub_data)=col_names

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(plyr)
average=aggregate(. ~subject_id + activity, sub_data, mean)
write.table(average, file = "tidy_data_set.txt",row.name=FALSE)






