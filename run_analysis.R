#Merges the training and the test sets to create one data set.
library(dplyr)

datasubject<-rbind(read.table("~/R/train/subject_train.txt"),read.table("~/R/test/subject_test.txt"))
datasubject<-tbl_df(datasubject)

dataactivity<-rbind(read.table("~/R/train/y_train.txt"),read.table("~/R/test/y_test.txt"))
dataactivity<-tbl_df(dataactivity)

dataset<-rbind(read.table("~/R/train/X_train.txt"),read.table("~/R/test/X_test.txt"))
dataset<-tbl_df(dataset)

dataset1<-cbind(datasubject,dataactivity,dataset)

feature<-read.table("~/R/train/features.txt")
feature<-as.character(feature[ ,2])
feature<-c("subject","activity",feature)
colnames(dataset1)<-feature

#Extracts only the measurements on the mean and standard deviation for each measurement.

dataset2<-dataset1%>%select(subject, activity, contains("mean"),contains("std"))%>%
        select(subject, activity, starts_with("tBodyAcc-"), starts_with("tGravityAcc-"), starts_with("tBodyGyro-"))
#Uses descriptive activity names to name the activities in the data set

for (i in 1:nrow(dataset2)){
        if(identical(dataset2$activity[i],"1")){
                dataset2$activity[i]<-"walking"
                
        }

        else if(identical(dataset2$activity[i], "2")){
                dataset2$activity[i]<-"walking_upstairs"
                
        }
        else if (identical(dataset2$activity[i], "3")){
                dataset2$activity[i]<-"walking_downstairs"
                
        }

        else if (identical(dataset2$activity[i], "4")){
                dataset2$activity[i]<-"sitting"
                
        }
        else if (identical(dataset2$activity[i], "5")){
                dataset2$activity[i]<-"standing"
                
        }
        else {
                dataset2$activity[i]<-"laying"
                
        }

}
#Appropriately labels the data set with descriptive variable names.



colnames(dataset2)<- c("subject", "activity", "mean_body_accelero_X","mean_body_accelero_Y","mean_body_accelero_Z", "std_body_accelero_X", "std_body_accelero_Y",
                       
                       "std_body_accelero_Z",   "mean_gravity_accelero_X", "mean_gravity_accelero_Y","mean_gravity_accelero_Z", "std_gravity_accelero_X",
                       
                       "std_gravity_accelero_Y","std_gravity_accelero_Z", "mean_body_gyro_X","mean_body_gyro_Y", "mean_body_gyro_Z", "std_body_gyro_X", 
                       "std_body_gyro_Y","std_body_gyro_Z")


#From the data set in step 4, creates a second, independent tidy data set
#with the average of each variable for each activity and each subject

dataset3<-dataset2%>%group_by(subject,activity)%>%summarise(mean_body_accelero_X=mean(mean_body_accelero_X), std_body_accelero_X=mean(std_body_accelero_X),
                                                            mean_body_accelero_Y=mean(mean_body_accelero_Y),std_body_accelero_Y=mean(std_body_accelero_Y),
                                                            mean_body_accelero_Z=mean(mean_body_accelero_Z),std_body_accelero_Z=mean(std_body_accelero_Z),
                                                            mean_gravity_accelero_X=mean(mean_gravity_accelero_X),std_gravity_accelero_X=mean(std_gravity_accelero_X),
                                                            mean_gravity_accelero_Y=mean(mean_gravity_accelero_Y),std_gravity_accelero_Y=mean(std_gravity_accelero_Y),
                                                            mean_gravity_accelero_Z=mean(mean_gravity_accelero_Z),std_gravity_accelero_Z=mean(std_gravity_accelero_Z),
                                                            mean_body_gyro_X=mean(mean_body_gyro_X),std_body_gyro_X=mean(std_body_gyro_X),
                                                            mean_body_gyro_Y=mean(mean_body_gyro_Y),std_body_gyro_Y=mean(std_body_gyro_Y), 
                                                            mean_body_gyro_Z=mean(mean_body_gyro_Z), std_body_gyro_Z=mean(std_body_gyro_Z))
                
         
                                                          
write.table(dataset3,file="~/R/getdataproject.txt", row.names=FALSE)
