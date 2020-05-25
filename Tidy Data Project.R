#Tidy Data Project#
#Reading file#
getwd()
setwd("C:/Users/edgar/Dropbox/Especializacion Data Science/Getting Data Clean/Project")
library(dplyr)
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "./project_data.zip", method = "curl")
unzip("./project_data.zip")

list.dirs(path="./UCI HAR Dataset")

list.files(path ="./UCI HAR Dataset/test")
X_test<-read.table(file = "./UCI HAR Dataset/test/X_test.txt", header = FALSE, dec = ".", sep = "")
y_test<-read.table(file = "./UCI HAR Dataset/test/y_test.txt", header = FALSE, dec = ".", sep = "")

list.files(path ="./UCI HAR Dataset/train")
X_train<-read.table(file = "./UCI HAR Dataset/train/X_train.txt", header = FALSE, dec = ".", sep = "")
y_train<-read.table(file = "./UCI HAR Dataset/train/y_train.txt", header = FALSE, dec = ".", sep = "")

activity_labels<-read.table(file="./UCI HAR Dataset/activity_labels.txt", header= FALSE , sep="")

Features<-read.table(file="./UCI HAR Dataset/features.txt", header = FALSE, sep="")

Subject_train<-read.table(file="./UCI HAR Dataset/train/Subject_train.txt", header=FALSE, sep="")

Subject_test<-read.table(file="./UCI HAR Dataset/test/Subject_test.txt", heade=FALSE, sep="")

#Merges the training and the test sets to create one data set.

View(X_test)
View(X_train)
View(y_test)
View(y_train)

  #Renaming Variable
test_Activities<-rename(y_test,"Act_Cod"=V1)
train_Activities<-rename(y_train,"Act_Cod"=V1 )

Subject_test<-rename(Subject_test, "Subject"=V1)
Subject_train<-rename(Subject_train, "Subject"=V1)

  #Mergin the data based

Test<-cbind(Subject_test,test_Activities, X_test)
Train<-cbind(Subject_train, train_Activities, X_train)
View(Test)
View(Train)
BD<-rbind(Train, Test)
View(BD)
names(BD)


#Extracts only the measurements on the mean and standard deviation for each measurement.

Var_mean<-rename(v1= "grep(\"mean\", Features$V2, value = TRUE)",as.data.frame(grep("mean", Features$V2,value = TRUE)))
                 
Var_Std<-rename(v1= "grep(\"std\", Features$V2, value = TRUE)",as.data.frame(grep("std", Features$V2,value = TRUE)))

mean_std<-rbind(Var_mean, Var_Std)

mean_std2<-arrange(mutate(merge(mean_std, Features, by.x="v1", by.y="V2"), "V3"="V"),V1)

mean_std2$V4<-paste(mean_std2$V3,mean_std2$V1, sep="")

View(mean_std2)

names(BD)

BD2<-select(BD, "Subject","Act_Cod",mean_std2[,4])

View(BD2)


#Uses descriptive activity names to name the activities in the data set

Act_Lab<-rename(activity_labels, Cod=V1, Desc_Act=V2)

BD3<-select(merge(Act_Lab, BD2, , by.x="Cod", by.y="Act_Cod"),"Desc_Act":"V552")

View(BD3)


#Appropriately labels the data set with descriptive variable names.

Desc_Names<-c("Desc_Act", "Subject", t(select(mean_std2, v1)))

View(Desc_Names)

names(BD3) <- Desc_Names

names(BD3)

View(BD3)

#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

BD4<-group_by(BD3, Desc_Act, Subject)

BD5<-BD4%>%summarise_all(funs(mean))

write.table(BD5, file="./Tidy_data_EA.txt", row.names = FALSE)

setwd("C:/Users/edgar/OneDrive/Documentos")