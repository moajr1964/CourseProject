# Moacyr Machado Cardoso Junior                          #
# e-mail: moacyr@ita.br                                  #
##########################################################
#Final Course Project - Getting & Cleaning Data

# Description:
# You will be required to submit: 
# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis,
# 3) a code book that describes the variables, the data, and any transformations
#    or work that you performed to clean up the data called CodeBook.md. 
#    You should also include a README.md in the repo with your scripts. 
#    This repo explains how all of the scripts work and how they are connected.  

# You should create one R script called run_analysis.R that does the following. 

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation 
#    for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each 
#    variable for each activity and each subject. 
##############################################################################
# NOTE setting working directory for different computers
setwd("C:\\Users\\ITA\\Dropbox\\week1_gettingdata")
setwd("C:\\Users\\ITA\\Dropbox\\week1_gettingdata\\data\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset")

# ITA
setwd("C:\\Users\\Moacyr\\Dropbox\\week1_gettingdata")
setwd("C:\\Users\\Moacyr\\Dropbox\\week1_gettingdata\\data\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset")

# Saving Image
save.image(file="getting_AND_cleaning.Rdata")
load (file="getting_AND_cleaning.Rdata")

#---------------------------------------------------------------------------
# Reading archives from getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset

activity.labels<-scan("activity_labels.txt",what=" ")
features_info<-read.table("features_info.txt",header=FALSE,sep="\t")
features <- scan(".\\features.txt", what="") 

# Transformming Features into a matrix [2,561]
features.sep.ma<-matrix((features),2,561)

# Transformming activity Labels into a matrix [2,6]
activity.labels.sep.ma<-matrix((activity.labels),2,6)

#----------------------------------------------------------------------------
#Reading archives from \\test 

X_test <- scan(".\\test\\X_test.txt") #, what="") 
Y_test<-read.table(".\\test\\Y_test.txt",header=FALSE,sep="\t")
subject_test<-read.table(".\\test\\subject_test.txt",header=FALSE,sep="\t")

Y_test<-as.matrix(Y_test)
subject_test<-as.matrix(subject_test)
# Transforming X_test into a matrix [2947,561]
X_test.ma<-matrix((X_test),2947,561)
head( X_test.ma,5)

#---------------------------------------------------------------------------
# Including Names to the X_test.ma

colnames(X_test.ma)<-(features.sep.ma[2,])

#----------------------------------------------
# Extracting only Mean() and std() columns

# Creates 1 dataframe for Mean() columns and 1 dataframe for std()

my.vars.mean<-grep("mean()",colnames(X_test.ma),value=FALSE,fixed=TRUE)
my.vars.std<-grep("std()",colnames(X_test.ma),value=FALSE,fixed=TRUE)
 
Data.test.mean<-X_test.ma[,c(my.vars.mean)]
Data.test.std<-X_test.ma[,c(my.vars.std)]

# Combine the two dataframes with desired variables (columns)

Data.test.Mean_std<-cbind(Data.test.mean,Data.test.std)
Data.test.Mean_std[1:10,62:66]

#------------------------------------------------------------------------
# Including Columns - Subject_test and Y_test
#subject_test<-as.data.frame(subject_test)
colnames(subject_test)<-"subject"
colnames(Y_test)<-"labels"
Data_test.tidy<-cbind(subject_test,Y_test,Data.test.Mean_std) 

sapply(Data.test.Mean_std,mode)
sapply(Data.test.Mean_std,class)

#----------------------------------------------
#Reading archives from \\train

X_train<-scan(".\\train\\X_train.txt") #, what="") 
Y_train<-read.table(".\\train\\Y_train.txt",header=FALSE,sep="\n")
subject_train<-read.table(".\\train\\subject_train.txt",header=FALSE,sep="\n")

# Transforming X_train into a matrix [7352,561]

X_train.ma<-matrix((X_train),7352,561)
head(X_train.ma)

#---------------------------------------------------------------------------
# Including Names to the X_train.ma

dim(X_train.ma)
colnames(X_train.ma)<-(features.sep.ma[2,])

#---------------------------------------------------------------------------
# Extracting only Mean() and std() columns

# Creates 1 dataframe for Mean() columns and 1 dataframe for std()
 
my.vars.mean<-grep("mean()",colnames(X_train.ma),value=FALSE,fixed=TRUE)
my.vars.std<-grep("std()",colnames(X_train.ma),value=FALSE,fixed=TRUE)

Data.train.mean<-X_train.ma[,c(my.vars.mean)]
Data.train.std<-X_train.ma[,c(my.vars.std)]

# Combine the two dataframes with desired variables (columns)

Data.train.Mean_std<-cbind(Data.train.mean,Data.train.std)
Data.train.Mean_std[1:10,1:5]

#------------------------------------------------------------------------
# Including Columns - Subject_train and Y_train

colnames(subject_train)<-"subject"
colnames(Y_train)<-"labels"
Data_train.tidy<-cbind(subject_train,Y_train,Data.train.Mean_std) 

#------------------------------------------------------------------------
# Merging test data set and train data set
# Obtaining an data.frame 10299 by 68

dataSet<-rbind(Data_test.tidy,Data_train.tidy)
dim(dataSet)
class(dataSet)
str(dataSet)

# Checking for NA presence <<>> no NA
colSums(is.na(dataSet))
any(is.na(dataSet))

#--------------------------------------------------------------------------
# Creating a data frame with Mean for each variable (66) splited by labels (6)
# and subjects (30)

 Final.tidy.table<-aggregate(dataSet[,3:68],
    by=list(dataSet[,1],dataSet[,2]), FUN="mean")
 
#------------------------------------------------------------------------
# Fixing names of columns
# All lower case; descriptive; no Underscore or dots or () or  
# or white spaces

spltNames<-strsplit(colnames(Final.tidy.table),"-")
firstElement<-function(x){paste(x[1],x[2],x[3],sep="")}
colnames(Final.tidy.table)<-sapply(spltNames,firstElement)
colnames(Final.tidy.table)

# Extract NA - Execute 2 times
colnames(Final.tidy.table)<-sub("NA","",colnames(Final.tidy.table))
colnames(Final.tidy.table)<-sub("NA","",colnames(Final.tidy.table))

colnames(Final.tidy.table)

# Extract ()
colnames(Final.tidy.table)<-sub("\\()","",colnames(Final.tidy.table))

# to low case
colnames(Final.tidy.table)<-tolower(colnames(Final.tidy.table))

# Fix name for the 1st and 2nd columns
colnames(Final.tidy.table)<-sub("Group.1","subject",colnames(Final.tidy.table))
colnames(Final.tidy.table)<-sub("Group.2","labels",colnames(Final.tidy.table))

write.table(colnames(Final.tidy.table),"ColNames.txt")
#-----------------------------------------------------------------
# Writing final tidy data set

write.table(Final.tidy.table,"Final.tidy.table.txt")
 
# Dimension and size
# [180,68)
# 104176 bytes
dim(Final.tidy.table)
object.size(Final.tidy.table);print(object.size(Final.tidy.table),units="Mb")
#----------------------------------------------------------------------------
#<<EOF>> 
Revised<-date()
print(Revised)
#"Sat Jun 14 16:26:19 2014"