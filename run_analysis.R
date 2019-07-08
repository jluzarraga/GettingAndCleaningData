## script for final project - getting and cleaning data
## July 7th, 2019
library(dplyr)

#Loading testing and training datasets
##setwd("C:/Users/jluza/OneDrive/Documents/RFiles/DataCleaning/UCI HAR DataSet")
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")

x_test  <- read.table("./test/X_test.txt")
y_test  <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

##adding subject and activity columns to testing and training datasets
data_test <- cbind(subject_test, y_test, x_test)
data_train<- cbind(subject_train, y_train, x_train)

# Loading activity labels
activity_labels <- read.table("activity_labels.txt")

## merging testing and training datasets
xdata <- rbind(data_train, data_test)

##load features dataset + adding subject and activity columns
features <- read.table("features.txt")
subject  <- data.frame(V1=1, V2="subject")
activity <- data.frame(V1=2, V2="activity")
features <- rbind(subject, activity, features)
features$V1 <- seq(1, 563, 1)

##Labeling DS with more descriptive names
featuresFiltered <- features[grep("subject|activity|mean()|std()", features$V2), ]
featuresFiltered$V2 <- gsub('-mean', 'Mean', featuresFiltered$V2)
featuresFiltered$V2 <- gsub('-std', 'StdDev', featuresFiltered$V2)
featuresFiltered$V2 <- gsub('[-()]', '', featuresFiltered$V2)
featuresFiltered$V2 <- gsub('^t', 'Time', featuresFiltered$V2)

new_xdata <- xdata[,features[grep("subject|activity|mean()|std()", features$V2), "V1"]]
colnames(new_xdata) <- featuresFiltered$V2

## Setting activity names
new_xdata[which(new_xdata$activity == 1), "activity"] <- "WALKING"
new_xdata[which(new_xdata$activity == 2), "activity"] <- "WALKING_UPSTAIRS"
new_xdata[which(new_xdata$activity == 3), "activity"] <- "WALKING_DOWNSTAIRS"
new_xdata[which(new_xdata$activity == 4), "activity"] <- "SITTING"
new_xdata[which(new_xdata$activity == 5), "activity"] <- "STANDING"
new_xdata[which(new_xdata$activity == 6), "activity"] <- "LAYING"

# Generating final dataset
new_data_df <- tbl_df(new_xdata)
arrange(new_data_df, activity, subject)
final_ds <- new_data_df  %>% 
    group_by(activity, subject) %>% 
    summarise_each(funs(mean))
write.table(final_ds, "finaldataset.txt", row.names = FALSE, quote = FALSE)