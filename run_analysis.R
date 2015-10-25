run_analysis <- function(){
  # load the feature
  f <- read.delim("UCI HAR Dataset/features.txt", header = F,sep="")
  fmeanstd <- grep("mean\\(|std\\(",f$V2)
  fcolname <- gsub("\\(\\)","",f$V2)
  fcolname <- gsub("-","_",fcolname)
  
  #load the measurement
  xtest <- read.delim("UCI HAR Dataset/test/X_test.txt", header = F, sep="", col.names = fcolname)
  xtrain <- read.delim("UCI HAR Dataset/train/X_train.txt", header = F, sep="", col.names = fcolname)
  x <- rbind(xtrain,xtest)
  x <- x[,fmeanstd]
  
  #load the activity
  ydetail <- cbind(c(1,2,3,4,5,6),c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS','SITTING','STANDING','LAYING'))
  ytest <- read.delim("UCI HAR Dataset/test/Y_test.txt", header = F, sep="")
  ytrain <- read.delim("UCI HAR Dataset/train/Y_train.txt", header = F, sep="")
  y <- rbind(ytrain,ytest)
  y <- merge(x = y, y = ydetail, by.x = "V1", by.y = 1)
  y <- rename(y,ActivityIdx=V1,Activity=V2)
  
  #load the subject
  stest <- read.delim("UCI HAR Dataset/test/subject_test.txt", header = F, sep="", col.names=c("subject"))
  strain <- read.delim("UCI HAR Dataset/train/subject_train.txt", header = F, sep="", col.names=c("subject"))
  subject <- rbind(strain,stest)
  
  #combine the data
  result <- cbind(x,y,subject)
  
  #summarize
  result_summary <- result %>% group_by(subject,Activity,ActivityIdx) %>% summarize_each(funs(mean))
  
  result_summary
}
