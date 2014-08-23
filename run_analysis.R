# libraries required
if (!require("data.table")) {
	install.packages("data.table")
	require("data.table")
}
# This script assumes the files required are in the folder "data"
path <- getwd()
datapath <- file.path(path, "data")

# Reading the files
subject_test <- read.table(file.path(datapath, "subject_test.txt"))
subject_train <- read.table(file.path(datapath, "subject_train.txt"))
X_train <- read.table(file.path(datapath, "X_train.txt"))
y_train <- read.table(file.path(datapath, "y_train.txt"))
X_test <- read.table(file.path(datapath, "X_test.txt"))
y_test <- read.table(file.path(datapath, "y_test.txt"))
features <- read.table(file.path(datapath, "features.txt"))
activity_labels <- read.table(file.path(datapath, "activity_labels.txt"))

# Merge data
data <- rbind(X_train, X_test)
colnames(data) <- c(as.character(features[,2]))
# Get only the measurements on the mean and standard deviation
colMean <- grep("mean()", colnames(data), fixed=TRUE)
colStd<- grep("std()", colnames(data), fixed=TRUE)
dataMeanStd <- data[,c(colMean, colStd)] 

dataActivity <- rbind(y_train, y_test)
setnames(dataActivity, "V1", "activity")
dataAll <- cbind(dataActivity, dataMeanStd)

# adding activities names
for(i in 1:length(dataAll[,1])){
        dataAll[i,1]<-as.character(activity_labels[dataAll[i,1],2])
}

dataSubject <- rbind(subject_train, subject_test)
setnames(dataSubject, "V1", "subject")

# creating a new dataset
dataMerged <- cbind(dataSubject, dataAll)

# creating an independant tidy data set
tidyData <- aggregate(dataMerged[,3] ~ subject + activity, data = dataMerged, FUN= "mean")

for(i in 4:ncol(dataMerged)){
        tidyData[,i] <- aggregate( dataMerged[,i] ~ dataMerged$subject + dataMerged$activity, data = dataMerged, FUN= "mean" )[,3]
}

# adding colnames to the data set
colnames(tidyData)[3:ncol(tidyData)] <- colnames(dataMeanStd)
tidyData <- data.table(tidyData)
#writing the final tidy data set to a file
write.table(tidyData, row.name=FALSE, file = "FinalData.txt")
