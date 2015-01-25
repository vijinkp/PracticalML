test <- function(){



# ====================================================
# Coursera Practical Machine Learning Project Work
# Weight Lifting Exercises Prediction
# Author: Vijin K P
# ====================================================


# Loading required packages
library('caret')
library('ggplot2')
library('gridExtra')
library('data.table')

# Reading the data from file
inData <- read.csv("pml-training.csv")
# Converting the data frame to data.table
inData <- data.table(inData)

# Subsetting actual measurement column & classe
inData <- inData[, c('accel_belt_x', 'accel_arm_x', 'accel_dumbbell_x', 'accel_forearm_x',
	'accel_belt_y', 'accel_arm_y', 'accel_dumbbell_y', 'accel_forearm_y',
	'accel_belt_z', 'accel_arm_z', 'accel_dumbbell_z', 'accel_forearm_z',
	'gyros_belt_x', 'gyros_arm_x', 'gyros_dumbbell_x', 'gyros_forearm_x',
	'gyros_belt_y', 'gyros_arm_y', 'gyros_dumbbell_y', 'gyros_forearm_y',
	'gyros_belt_z', 'gyros_arm_z', 'gyros_dumbbell_z', 'gyros_forearm_z',
	'magnet_belt_x', 'magnet_arm_x', 'magnet_dumbbell_x', 'magnet_forearm_x',
	'magnet_belt_y', 'magnet_arm_y', 'magnet_dumbbell_y', 'magnet_forearm_y',
	'magnet_belt_z', 'magnet_arm_z', 'magnet_dumbbell_z', 'magnet_forearm_z',
	'pitch_belt', 'pitch_arm', 'pitch_dumbbell', 'pitch_forearm',
	'roll_belt', 'roll_arm', 'roll_dumbbell', 'roll_forearm',
	'total_accel_belt', 'total_accel_arm', 'total_accel_dumbbell', 'total_accel_forearm',
	'yaw_belt', 'yaw_arm', 'yaw_dumbbell', 'yaw_forearm',
	'classe'), with=FALSE]

# Plotting total_accel_(belt|arm|dumbbell|forearm) with one other
# and observe the variation of other variables
varname<- "total_accel"
p1 <- ggplot(inData, aes_string(x=paste(varname, "belt",sep="_"), y=paste(varname, "arm",sep="_"), color="classe")) + geom_point(shape=1)
p2 <- ggplot(inData, aes_string(x=paste(varname, "belt",sep="_"), y=paste(varname, "dumbbell",sep="_"), color="classe")) + geom_point(shape=1)
p3 <- ggplot(inData, aes_string(x=paste(varname, "belt",sep="_"), y=paste(varname, "forearm",sep="_"), color="classe")) + geom_point(shape=1)
p4 <- ggplot(inData, aes_string(x=paste(varname, "arm",sep="_"), y=paste(varname, "dumbbell",sep="_"), color="classe")) + geom_point(shape=1)
p5 <- ggplot(inData, aes_string(x=paste(varname, "arm",sep="_"), y=paste(varname, "forearm",sep="_"), color="classe")) + geom_point(shape=1)
p6 <- ggplot(inData, aes_string(x=paste(varname, "dumbbell",sep="_"), y=paste(varname, "forearm",sep="_"), color="classe")) + geom_point(shape=1)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)


# Computing the correlation
cor(inData[, c("roll_belt", "roll_arm", "roll_dumbbell", "roll_forearm",
				"pitch_belt", "pitch_arm", "pitch_dumbbell", "pitch_forearm", 
				"yaw_belt", "yaw_arm", "yaw_dumbbell", "yaw_forearm", 
				"total_accel_belt", "total_accel_arm", "total_accel_dumbbell", "total_accel_forearm"), with=FALSE])

cor(inData[, c("roll_belt", "pitch_belt", "yaw_belt", "total_accel_belt"), with=FALSE])

cor(inData[, c("roll_arm", "pitch_arm", "yaw_arm", "total_accel_arm"), with=FALSE])

cor(inData[, c("roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "total_accel_dumbbell"), with=FALSE])

cor(inData[, c("roll_forearm", "pitch_forearm", "yaw_forearm", "total_accel_forearm"), with=FALSE])

cor(inData[, c("accel_belt_x", "accel_arm_x", "accel_dumbbell_x", "accel_forearm_x",
	"accel_belt_y", "accel_arm_y", "accel_dumbbell_y", "accel_forearm_y",
	"accel_belt_z", "accel_arm_z", "accel_dumbbell_z", "accel_forearm_z"), with=FALSE])

cor(inData[, c("gyros_belt_x", "gyros_arm_x", "gyros_dumbbell_x", "gyros_forearm_x",
	"gyros_belt_y", "gyros_arm_y", "gyros_dumbbell_y", "gyros_forearm_y",
	"gyros_belt_z", "gyros_arm_z", "gyros_dumbbell_z", "gyros_forearm_z"), with=FALSE])

cor(inData[, c("magnet_belt_x", "magnet_arm_x", "magnet_dumbbell_x", "magnet_forearm_x",
	"magnet_belt_y", "magnet_arm_y", "magnet_dumbbell_y", "magnet_forearm_y",
	"magnet_belt_z", "magnet_arm_z", "magnet_dumbbell_z", "magnet_forearm_z"), with=FALSE])


	
	
# Creating a training model with random forest classifier
# Setting the training parameter
trCtrl <- trainControl(method = "cv", preProcOptions = list(thresh = 0.95))
# Calling train function from caret package
modelFit <- train(form = classe ~ ., data=inData, method='rf', trControl=trCtrl)

# Reading the Test data
testData <- read.csv("pml-testing.csv")
testData <- data.table(testData)
testData <- testData[, c('accel_belt_x', 'accel_arm_x', 'accel_dumbbell_x', 'accel_forearm_x',
	'accel_belt_y', 'accel_arm_y', 'accel_dumbbell_y', 'accel_forearm_y',
	'accel_belt_z', 'accel_arm_z', 'accel_dumbbell_z', 'accel_forearm_z',
	'gyros_belt_x', 'gyros_arm_x', 'gyros_dumbbell_x', 'gyros_forearm_x',
	'gyros_belt_y', 'gyros_arm_y', 'gyros_dumbbell_y', 'gyros_forearm_y',
	'gyros_belt_z', 'gyros_arm_z', 'gyros_dumbbell_z', 'gyros_forearm_z',
	'magnet_belt_x', 'magnet_arm_x', 'magnet_dumbbell_x', 'magnet_forearm_x',
	'magnet_belt_y', 'magnet_arm_y', 'magnet_dumbbell_y', 'magnet_forearm_y',
	'magnet_belt_z', 'magnet_arm_z', 'magnet_dumbbell_z', 'magnet_forearm_z',
	'pitch_belt', 'pitch_arm', 'pitch_dumbbell', 'pitch_forearm',
	'roll_belt', 'roll_arm', 'roll_dumbbell', 'roll_forearm',
	'total_accel_belt', 'total_accel_arm', 'total_accel_dumbbell', 'total_accel_forearm',
	'yaw_belt', 'yaw_arm', 'yaw_dumbbell', 'yaw_forearm'), with=FALSE]

# Predicting for the test data
test2 <- predict(modelFit, newdata=testData)

# Writing the restuls to files
pml_write_files(test2)


}