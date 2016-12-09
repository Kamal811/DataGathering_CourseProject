##**********************Set the Activity Names from the subject file

colActivityNames <- c("WALKING" ,
                      "WALKING_UPSTAIRS",
                      "WALKING_DOWNSTAIRS",
                      "SITTING",
                      "STANDING",
                      "LAYING")

##**********************Read the training data set into data frame
X_train <- read.table("X_train.txt", 
               fill=FALSE, 
               strip.white=TRUE, stringsAsFactors = FALSE)

Y_train <- read.table("Y_train.txt", 
                              fill=FALSE, 
                              strip.white=TRUE , stringsAsFactors = FALSE)

Subject_train <- read.table("subject_train.txt", 
                      fill=FALSE, 
                      strip.white=TRUE , stringsAsFactors = FALSE)



body_acc_x_train <- read.table("body_acc_x_train.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)


body_acc_y_train <- read.table("body_acc_y_train.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)


body_acc_z_train <- read.table("body_acc_z_train.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)
##************

body_gyro_x_train <- read.table("body_gyro_x_train.txt", 
                                fill=FALSE, 
                                strip.white=TRUE , stringsAsFactors = FALSE)

body_gyro_y_train <- read.table("body_gyro_y_train.txt", 
                                fill=FALSE, 
                                strip.white=TRUE , stringsAsFactors = FALSE)

body_gyro_z_train <- read.table("body_gyro_z_train.txt", 
                                fill=FALSE, 
                                strip.white=TRUE , stringsAsFactors = FALSE)


##*************************


total_acc_x_train <- read.table("total_acc_x_train.txt", 
                                fill=FALSE, 
                                strip.white=TRUE, stringsAsFactors = FALSE)

total_acc_y_train <- read.table("total_acc_y_train.txt", 
                                fill=FALSE, 
                                strip.white=TRUE, stringsAsFactors = FALSE)

total_acc_z_train <- read.table("total_acc_z_train.txt", 
                                fill=FALSE, 
                                strip.white=TRUE , stringsAsFactors = FALSE)


##**********************Read the test data set into data frame


X_test <- read.table("X_test.txt", 
                     fill=FALSE, 
                     strip.white=TRUE , stringsAsFactors = FALSE)

Y_test <- read.table("Y_test.txt", 
                     fill=FALSE, 
                     strip.white=TRUE , stringsAsFactors = FALSE)

Subject_test <- read.table("subject_test.txt", 
                           fill=FALSE, 
                           strip.white=TRUE , stringsAsFactors = FALSE)



body_acc_x_test <- read.table("body_acc_x_test.txt", 
                              fill=FALSE, 
                              strip.white=TRUE , stringsAsFactors = FALSE)


body_acc_y_test <- read.table("body_acc_y_test.txt", 
                              fill=FALSE, 
                              strip.white=TRUE , stringsAsFactors = FALSE)


body_acc_z_test <- read.table("body_acc_z_test.txt", 
                              fill=FALSE, 
                              strip.white=TRUE , stringsAsFactors = FALSE)
##************

body_gyro_x_test <- read.table("body_gyro_x_test.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)

body_gyro_y_test <- read.table("body_gyro_y_test.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)

body_gyro_z_test <- read.table("body_gyro_z_test.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)


##*************************


total_acc_x_test <- read.table("total_acc_x_test.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)

total_acc_y_test <- read.table("total_acc_y_test.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)

total_acc_z_test <- read.table("total_acc_z_test.txt", 
                               fill=FALSE, 
                               strip.white=TRUE , stringsAsFactors = FALSE)



##********************Merge the training and test files of each type

X_Merged <- rbind(X_train , X_test) 
Y_Merged <- rbind(Y_train , Y_test) 
Subject_Merged <- rbind(Subject_train , Subject_test) 

for(i in 1:6)
{
  Y_Merged$V1[Y_Merged$V1 == i] <- colActivityNames[i]
}

Data_Set <- cbind(Subject_Merged , Y_Merged , X_Merged)

##****************************************************
body_acc_x <- rbind(body_acc_x_train , body_acc_x_test) 
body_acc_y <- rbind(body_acc_y_train , body_acc_y_test) 
body_acc_z <- rbind(body_acc_z_train , body_acc_z_test) 

##************
body_gyro_x <- rbind(body_gyro_x_train , body_gyro_x_test) 
body_gyro_y <- rbind(body_gyro_y_train , body_gyro_y_test) 
body_gyro_z <- rbind(body_gyro_z_train , body_gyro_z_test) 


##******

total_acc_x <- rbind(total_acc_x_train , total_acc_x_test) 
total_acc_y <- rbind(total_acc_y_train , total_acc_y_test) 
total_acc_z <- rbind(total_acc_z_train , total_acc_z_test) 



##******************Merged all data with mean and srandard deviations and column names for each observation

Complete_Mean_StdDev <- data.frame( Obs = numeric() ,Subject = character() , Activity = character(), X_Merged_Avg = numeric() , X_Merged_StdDev = numeric() , 
                                    body_acc_x_avg = numeric(), body_acc_x_stddev = numeric() , 
                                    body_acc_y_avg = numeric() , body_acc_y_stddev = numeric() , 
                                    body_acc_z_avg = numeric() , body_acc_z_stddev = numeric() ,
                                    
                                    body_gyro_x_avg = numeric() , body_gyro_x_stddev = numeric() ,
                                    body_gyro_y_avg = numeric() , body_gyro_y_stddev = numeric() ,
                                    body_gyro_z_avg = numeric() , body_gyro_z_stddev = numeric() ,
                                    
                                    total_acc_x_avg = numeric() , total_acc_x_stddev = numeric() ,
                                    total_acc_y_avg = numeric() , total_acc_y_stddev = numeric() ,
                                    total_acc_z_avg = numeric() , total_acc_z_stddev = numeric() , stringsAsFactors = FALSE
)
library(stats)
count <- nrow(X_Merged)

for(i in 1:count)
{ 
  newRow <- list(i , Subject_Merged[i,1] , Y_Merged[i,1] , mean(as.numeric(X_Merged[i,]) , na.rm = TRUE) , sd(as.numeric(X_Merged[i,]) , na.rm = TRUE) ,
              
              mean(as.numeric(body_acc_x[i,]) , na.rm = TRUE) , sd(as.numeric(body_acc_x[i,]) , na.rm = TRUE),
              mean(as.numeric(body_acc_y[i,]) , na.rm = TRUE) , sd(as.numeric(body_acc_x[i,]) , na.rm = TRUE),
              mean(as.numeric(body_acc_z[i,]) , na.rm = TRUE) , sd(as.numeric(body_acc_x[i,]) , na.rm = TRUE),
              
              mean(as.numeric(body_gyro_x[i,]) , na.rm = TRUE) , sd(as.numeric(body_gyro_x[i,]) , na.rm = TRUE),
              mean(as.numeric(body_gyro_y[i,]) , na.rm = TRUE) , sd(as.numeric(body_gyro_y[i,]) , na.rm = TRUE),
              mean(as.numeric(body_gyro_z[i,]) , na.rm = TRUE) , sd(as.numeric(body_gyro_z[i,]) , na.rm = TRUE),
              
              mean(as.numeric(total_acc_x[i,]) , na.rm = TRUE) , sd(as.numeric(total_acc_x[i,]) , na.rm = TRUE),
              mean(as.numeric(total_acc_y[i,]) , na.rm = TRUE) , sd(as.numeric(total_acc_y[i,]) , na.rm = TRUE),
              mean(as.numeric(total_acc_z[i,]) , na.rm = TRUE) , sd(as.numeric(total_acc_z[i,]) , na.rm = TRUE))
  
  Complete_Mean_StdDev[i ,] <- newRow
}
write.table(Complete_Mean_StdDev , "Consolidated.txt" , row.names = FALSE)