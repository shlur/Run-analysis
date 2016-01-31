# loading the data

Xtest <-  read.table(file = "~/UCI HAR Dataset/test/X_test.txt");
ytest <-  read.table(file = "~/UCI HAR Dataset/test/y_test.txt");
Xtrain <-  read.table(file = "~/UCI HAR Dataset/train/X_train.txt");
ytrain <-  read.table(file = "~/UCI HAR Dataset/train/y_train.txt");
subjtrain <-  read.table(file = "~/UCI HAR Dataset/train/subject_train.txt");
subjtest <-  read.table(file = "~/UCI HAR Dataset/test/subject_test.txt");
features <-  read.table(file = "~/UCI HAR Dataset/features.txt");
activity <-  read.table(file = "~/UCI HAR Dataset/activity_labels.txt");


# binding with subject and labels

Xtest <- cbind(subjtest, Xtest); Xtrain <- cbind(subjtrain, Xtrain);
colnames(Xtest)[1] <-"subject"; colnames(Xtrain)[1] <-"subject";

Xtest <- cbind(ytest, Xtest); Xtrain <- cbind(ytrain, Xtrain);
colnames(Xtest)[1] <-"activity_id"; colnames(Xtrain)[1] <-"activity_id";

names(activity) <- c("id","activityname");


# 1. Merges the training and the test sets to create one data set.

Xtest <- mutate(Xtest, sample = "test");
Xtrain <- mutate(Xtrain, sample = "train");

big_sample <- bind_rows(Xtest, Xtrain);

d <- dim(big_sample);
colnames(big_sample)[3:(d[2]-1)] <- as.vector(features[,2]);
names(big_sample) <- make.names(names(big_sample), unique = TRUE);

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

cut_sample <- big_sample[,grep("[Mm]ean|[Ss]td|^activity|^subj|^sample",names(big_sample))];

# 3. Uses descriptive activity names to name the activities in the data set

cut_sample <- merge(cut_sample, activity, by.x = "activity_id", by.y = "id");

# 4. Appropriately labels the data set with descriptive variable names.

# replace "." and add large letters to function name (Mear or Std) 
names(cut_sample) <- gsub("\\.","",names(cut_sample)); 
names(cut_sample) <- gsub("std","Std",names(cut_sample)); 
names(cut_sample) <- gsub("mean","Mean",names(cut_sample));


# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
#    of each variable for each activity and each subject.

cut_sample[,-1][,-88] %>%
  group_by(subject, activityname) %>%
  summarise_each(funs(mean)) -> final_sample;

final_sample <- as.data.frame(final_sample);

write.table(final_sample, file = "~/UCI HAR Dataset/combined.txt", sep = " ", row.name=FALSE);

print(final_sample);



