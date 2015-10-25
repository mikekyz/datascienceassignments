# datascienceassignments


## Description of Scripts for Course Project
This file provide and describes the code snippets used in this project and how they are related. The objects of this project is achieved using three functions.

- download_file: This function  downloads and extracts the zipped the working folder

```r
download_file <- function(){
                
        ## Download zipped file from the internet
        fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl, destfile="UCIHARDataset.zip")
        
        ## Unzip the downloaded file
        unzip("UCIHARDataset.zip", overwrite = TRUE)
        
}
```

- load_data: This function is a helper function that takes 3 arguments. The first one is set which indicates if the data to be loaded is a train or a test set, The second is a data frame with two variable index and name that represent the column name of the data collected that was extracted from the features.txt file. Lastly is the labels argument that contains the label for each of the activities performed by the sample.

The load_data function includes proper column names for each of the columns in the provided data set and also include id to each row that data for each sample thereafter a label of each activities that each row stood for. It then returns a raw dataset.

This function is mainly called in the run_analysis() function to load the train and test datasets.


```r
load_data <- function(set, features, labels){
        library(data.table)
        
        
        ## set the path to files
        prefix <- paste(set, '/', sep = '')
        data_file <- paste(prefix, 'X_', set, '.txt', sep = '')
        label_file <- paste(prefix, 'y_', set, '.txt', sep = '')
        subject_file <- paste(prefix, 'subject_', set, '.txt', sep = '')
        
        ## load the data into a data frame
        mydata <- read.table(data_file)[, features$index]
        
        ## Set the column name of file using info from features dataset
        names(mydata) <- features$name
        
        label_data <- read.table(label_file)[, 1]
        mydata$label <- factor(label_data, levels=labels$level, labels=labels$label)
        
        subject_data <- read.table(subject_file)[, 1]
        mydata$subject <- factor(subject_data)
        
        # convert to data table
        data.table(mydata)
}
```


- run_analysis: This is the main function that performs the analysis. This function calls the load_data function to load cleaner set of the train and test data, it then joins the data sets together, cleans up the variable names for easy reading and writes out the raw data and the cleaned datasets.


```r
run_analysis <- function () {
       
        
        # Get the features
        feature_data <- read.table('features.txt', col.names = c('index', 'name'))
        features <- subset(feature_data, grepl('-(mean|std)[(]', feature_data$name))
        
        # Get the labels
        label_values <- read.table('activity_labels.txt', col.names = c('level', 'label'))
        
        # Read train and test data sets
        train_dataset <- load_data('train', features, label_values)
        test_dataset <- load_data('test', features, label_values)
   
        # The raw data set
        raw_dataset <- rbind(train_dataset, test_dataset)
        
        # Generate the tidy data set
        tidy_dataset <- raw_dataset[, lapply(.SD, mean), by=list(label, subject)]
        
        # Make the variable name descriptive and readable
        names <- names(tidy_dataset)
        names <- gsub('-mean', 'Mean', names) # Replace `-mean' by `Mean'
        names <- gsub('-std', 'Std', names) # Replace `-std' by 'Std'
        names <- gsub('[()-]', '', names) # Remove the parenthesis and dashes
        names <- gsub('BodyBody', 'Body', names) # Replace `BodyBody' by `Body'
        setnames(tidy_dataset, names)
        
        # Write the raw and the tidy data sets to files
        setwd('..')
        write.table(raw_dataset, file = 'rawdata.csv', row.names = FALSE)
        write.table(tidy_dataset, file = 'tidydata.txt',
                  row.names = FALSE)
        
        # Return the tidy data set
        tidy_dataset     
}
```


