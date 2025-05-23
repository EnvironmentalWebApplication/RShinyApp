# Updating Datasets in the R Shiny App

## Overview
This guide explains how to update the datasets used in the R Shiny app, particularly how to switch out the current dataset (`DailyAverage.txt`) with a new one. The process is designed to ensure consistency in the app's functionality, regardless of any changes to the data.

## Changing the Dataset

1. **Replacing the Data File**
    - The dataset is read from a tab-delimited text file. If you need to switch the dataset, replace the current `DailyAverage.txt` file located in the `./data/` directory with the new dataset.
    - Ensure that the new dataset is in the same format (i.e., tab-delimited, with a header row) to maintain compatibility with the app.

   ```r
   dailyLakeData <- read.table("./data/DailyAverage.txt", header = TRUE, sep = "\t")

2. **Standardize Column Names**
    - Use the colnames() function to standardize data sets
    - If the new dataset has a different order for the columns, update the column order in colnames()

   ```r
   colnames(dailyLakeData) <- c("sensorType", "meter", "date",
                             "value", "STD", "var", "n")

3. **Handling New Columns**
   - If the new dataset includes additional columns, add them to the colnames() function to maintain consistency. 

   Example: If the new dataset includes a column location, update the colnames as follows:
    ```r
   colnames(dailyLakeData) <- c("sensorType", "meter", "date",
                             "value", "STD", "var", "n", "location")

4. **Check for data consistency**
   - After updating the dataset and column names, run the app to ensure that the data is properly loaded and no errors occur due to mismatched column names or data formats.
   - Verify that the new dataset is providing the expected outputs in the app's user interface and functions.
   - **Make sure the actual contents of columns are the same**, the contents are case-sensitive, capitalization matters
