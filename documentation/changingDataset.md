# Updating Datasets in the R Shiny App

## Overview
This guide explains how to update the datasets used in the R Shiny app, particularly how to switch out the current dataset (`DailyAverage.txt`) with a new one. The process is designed to ensure consistency in the app's functionality, regardless of any changes to the data.

## Changing the Dataset

1. **Replace the Data File**
    - The dataset is read from a tab-delimited text file. If you need to switch the dataset, replace the current `DailyAverage.txt` file located in the `./data/` directory with the new dataset.
    - Ensure that the new dataset is in the same format (i.e., tab-delimited, with a header row) to maintain compatibility with the app.

   ```r
   dailyLakeData <- read.table("./data/DailyAverage.txt", header = TRUE, sep = "\t")

2. **Standardize Column Names**
    - Column names are standardized to ensure consistency across the app's code. If you switch datasets, you must ensure that the new dataset has the same column order and names as the original.
    - Important: Do not change the names of the columns. Changing column names will cause issues with the app, as other parts of the code rely on these specific names.
    - If the new dataset has a different order for the columns, update the column order in the code. If new columns are added, add them to the list of standardized column names.

   ```r
   colnames(dailyLakeData) <- c("sensorType", "meter", "date",
                             "value", "STD", "var", "n")

3. **Handling New Columns**
   - If the new dataset includes additional columns, add them to the colnames() function to maintain consistency. Ensure that the new column names are correctly mapped and the order of columns matches what is expected in the app.

   Example: If the new dataset includes a column location, update the colnames as follows:
    ```r
   colnames(dailyLakeData) <- c("sensorType", "meter", "date",
                             "value", "STD", "var", "n", "location")

4. **Check for data consistency**
   - After updating the dataset and column names, run the app to ensure that the data is properly loaded and no errors occur due to mismatched column names or data formats.
   - Verify that the new dataset is providing the expected outputs in the app's user interface and functions.
   - Make sure the actual contents of columns are the same
