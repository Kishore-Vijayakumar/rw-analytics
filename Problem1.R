#################################################
# Problem 1
# USING AGGREGATION FUNCTIONS FOR DATA ANALYSIS
#################################################



#################################################

################################################

#################################################
# T1 Understand the data
################################################



# (i) Download the txt file (ENB_2023.txt) 
# (ii) Assign the data to a matrix.

# Importing the package into session and setting the working directory and importing the data
library(readr)
#Set your working directory
setwd()

the.data <- as.matrix(read.table("ENB_2023.txt"))



# (iii) The variable of interest is Y (Appliances). To investigate Y, 
# generate a subset of 340 with numerical data.
set.seed(223344556) #To reproduce sample
my.data <- the.data[sample(1:671,340),c(1:6)]


# (iv) Use scatter plots and histograms to understand the relationship
# between each of the variables X1, X2, X3, X4, X5, and your variable of interest Y.

# Since each variables have specific name we are going to rename them into X1,X2,X3,X4,X5 and Y for better readability.
colnames(my.data) <- c("X1","X2","X3","X4","X5","Y")

# In-order to create different plot we are going to convert the matrix into dataframe for better functioning.
my.dataframe <- data.frame(my.data)



# Creating 5 scatter plots


plot(my.dataframe$X1,my.dataframe$Y,
     xlab = "Temperature in kitchen area (Celsius)(X1)",ylab = "Energy uses of Appliances (Wh)(Y)",
     main = "Kitchen Temperature vs Energy Use",col="blue")

plot(my.dataframe$X2,my.dataframe$Y,
     xlab = "Percentage of Humidity in Kitchen area(X2)",ylab = "Energy uses of Appliances (Wh)(Y)",
     main = "Humidity in Kitchen area vs Energy Use",col="blue")

plot(my.dataframe$X3,my.dataframe$Y,
     xlab = "Temperature outside (Celsius)(X3)",ylab = "Energy uses of Appliances (Wh)(Y)",
     main = "Outside Temperature vs Energy Use",col="blue")

plot(my.dataframe$X4,my.dataframe$Y,
     xlab = "Percentage of Humidity outside (X4)",ylab = "Energy uses of Appliances (Wh)(Y)",
     main = "Outside Humidity vs Energy Use",col="blue")

plot(my.dataframe$X5,my.dataframe$Y,
     xlab = "Visibility (km) (X5)",ylab = "Energy uses of Appliances (Wh)(Y)",
     main = "Visibility vs Energy Use",col="blue")



# Histogram for the variables

hist(my.dataframe$X1,
     xlab = "Temperature in kitchen area (Celsius)(X1)",
     main = "Distribution of Kitchen area Temperature")

hist(my.dataframe$X2,
     xlab = "Percentage of Humidity in Kitchen area(X2)",
     main = "Distribution of Kitchen area Humidity")

hist(my.dataframe$X3,
     xlab = "Temperature outside (Celsius)(X3)",
     main = "Distribution of Outside Temperature")

hist(my.dataframe$X4,
     xlab = "Percentage of Humidity outside (X4)",
     main = "Distribution of Humidity outside")

hist(my.dataframe$X5,
     xlab = "Outdoor Visibility (km) (X5)",
     main = "Distribution of Outdoor Visibility")

hist(my.dataframe$Y,
     xlab = "Energy uses of Appliances (Wh)(Y)",
     main = "Distribution of Energy in Appliances")


# Lets understand the variables more

summary(my.dataframe)
# We can observe small skewness while examining the 5 point summary. Clarify it with a boxplot.
boxplot(my.dataframe[-6], main = "Boxplots for Variables", col = "skyblue")
# We can see there are skewness and outliers present in the data.


# Lets check the correlation between the variables
cor_matrix <- cor(my.dataframe[c("X1", "X2", "X3", "X4", "X5", "Y")])
cor_matrix


# Lets visualize this correlation using heatmap
heatmap(cor_matrix, 
        col = colorRampPalette(c("white", "orange", "red"))(100),  # Color palette
        main = "Correlation Heatmap",
        xlab = "Variables",
        ylab = "Variables")
# Now we can see that some variables are highly correlated to each other and some are not.





#################################################
# T2 Transform the data
################################################

# Before transforming the data we have to check for outliers and if present we need to fix it.
# From the boxplot we can see there are outliers present but need to check the quantity so we use IQR method to find it.



# Define the variables
variables <- c("X1", "X2", "X3", "X4", "X5")

# Define the IQR threshold
threshold <- 1.5

# Loop through each variable
for (variable in variables) {
  # Calculate IQR for the current variable
  q1 <- quantile(my.dataframe[[variable]], 0.25)
  q3 <- quantile(my.dataframe[[variable]], 0.75)
  iqr <- q3 - q1
  
  # Identify outliers and their values
  outliers <- my.dataframe[[variable]][my.dataframe[[variable]] < (q1 - threshold * iqr) | my.dataframe[[variable]] > (q3 + threshold * iqr)]
  
  # Display the count and values of outliers for the current variable
  cat("Number of outliers in", variable, ":", length(outliers), "\n")
  cat("Outlier values in", variable, ":", outliers, "\n\n")
}

# There is only a negligible amount of outliers we can remove/impute them but since we are choosing only 4 variable for transformation 
# we can only remove/impute them after choosing the variables.




# Lets check for skewness

# Load the necessary library for skewness
library(e1071)

# Define the variables
variables <- c("X1", "X2", "X3", "X4", "X5", "Y")

# Initialize an empty data frame to store skewness results
skewness_results <- data.frame(Variable = character(), Skewness = numeric(), stringsAsFactors = FALSE)

# Loop through each variable
for (variable in variables) {
  # Calculate skewness for the current variable
  skewness_value <- skewness(my.dataframe[[variable]])
  
  # Store results in the data frame
  result_row <- data.frame(Variable = variable, Skewness = skewness_value, stringsAsFactors = FALSE)
  skewness_results <- rbind(skewness_results, result_row)
}
# Display the summary data frame
print(skewness_results)




# Based on the skewness and distributions we can choose the variables X1,X2,X3,X5 and Y. We are ignoring 
# X4 since it has skewness close to zero. Now we need to impute the outliers before transformation.


# Impute outliers with the median value
for (variable in c("X2", "X3", "X5")) {
  # Calculate the median excluding outliers
  median_without_outliers <- median(my.dataframe[[variable]])
  
  # Identify outliers
  outliers <- my.dataframe[[variable]] < quantile(my.dataframe[[variable]], 0.25) | my.dataframe[[variable]] > quantile(my.dataframe[[variable]], 0.75)
  
  # Impute outliers with the median
  my.dataframe[[variable]][outliers] <- median_without_outliers
}

# Display the summary of the data after imputation
summary(my.dataframe)




# Since outlier is dealt with, we are transforming the data.
I <- c(1,2,3,5,6)
variables_to_transform <- my.dataframe[,I]

# We are doing log transformation for all 5 variables since all of them have positive skewness and and have good correlation.
data.transformed <- data.frame(variables_to_transform)
# Log transformation of variables X1, X2, X3, and X5
data.transformed$X1 <- log(variables_to_transform$X1 + 1)  # Adding 1 to avoid issues with zero values
data.transformed$X2 <- log(variables_to_transform$X2 + 1)  
data.transformed$X3 <- log(variables_to_transform$X3 + 1)  
data.transformed$X5 <- log(variables_to_transform$X5 + 1)  
data.transformed$Y <- log(variables_to_transform$Y + 1)

# To adjust the scale we are going to use MinMax Scaling and ZScore standardizing


# Min-Max normalization function
minmax <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

# Z-score standardization and scaling to unit interval function
unit.z <- function(x){
  return(0.15 * ((x - mean(x))/sd(x)) + 0.5)
}
data.log <- data.transformed

# Applying Min-Max normalization and Z-score standardization to the log-transformed variables
data.transformed$X1 <- minmax(data.transformed$X1)
data.transformed$X2 <- minmax(data.transformed$X2)
data.transformed$X3 <- minmax(data.transformed$X3)
data.transformed$X5 <- minmax(data.transformed$X5)
data.transformed$Y <- minmax(data.transformed$Y)

data.minmax <- data.transformed

data.transformed$X1 <- unit.z(data.transformed$X1)
data.transformed$X2 <- unit.z(data.transformed$X2)
data.transformed$X3 <- unit.z(data.transformed$X3)
data.transformed$X5 <- unit.z(data.transformed$X5)
data.transformed$Y <- unit.z(data.transformed$Y)  

summary(data.transformed) # Checking the transformation

# After completing transformation we are writing to a new file.

# Save this transformed data to a text file
write.table(data.transformed, "Kishore-transformed.txt")  




###################################################################
# T3 Build models and investigate the importance of each variable.
##################################################################



# (i) Download the AggWaFit.R file and load into the R workspace

library(lpSolve) # Importing Package
source("AggWaFit718.R") # Importing file



# (ii) Use the fitting functions to learn the parameters for

# a. A weighted arithmetic mean (WAM),

data.transformed_copy <- as.matrix(read.table("Kishore-transformed.txt"))  # import your saved data
fit.QAM(data.transformed_copy,"out_AM.txt","stat_AM.txt") # by default, it uses AM


# b. Weighted power means (WPM) with p = 0.5,

fit.QAM(data.transformed_copy,"out_PM05.txt","stat_PM05.txt",g = PM05,g.inv = invPM05)

# c. Weighted power means (WPM) with p = 2,

fit.QAM(data.transformed_copy,"out_QM.txt","stat_QM.txt",g = QM,g.inv = invQM)

# d. An ordered weighted averaging function (OWA).
fit.OWA(data.transformed_copy,"out_OWA.txt","stat_OWA.txt")

# e. The Choquet integral
fit.choquet(data.transformed_copy,"out_choq.txt","stat_choq.txt")
# The outputs and parameters of the above fit methods will be  stored in the different stat.txt files and output.txt files.






###################################################################
# T4 Use your model for prediction.
##################################################################

# Using your best fitting model from T3, predict Y (the area) for the following input
# X1=22; X2=38; X3=4; X4=88.2, X5=34.


# Since we took X1,X2,X3,X5 we are taking the same for transformation.

new_input_to_transform <- c(22,38,4,34)
transformed_new_data <- new_input_to_transform



# Customized Min-Max normalization function for new values
minmax_new <- function(new_x,x){
  return((new_x - min(x))/(max(x) - min(x)))
}

# Z-score standardization and scaling to unit interval function for new values
unit.z_new <- function(new_x,x){
  return(0.15 * ((new_x - mean(x))/sd(x)) + 0.5)
}

# We used log transformation so using the same here

transformed_new_data[1] <- log(new_input_to_transform[1] + 1)  # Adding 1 to avoid issues with zero values
transformed_new_data[2] <- log(new_input_to_transform[2] + 1)  
transformed_new_data[3] <- log(new_input_to_transform[3] + 1)  
transformed_new_data[4] <- log(new_input_to_transform[4] + 1)  


# Calling the transformation functions with new values.
transformed_new_data[1] <- minmax_new(transformed_new_data[1],data.log$X1)
transformed_new_data[2] <- minmax_new(transformed_new_data[2],data.log$X2)
transformed_new_data[3] <- minmax_new(transformed_new_data[3],data.log$X3)
transformed_new_data[4] <- minmax_new(transformed_new_data[4],data.log$X5)


transformed_new_data[1] <- unit.z_new(transformed_new_data[1],data.minmax$X1)
transformed_new_data[2] <- unit.z_new(transformed_new_data[2],data.minmax$X2)
transformed_new_data[3] <- unit.z_new(transformed_new_data[3],data.minmax$X3)
transformed_new_data[4] <- unit.z_new(transformed_new_data[4],data.minmax$X5)


# Now using best fit model we are predicting the new data values

# Here we are choosing choquet model as our best fit so, its weights are found out from the stat_choq.txt 
# and imputed to the model for prediction

C.weights <- c(0.559177176497739,0.0696322560330573,0.559177176497948,0,0.594945828813432,0.541088074274702,
               0.604184184453468,0.541088074274751,0.559177176497741,0.541088074274749,0.685244314812943,
               0.541088074274751,0.787531007619544,0.541088074274977,1.00000000000043) #Weights of the model.


# Prediction
result <- choquet(transformed_new_data,C.weights) # Calling the model
result # Here we got the result but in a transformed format, so need to convert into original
# This reverse transformation can be done by applying the same methods but in reverse order.


# Reverse of Zscore standardization
result <- ((result - 0.5) / 0.15) * sd(data.minmax$Y) + mean(data.minmax$Y)
result


# Reverse of minmax scaling
predictions <- result * (max(data.log$Y) - min(data.log$Y)) + min(data.log$Y)
predictions

# Reverse of log
predictions <- exp(predictions) - 1

predictions # The final output.

# When compared with given value Y=100 we got only 84. It appears that there is a difference between the predicted and measured values. 
# The predicted value is lower than the measured value, indicating that the model might be underestimating the target variable Y in this particular instance.
# It is close enough but there is room for improvement.