
# Importing required libraries
getwd() #getting the working directory to check that everything is in the right spot
library(ggplot2) #for plotting
library(reshape2)
library(corrplot)
library(dplyr)
library(gridExtra) #  this for grid.arrange
library(rlang) #for the ehanced scatterplots

# Reading the dataset using read.csv because it is a csv file 
data <- read.csv("diabetesdata.csv")

# Displaying the initial correlation matrix before cleaning so that I can see the difference after cleaning 
# Displaying correlation matrix
sapply(data, class)
numeric_data <- data[sapply(data, is.numeric)]

cor_matrix <- cor(numeric_data)

corrplot(cor_matrix, method="circle")
#  Ensure the Outcome column is a factor because it is categorical e.g. yes or no 1/0
data$Outcome <- as.factor(data$Outcome)

#creating histograms for variables I saw had zero values when observing the csv file
#this allows me to infer their shape and to decide how to replace them with either then mean or the median 
# Function to create side-by-side histograms
plot_histogram <- function(data, column_name) #here I am declaring the function
{
  #the next step is that I initialize the plot with a ggplot obj
  #aes_string lets me define the asthetics using strings (setting the x aesthetic to column name etc)
  p <- ggplot(data, aes(!!sym(column_name), fill=as.factor(Outcome))) +
    #adds histogram geometry, and the position=dodge makes sure that all the bars are side by side 
    geom_histogram(position="dodge", bins=30) + 
    #setting the title of my graph which is the column name e.g. glucose + distribution by outcome
    labs(title=paste(column_name, "Distribution by Outcome")) +
    #customizing the colors of the bars and specifies that 0 = green = non-diabetic and that 1=red=diabetic
    scale_fill_manual(values=c("green", "red"), 
                      name="Outcome", 
                      breaks=c("0", "1"), 
                      labels=c("Non-diabetic", "Diabetic")) +
    #theme setting
    theme_minimal() +
    #x-axis label will be the column/the variables name
    xlab(column_name) +
    #the y-axis label will be frequency
    ylab("Frequency")
  
  return(p) # Return the plot object
}

# Variables of interest that have zero values so that I can visualize their skew 
variables <- c("Insulin", "Glucose", "SkinThickness", "BloodPressure", "BMI", "Pregnancies", "DiabetesPedigreeFunction")

# Collect all the plots in a list
plots <- lapply(variables, function(var) plot_histogram(data, var))

# Display the histograms in a grid layout
grid.arrange(grobs=plots, ncol=3) #number of columns for the grid of histograms 

# Replacing 0 values with median/mean values
#not replacing pregnancy as you can have 0 pregnancies 
#using the median for skin thickness due to the skew 
data$SkinThickness[data$SkinThickness == 0] <- median(data$SkinThickness, na.rm=TRUE)
#using the median for glucose due to the slight skew
data$Glucose[data$Glucose == 0] <- median(data$Glucose, na.rm=TRUE)
#using the median for BMI due to the slight skew
data$BMI[data$BMI == 0] <- median(data$BMI, na.rm=TRUE)
#using the median of blood pressure due to the slight skew 
data$BloodPressure[data$BloodPressure == 0] <- median(data$BloodPressure, na.rm=TRUE)
#using the median of insulin due to the skew 
data$Insulin[data$Insulin == 0] <- median(data$Insulin, na.rm=TRUE)

#OUTLIERS USING IQR
#would have usually used a z-test but due to my non-normal data this was not appropriate as it assumes normality

# here I am calculating Q1 and Q3 for each variable excluding but not outcome (-ncol)
Q1 <- sapply(data[,-ncol(data)], quantile, probs=0.25)
Q3 <- sapply(data[,-ncol(data)], quantile, probs=0.75)

# here I am calculating the IQR for each variable 
IQRs <- Q3 - Q1

# here I am calculating the lower and upper bounds for each variable 
#lower bounds 
lower_bounds <- Q1 - 1.5 * IQRs
#upper bounds
upper_bounds <- Q3 + 1.5 * IQRs

# here the potential outliers for each variable are being detected
outliers_list_iqr <- lapply(1:ncol(data[,-ncol(data)]), function(i) 
{
  which(data[[i]] < lower_bounds[i] | data[[i]] > upper_bounds[i])
})

# using the variable name so it is easier to infer when printed 
names(outliers_list_iqr) <- colnames(data[,-ncol(data)])

# priting out the potential outliars 
outliers_list_iqr

#i didnt remove the outliers as they all seemed to lie within natural bounds, though I could have I wanted to keep the integrity of the data set 

# Displaying correlation matrix
sapply(data, class)
numeric_data <- data[sapply(data, is.numeric)]

cor_matrix <- cor(numeric_data)

corrplot(cor_matrix, method="circle")
#DISTRIBUTION PLOTTING 

# 1. Bar Chart:
#initializing the plotting using the ggplot funciton
ggplot(data, aes(x=Outcome, fill=Outcome)) + 
  #adding the bar geometry
  geom_bar() + 
  #setting the colors and labels to fill
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), 
                    labels = c("Non-Diabetic", "Diabetic")) +
  #setting the plots labels
  labs(title="Bar Chart of the Outcome Variable", x="", y="Count") +
  #modifiying the theme to remove the ledgend title 
  theme(legend.title = element_blank())

# 2. Pie Chart:
#creates a summary of the outcome variable
data_summary <- data.frame(table(data$Outcome))
#initializing the plotting using the ggplot funciton (since i am doing a pie chart the x is set to empty string and the y is set to freq)
ggplot(data_summary, aes(x="", y=Freq, fill=Var1)) + 
  #adds the bar geometry with a width of 1 
  geom_bar(width = 1, stat = "identity") + 
  #converts the bar chart into a pie chart through polar coords
  coord_polar("y", start=0) + 
  #setting custom colors and labels likewise to the bar chart 
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), 
                    labels = c("Non-Diabetic", "Diabetic")) +
  #setting the title 
  labs(title="Pie Chart of the Outcome Variable") + 
  theme(legend.title = element_blank())


#VISUALISING THE CLEAN DATA FOR ANALYSIS 

#the charts show me that my dataset is imblan
#  Ensure the Outcome column is a factor
data$Outcome <- as.factor(data$Outcome)

#re-using the function to create side by side histograms again


# Variables of interest (displaying all independent variables for analysis)
variables <- c("Insulin", "Glucose", "SkinThickness", "BloodPressure", "BMI", "Age", "Pregnancies", "DiabetesPedigreeFunction")

# Collect all the plots in a list
plots <- lapply(variables, function(var) plot_histogram(data, var))

# Display the histograms in a grid layout
grid.arrange(grobs=plots, ncol=3) #number of columns for the grid of histograms 

#i am displaying the histograms again post cleaning because i wanted to be able to properly analyze the now clean data 


dev.off()

#SCATTERPLOTS FOR CORRELATIONS AND QUESTIONS BETWEEN VARIABLES 

#Enhanced scatter plots with custom rectangles and labeled outcomes
#creating the enhanced_scatter_plot function definition (data is the dataframe containing the data, x/y_col are the column names, rectx/y are the starting x and y coords, rect_width/height are the width and height of the rectangle)
#utilizing the rlang library here 
enhanced_scatter_plot <- function(data, x_col, y_col, rect_x, rect_y, rect_width, rect_height) {
  
  # Ensure that Outcome is a factor
  data$Outcome <- as.factor(data$Outcome)
  # Using ggplot with aes_string for dynamic column names
  ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]], color = Outcome)) +
    # Adding scatter points for each row of the data
    geom_point() +
    # Draws a rectangle on the scatter plot to highlight specific clusters
    geom_rect(aes(xmin=rect_x, xmax=rect_x + rect_width, ymin=rect_y, ymax=rect_y + rect_height), 
              color="red", fill=NA) +
    # Specifies manual color assignments for each point
    scale_color_manual(values = c("red", "green"), 
                       labels = c("Diabetic", "Non-Diabetic"), 
                       breaks = levels(data$Outcome)) +
    # Setting the title and color legend of the plot
    labs(title=paste("Impact of", x_col, "and", y_col, "on Diabetes"), color="Status") + 
    # Using theme_minimal for aesthetics
    theme_minimal()
}


#pair plot scatter to answer some questions I have about the data set 
#correlation between glucose and insulin
#do glucose and insulin have an impact on diabetes outcome
print(enhanced_scatter_plot(data, "Glucose", "Insulin", 150, -50, 50, 450))
#correlation between glucose and BMI
#do glucose and BMI have an impact on diabetes outcomes
print(enhanced_scatter_plot(data, "Glucose", "BMI", 125, 30, 75, 30))
#correlation between glucose and Age
#does glucose and age have an impact on diabetes outcome
print(enhanced_scatter_plot(data, "Glucose", "Age", 50, 20, 80, 30))

# Pair plots (Scatterplot matrices)
pairs(data, pch=21, bg=c("red", "green")[unclass(data$Outcome)])

# Check again for null values
sum(is.na(data))

#HYPOTHESIS TESTING USING T-TESTS

#AGE HYPOTHESIS "The likelihood of diabetes increases in those over the age of 45"

# Split data based on age and Outcome
above_45_diabetic <- subset(data, Age > 45 & Outcome == 1)
below_or_equal_45_diabetic <- subset(data, Age <= 45 & Outcome == 1)

above_45_non_diabetic <- subset(data, Age > 45 & Outcome == 0)
below_or_equal_45_non_diabetic <- subset(data, Age <= 45 & Outcome == 0)

# Calculate the proportions of diabetic individuals in each age group
prop_above_45 <- nrow(above_45_diabetic) / (nrow(above_45_diabetic) + nrow(above_45_non_diabetic))
prop_below_or_equal_45 <- nrow(below_or_equal_45_diabetic) / (nrow(below_or_equal_45_diabetic) + nrow(below_or_equal_45_non_diabetic))

# Convert Outcome variable to numeric
data$Outcome <- as.numeric(as.character(data$Outcome))

# Re-split the data based on the age and Outcome after conversion
above_45 <- subset(data, Age > 45)
below_or_equal_45 <- subset(data, Age <= 45)

#conducting a welches t-test, setting var.equal to false because of my non-normal distribution and I dont want it to assume that my data set is normal
result_age <- t.test(above_45$Outcome, below_or_equal_45$Outcome, var.equal = FALSE)

#print the welch test results 
print(result_age)

#automatically tell me the hypothesis rejection or not 
if (result_age$p.value < 0.05) {
  print(paste("Reject the null hypothesis: The likelihood of diabetes is different for individuals above and below the age of 45. Diabetic proportion for above 45 is", prop_above_45, "and for 45 or below is", prop_below_or_equal_45))
} else {
  print("Fail to reject the null hypothesis: The likelihood of diabetes is the same for individuals regardless of being above or below the age of 45.")
}

#BMI HYPOTHESIS "Those with a BMI greater than 35 are more likely to have diabetes"


# For BMI and Diabetes 
#splitting the data 
above_35_BMI <- subset(data, BMI > 35)
below_or_equal_35_BMI <- subset(data, BMI <= 35)

#conducting a welches t-test, again using var.equal because of my dataset
result_BMI <- t.test(above_35_BMI$Outcome, below_or_equal_35_BMI$Outcome, var.equal = FALSE)

#printing the result 
print(result_BMI)

#automatically displaying if the hypothesis should be rejected or not 
if (result_BMI$p.value < 0.05) {
  print("Reject the null hypothesis: The likelihood of diabetes differs between those with a BMI above 35 and those 35 or below.")
} else {
  print("Fail to reject the null hypothesis for BMI.")
}

#GLUCOSE HYPOTHESIS "High plasma glucose levels leads to diabetes"

# Split data based on diabetes outcome
diabetic_data <- subset(data, Outcome == 1)   # 1 for diabetic
non_diabetic_data <- subset(data, Outcome == 0)  #0 for non diabetic 

# Conduct a two-sample t-test comparing glucose levels of both groups using Welch's
result_glucose <- t.test(diabetic_data$Glucose, non_diabetic_data$Glucose, var.equal = FALSE) #var.equal = false is how i am telling the test that my dataset isnt normal

print(result_glucose)

# Interpret the results
if (result_glucose$p.value < 0.05) {
  if(result_glucose$estimate[1] > result_glucose$estimate[2]) {
    print("Reject the null hypothesis: Diabetic individuals have significantly higher plasma glucose levels compared to non-diabetic individuals.")
  } else {
    print("Reject the null hypothesis: Diabetic individuals have significantly lower plasma glucose levels compared to non-diabetic individuals.")
  }
} else {
  print("Fail to reject the null hypothesis: There's no significant difference in plasma glucose levels between diabetic and non-diabetic individuals.")
}