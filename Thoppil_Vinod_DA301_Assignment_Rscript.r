## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)
library(dplyr)
library(skimr)

library(moments)

install.packages('BSDA')
library(BSDA)
library(ggplot2)


# Import the data set.
data<- read.csv('turtle_sales.csv',header = TRUE)

# Print the data frame
print(data)
# sense checking data
###############
str(data)

#############
dim(data)
#####################

summary(data)

###############################################################

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sdata <- select(data,-c(Ranking,Year,Genre,Publisher))

# View the data frame.
str(sdata)
## To check if there are any missing values on the dataset.
sum(is.na(sdata))

# View the descriptive statistics.
summary(sdata)
#################
glimpse(sdata)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.


qplot(Product,NA_Sales,data=sales_data)
qplot(Product,EU_Sales,data=sales_data)
qplot(Product,Global_Sales,data=sales_data)
#############################################

qplot(NA_Sales,Platform,data=sdata)
qplot(EU_Sales,Platform,data=sdata)
qplot(Global_Sales,Platform,data=sdata)


#############################################


## 2b) Histograms
# Create histograms.

qplot(Global_Sales,bin=5,data=sdata)
qplot(NA_Sales,bin=5,data=sdata)
qplot(EU_Sales,bin=5,data=sdata)



## 2c) Boxplots
# Create boxplots.

qplot(Platform,EU_Sales,data=sales_data,geom ='boxplot')
qplot(Platform,NA_Sales,data=sales_data,geom ='boxplot')
qplot(Platform,Global_Sales,data=sales_data,geom ='boxplot')

qplot(Global_Sales,Platform,data=sdata,geom='boxplot')




###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

#Firstly, the turtle_sales.csv data has been imported to the DataFrame and sense
#checked.
#Irrelevant columns has been dropped and a new dataframe has been created, 
#called 'sdata'.
#Again, basic sense check has been performed.There are 352 observations and 
# 5 columns in the current dataset.
#The sales data is largely spread from 0 to a maximum value of 67.8.
# The median values for NA,EU,Global are 2.51,1.64,5.33 respectively.
# The data has been plotted against various variables.
#Chart types used are scatter, histogram and boxplots.
# Scatter charts:
# The data is quite widely distributed as shown in the graphs.
#
# EU Sales: Mostly centered around 4 million sales.
# NA sales: Mostly centered around 5 million sales.
# Global sales: Mostly ceneterd around the 10 million range.
# EU : X360 & PS3, PC are the most sold products respectively.
# NA:   X360, PS3 have strong performance.
# Global: Seems X360 has a  strong performance compared to others.



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.


# Check output: Determine the min, max, and mean values.


# View the descriptive statistics.
mean(sdata$NA_Sales)
median(sdata$NA_Sales)
min(sdata$NA_Sales)
max(sdata$NA_Sales)
# range :
max(sdata$NA_Sales)- min(sdata$NA_Sales)
##################

# Summarising the descriptive statistics to a new dataframe SS
ss <- summarise(sdata,mean_NA=mean(sdata$NA_Sales),mean_eu=mean(sdata$EU_Sales),
                mean_gl=mean(sdata$Global_Sales),min_na=min(sdata$NA_Sales),
                min_eu=min(sdata$EU_Sales),min_gl=min(sdata$Global_Sales),
                max_na=max(sdata$NA_Sales),max_eu=max(sdata$EU_Sales),
                max_gl=max(sdata$Global_Sales))
# View dataframe
ss
# Output:
#############################################################################
#mean_NA  mean_eu  mean_gl min_na min_eu min_gl max_na max_eu max_gl
#1 2.515966 1.643778 5.334688      0      0   0.01  34.02   23.8  67.85
###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.


##############################################################

df2 <- sdata%>% 
  group_by(Product) %>%
  summarise(NA_Sales =sum(NA_Sales),EU_Sales = sum(EU_Sales),
            Global_sales = sum(Global_Sales))
###############################################################

# View the data frame.
head(df2)
#############################################
#A tibble: 6 × 4
#Product NA_Sales EU_Sales Global_sales
#<int>    <dbl>    <dbl>        <dbl>
#  1     107    34.0     23.8          67.8
#2     123    26.6      4.01         37.2
#3     195    13       10.6          29.4
#4     231    12.9      9.03         27.1
#5     249     9.24     7.29         25.7
#6     254    21.5      2.42         29.4
############################################

# Explore the data frame.

skim(df2)
##############################################
#skim_variable n_missing complete_rate    mean      sd     p0     p25     p50
#1 Product               0             1 3490.   2412.   107    1468    3158   
#2 NA_Sales              0             1    5.06    4.56   0.06    2.50    3.61
#3 EU_Sales              0             1    3.31    3.08   0       1.46    2.3 
#4 Global_sales          0             1   10.7     8.13   4.2     5.52    8.09
#p75   p100 hist 
#1 5442.   9080   ▇▇▅▅▂
#2    5.57   34.0 ▇▂▁▁▁
#3    4.03   23.8 ▇▂▁▁▁
#4   12.8    67.8 ▇▂▁▁▁
#############################################################################
## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.


# Create histograms.
# Histogram of  sales of NA region
hist(df2$NA_Sales)
# Histogram of  sales of EU region
hist(df2$EU_Sales)
# Histogram of total sales of Global region
hist(df2$Global_sales)

# Create boxplots.
################
# boxplot NA sales
boxplot(df2$NA_Sales)
#boxplot eu slaes
boxplot(df2$EU_Sales)
# boxplot global sales
boxplot(df2$Global_sales)
###########bar chart
qplot(G_Sales,Platform,data=sdata,geom='bar')



###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
#Measure normality in BMI values.
# Q-Q plot:
qqnorm(sdata$NA_Sales)
# Add a reference line:
qqline(sdata$NA_Sales, col='red')

# qq plot for eu_sales column
qqnorm(sdata$EU_Sales)
# Add a reference line:
qqline(sdata$EU_Sales, col='red')
# qq plot for Global_sales column
qqnorm(sdata$Global_Sales)
# Add a reference line:
qqline(sdata$Global_Sales, col='red')


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.
# NA sales column
shapiro.test(sdata$NA_Sales)
######################################
#shapiro-Wilk normality test

data:  (sdata$NA_Sales)
#W = 0.6293, p-value < 2.2e-16
#######################################
# EU sales column
shapiro.test(sdata$EU_Sales)
#######################################
#Shapiro-Wilk normality test

data:  (sdata$EU_Sales)
#W = 0.64687, p-value < 2.2e-16
######################################
# Global sales column
shapiro.test(sdata$Global_Sales)
#####################################
#Shapiro-Wilk normality test

data:  sdata$Global_Sales
#W = 0.6818, p-value < 2.2e-16
################################################

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
#NA sales column
skewness(sdata$NA_Sales)
kurtosis(sdata$NA_Sales)
#EU sales column
skewness(sdata$EU_Sales)
kurtosis(sdata$EU_Sales)
#Global sales column
#NA sales column
skewness(sdata$Global_Sales)
kurtosis(sdata$Global_Sales)


## 3d) Determine correlation
# Determine correlation
cor(sdata$NA_Sales,sdata$Global_Sales)
#######################################
#cor(sdata$NA_Sales,sdata$Global_Sales)
#[1] 0.9349455
#######################
cor(sdata$EU_Sales,sdata$Global_Sales)
#[1] 0.8775575
#########################
cor(sdata$NA_Sales,sdata$EU_Sales)
#[1] 0.7055236
#########################################
#Since all the values are positive, all the sales data are positively 
#correlated.
###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


# Density plot
ggplot(df2, aes(x=NA_Sales)) + 
  geom_density()
ggplot(df2, aes(x=EU_Sales)) + 
  geom_density()
ggplot(df2, aes(x=Global_sales)) + 
  geom_density()

ggplot(df2, aes(x=EU_Sales)) + 
  geom_histogram()
ggplot(df2, aes(x=Global_sales)) + 
  geom_histogram()



################################

ggplot(df2, aes(x=Product, y=NA_Sales)) + 
  geom_point()+
  geom_smooth(method=lm)

##################

ggplot(df2, aes(x=Product, y=EU_Sales)) + 
  geom_point()+
  geom_smooth(method=lm)
#############################
ggplot(df2, aes(x=Product, y=Global_sales)) + 
  geom_point()+
  geom_smooth(method=lm)


  
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
#The data has been further cleaned, sense checked, statistical methods have been
#applied including removing un wanted columns, checking for missing values and 
#descriptive statistics.
# From Q-Q test, Shapiro test( since p value is higher than p=0.05, the data is
# not normally distributed. The dataset is skewed. However, there is positive 
#correlation between the sales data columns.
# Various plots have been plotted to understand the data in every perspective.
# The final plots chosen for the visualisation was scatter plots, as they were 
#the suitable choice to show the product vs the sales and was able to add a 
#regression line of best fit.
# The global sales was just under 20millions products,which the EU sale 
#contributed to around 5 million and NA sales to about 10 million.


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.



## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.



###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




