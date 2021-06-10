library(tidyverse)

#Link to original dataset: https://www.kaggle.com/janiobachmann/bank-marketing-dataset

bank_data <- read_csv2('...Bank Marketing Dataset/bank.csv') #add data path here
dim(bank_data)
head(bank_data)

#-----------------------------------funModelling---------------------------------------
library(funModeling)

df_status(bank_data)
#To analyze missing values, data types and unique values
plot_num(bank_data)
#To plot different kinds of plots to visualize data
#To plot histograms of quantity of only numerical columns
freq(bank_data)
#To calculate and plot frequency distribution

#'pdays' and 'previous' have most of the values =0
#pdays hold the number of days passed after the client was last contacted from
#previous campaign. -1 probably means that client was not contacted before or 
# it might only stands for that its missing data. Since it is not sure the true
#nature of the -1 in pdays and same is the case with previous. They are removed
bank_data <- bank_data[!(bank_data$pdays <= -1 & bank_data$previous <= -1),]

profiling_num(bank_data)
#To calculate statistical results (automatically excludes non-numeric columns)
y1 <- 0
for(i in 1:nrow(bank_data)){
  if(bank_data$y[i]=='yes') {y1[i] <- 1}
  else {y1[i] <- 0}}
#Convert output y (YES/NO) to numeric (0/1) for model building

bank_data[, 'y1'] <- y1
bank_data$y1[10:20]
#To add logical output y as y1 in the data frame

correlation_table(bank_data, 'y1')
#To calculate correlations between all variables against target variable
#It only uses numerical variables

all_corr <- var_rank_info(bank_data, 'y1')
#To calculate correlation using all variables (numerical and categorical)

all_corr <- all_corr[-c(1), ]
all_corr
#Since above correlation also included target variable to to compare with itself
#It will be removed


ggplot(all_corr, aes(x=reorder(var, gr), y = gr, fill = var)) + 
  geom_bar(stat = 'identity') + coord_flip() + theme_bw() + xlab('') + 
  ylab('Each feature importance according to the gr (Information Gain)') + 
  guides(fill = FALSE)
#Plot of var_rank_info to visualize better

cross_plot(data = bank_data, input = c('pdays','balance'), target = 'y1')
#From the above plot, we can take the most correlated features for further analysis
#To visualize distribution between input and target variables

categ_analysis(data = bank_data, input = 'marital', target = 'y1')
#To do quantitative analysis (input must be categorical variable)

categorical_char = convert_df_to_categoric(data = bank_data, n_bins = 6)
categorical_char
#To convert every column into character variable
#Criteria is to discritize into equal frequency variables
#This is useful for applying machine learning models as some of them take discrete inputs

var_rank_info(bank_data, "y")
#To check importance of all the other variables with respect to target variable ('y' in my case)
#Entropy (en), mutual information (mi), information gain (ig) and gain ratio (gr)

#-----------------------------------DataExplorer---------------------------------------
library(DataExplorer)

plot_str(bank_data)
#To visualize the structure of the dataset (useful for complicated datasets)

plot_str(bank_data, type = 'r')
#To plot in a radial manner (easier to understand)

t(introduce(bank_data))
#To analyze basic information from the dataset
#t() is to change columns in rows

plot_intro(bank_data)
#To visualize the above information
#No missing values or columns

plot_bar(bank_data)
#Bar charts visualization (only uses discrete features)

plot_histogram(bank_data)
#Histograms to visualization of continuous features
#histograms are also used to visualize the distribution of variables

plot_density(bank_data)
#Another way to visualize distribution of continuous variables is by plotting density
read_cs
plot_qq(bank_data)
#To visualize the deviation from probability distribution

plot_correlation(bank_data)
#To plot confusion matrix of all variables

plot_prcomp(na.omit(bank_data))
#PCA is used to analyze specific features

bank_pca <- na.omit(bank_data[, c('age', 'balance', 'duration')])
plot_prcomp(bank_pca, variance_cap = 0.9, nrow = 2L, ncol = 2L)
#To use only specific features