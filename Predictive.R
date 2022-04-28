library(dplyr)
library(util)
library(ggplot2)
library(missForest)
# DATASET

df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00397/LasVegasTripAdvisorReviews-Dataset.csv",sep = ";")
n <-ncol(df)


View(df)
# df <- apply (df, 2, function(x) {x[sample( c(1:n), floor(n/10))] <- NA; x} )
> tail(df)
#SUMMARY OF THE DATASET
summary(df)

# #DATA PREPROCESSING
# #STORING THE DATA SET IN NEW VARIABLE KEEPS THE ORIGINALITY OF THE DATA SET BEFORE CLEANING
# df1 <-data.frame(df)
# #AN INITIAL LOOK AT THE DATA FRAME
# str(df1)
# #checking the observation,resulting in 301 obs. of 23 variables and 22 columns
# View(df1)
# #summarizing the data set for checking number of NA's
# summary(df1)
# #replacing empty cells with NA
# df1[df1==""] <- NA
# 
# #Removing the columns and rows having missing data more than 60%
# #initializing a variable with the percentage value
# #REMOVING ROWS WITH MORE THAN 60% NA
# threshold <- 0.6 # for 60%
# df1 <- df1 %>% filter(rowMeans(is.na(.))< threshold)
# #REMOVING COLUMNS WITH MORE THAN 60% NA
# df1 <- df1[,which(colMeans(!is.na(df1))>threshold)]
# summary(df1)
# rownames(df1)<- NULL

# #Imputing the missing value
# #-----------------------------------------------------------#KNN-IMPUTATION ---------------------------------------------------
# #The k nearest neighbours is an algorithm that is used for simple classification
# #This can be very useful in making predictions about the missing values by finding the k's closest 
# neighbours to the observation with missing data 
# # and then imputing them based on the non-missing values in the neighbourhood
# # This KNN imputation is used in statistical data so that the NA columns are filled with appropriate 
# nearby value,so that the calculations are made easier.
# # This is a standard method used in imputing the missing values.
# # The main reason of using KNN for this data set is it creates a basic mean impute then uses the resulting 
# complete list to construct a KDTree. Then, it uses the resulting KDTree to compute nearest neighbours 
# (NN). 
# #After it finds the k-NNs, it takes the weighted average of them.
# #install.packages("VIM")
# library(VIM)
# df1 <- kNN(df1,variable = c('uczaa'),metric=NULL,k=6)
# df1 <- subset(df1,select =1:21)
# View(df1)
# #for a particular column replacing NA with median of the column eg:chip_rate
# df1$chip_rate <- ifelse(is.na(df1$chip_rate),median(df1$chip_rate,na.rm = TRUE),df1$chip_rate)
# #Applying Mean or Median for Imputing Missing Values in remaining columns[mean is used in 
# continuous data]
# for(i in 1:ncol(df1)){
#   df1[is.na(df1[,i]), i] <- mean(df1[,i], na.rm = TRUE)
# }
# summary(df1)


# TO CALCULATE STANDARD DEVIATION

for(i in 1:ncol(df))
{
  if(is.integer(df[,i]))
  {
    print(paste0("The SD of ",colnames(df[i])))
    print(sd(df[,i]))
  }
}

for(i in 1:nrow(df))
{
  if(df$Member.years[i]>0)
    df$Member.years[i]<-df$Member.years[i]
  else
    df$Member.years[i]<-1
}

#Converting categorical values into factors

for (i in 1:ncol(df)) {
  
  if(is.character(df[,i]))
  {
    df[,i]<- as.factor(df[,i])
  }
}

str(df)
# EDA

#BAR PLOT
ggplot(df,aes(Score))+geom_bar(aes(fill=factor(Traveler.type)))+theme_bw()

ggplot(df,aes(Hotel.stars))+geom_bar(aes(fill=factor(Score)))+theme_bw()

#PIE CHARTS
ggplot(df, aes(x = "", y =Hotel.stars,fill=Score)) +geom_col(color = "black") + coord_polar(theta = "y")


#HISTOGRAMS

ggplot(df,aes(Nr..reviews))+geom_histogram(bins=10,alpha=0.5,aes(fill=factor(User.continent)))+theme_bw()


#BOX PLOT

ggplot(df,aes(Traveler.type,Score))+geom_boxplot(aes(fill=factor(User.continent),alpha=0.4))+theme_bw()

#TIME PLOT

 ggplot(df, aes(x=Member.years, y=Nr..reviews)) +
  geom_line() + 
  xlab("")
 
#SCATTER PLOT
 ggplot(df,aes(Nr..reviews,User.continent))+geom_point(fill="blue")+theme_bw()
