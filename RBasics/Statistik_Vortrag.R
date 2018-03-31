##########################################################################  
#	The company:
#	This document descibes a first approach to solve the Prudential Kaggle challenge.
#	Prudential is one of the largest issuer of life insurances in the USA.
#	As a matter of fact, the company faces a high turnover rate when it comes to signing a life insurace contract, 
#	because the number of questions asked during the insurance process is too high. Therfore people are turned off.
#
#	The challenge:
#	The challenge is to develope a predictive model that accurately classifies the risk of each individual and streamlines the process.
#	
#
#	The data:
#	Variable					Description
#	Id							A unique identifier associated with an application.
#	Product_Info_1-7			A set of normalized variables relating to the product applied for
#	Ins_Age						Normalized age of applicant
#	Ht							Normalized height of applicant
#	Wt							Normalized weight of applicant
#	BMI							Normalized BMI of applicant
#	Employment_Info_1-6			A set of normalized variables relating to the employment history of the applicant.
#	InsuredInfo_1-6				A set of normalized variables providing information about the applicant.
#	Insurance_History_1-9		A set of normalized variables relating to the insurance history of the applicant.
#	Family_Hist_1-5				A set of normalized variables relating to the family history of the applicant.
#	Medical_History_1-41		A set of normalized variables relating to the medical history of the applicant.
#	Medical_Keyword_1-48		A set of dummy variables relating to the presence of/absence of a medical keyword being associated with the application.
#	Response					This is the target variable, an ordinal variable relating to the final decision associated with an application
#
#
#	What do we do next?
#	1. Creating a multiple regression model
# 	1.0. Load the data
#	1.1. Validate Data
#	1.2. Data Cleansing
# 	1.3. Split data into train and test sets
#	1.4. Data modelling (Modellanpassung)
# 	1.4.1. Single model
# 	1.4.2. Multiple models
# 	1.4.3. All models
# 	1.5. Model validation (Modellvergleich)
#	1.6. Prediction
#
#	2. Cluster Analysis
##########################################################################  

###########################################  
# 0. Introduction into R
###########################################

# 0.1 What is R?

# 0.2 Basic commands
help(hist)
mean(train_data_initial$BMI)
mean(train_data_initial[["BMI"]])
available.packages()
help(package="MASS")








###########################################  
# 1. Creating a multiple regression model
###########################################

#####################################  
# 1.0. Load the data
#####################################

# create an odbc connection



# install varies R packages
#install.packages('RODBC')																		 	
#install.packages('MAAS')																		 	
#install.packages('leaps')																		  	
#install.packages('car')																  	
#install.packages('rgl')
#install.packages("scatterplot3d")
#install.packages("rgl")
#äinstall.packages('data.table')

# Load the libraries
#library('RODBC')																					 
library(MASS)																					
library('leaps')																					 
library('car')																					
#library(rgl)
library(scatterplot3d)
library(leaps)
library(data.table)



# Connect to the MSSQl server  
#odbcconnect <- odbcConnect("Statistik_Connection") 													
# Select the trainings set
#train_data_initial<-sqlFetch(odbcconnect,"PRUDENTIAL.train") 													
# Select the test set
#validation<-sqlFetch(odbcconnect,"PRUDENTIAL.test")															

# categorize data into groups
train_data_initial<-read.table("/home/ventum/Downloads/input/train.csv", header=TRUE, sep=",")
validation<-read.table("/home/ventum/Downloads/input/test.csv", header=TRUE, sep=",")

Product_Info_1_7 = c("Product_Info_1","Product_Info_2","Product_Info_3","Product_Info_4","Product_Info_5","Product_Info_6","Product_Info_7")
Employment_Info_1_6 = c("Employment_Info_1","Employment_Info_2","Employment_Info_3","Employment_Info_4","Employment_Info_5","Employment_Info_6")
InsuredInfo_1_6 = c("InsuredInfo_1","InsuredInfo_2","InsuredInfo_3","InsuredInfo_4","InsuredInfo_5","InsuredInfo_6")
Insurance_History_1_9 =c("Insurance_History_1","Insurance_History_2","Insurance_History_3","Insurance_History_4","Insurance_History_5","Insurance_History_6","Insurance_History_7","Insurance_History_8","Insurance_History_9")
Family_Hist_1_5 =c("Family_Hist_1","Family_Hist_2","Family_Hist_3","Family_Hist_4","Family_Hist_5")
Medical_History_1_41 =c("Medical_History_1","Medical_History_2","Medical_History_3","Medical_History_4","Medical_History_5","Medical_History_6","Medical_History_7","Medical_History_8","Medical_History_9","Medical_History_10","Medical_History_11","Medical_History_12","Medical_History_13","Medical_History_14","Medical_History_15","Medical_History_16","Medical_History_17","Medical_History_18","Medical_History_19","Medical_History_20","Medical_History_21","Medical_History_22","Medical_History_23","Medical_History_24","Medical_History_25","Medical_History_26","Medical_History_27","Medical_History_28","Medical_History_29","Medical_History_30","Medical_History_31","Medical_History_32","Medical_History_33","Medical_History_34","Medical_History_35","Medical_History_36","Medical_History_37","Medical_History_38","Medical_History_39","Medical_History_40","Medical_History_41")
Medical_Keyword_1_48=c("Medical_Keyword_1"," Medical_Keyword_2" ," Medical_Keyword_3" ," Medical_Keyword_4" ," Medical_Keyword_5" ," Medical_Keyword_6" ," Medical_Keyword_7" ," Medical_Keyword_8" ," Medical_Keyword_9" ," Medical_Keyword_10" ," Medical_Keyword_11" ," Medical_Keyword_12" ," Medical_Keyword_13" ," Medical_Keyword_14" ," Medical_Keyword_15" ," Medical_Keyword_16" ," Medical_Keyword_17" ," Medical_Keyword_18" ," Medical_Keyword_19" ," Medical_Keyword_20" ," Medical_Keyword_21" ," Medical_Keyword_22" ," Medical_Keyword_23" ," Medical_Keyword_24" ," Medical_Keyword_25" ," Medical_Keyword_26" ," Medical_Keyword_27" ," Medical_Keyword_28" ," Medical_Keyword_29" ," Medical_Keyword_30" ," Medical_Keyword_31" ," Medical_Keyword_32" ," Medical_Keyword_33" ," Medical_Keyword_34" ," Medical_Keyword_35" ," Medical_Keyword_36" ," Medical_Keyword_37" ," Medical_Keyword_38" ," Medical_Keyword_39" ," Medical_Keyword_40" ," Medical_Keyword_41" ," Medical_Keyword_42" ," Medical_Keyword_43" ," Medical_Keyword_44" ," Medical_Keyword_45" ," Medical_Keyword_46" ," Medical_Keyword_47" ," Medical_Keyword_48" )


categoricalVariables = c("Product_Info_1", "Product_Info_2","Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", "Medical_History_10", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")
continuousVariables  = c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5")
discreteVariables    = c("Medical_History_1", "Medical_History_10", "Medical_History_15", "Medical_History_24", "Medical_History_32")


#####################################  
# 1.1. Validate Data
#####################################

# Inspect BMI data
mean(train_data_initial[["BMI"]])
var(train_data_initial[["BMI"]])
sd(train_data_initial[["BMI"]])
hist(train_data_initial[["BMI"]])
qqnorm(train_data_initial[["BMI"]])


#Inspect Ins-Age data
mean(train_data_initial[["Ins_Age"]])
var(train_data_initial[["Ins_Age"]])
sd(train_data_initial[["Ins_Age"]])
hist(train_data_initial[["Ins_Age"]])
qqnorm(train_data_initial[["Ins_Age"]])


#Inspect Ht data
summary(train_data_initial[["Ht"]])
mean(train_data_initial[["Ht"]])
var(train_data_initial[["Ht"]])
sd(train_data_initial[["Ht"]])
hist(train_data_initial[["Ht"]])
qqnorm(train_data_initial[["Ht"]])

# --> doe not look so good. try to linerize it!
hist(train_data_initial[["Ht"]])
hist(log(train_data_initial[["Ht"]]))
hist(exp(train_data_initial[["Ht"]]))


qqnorm(train_data_initial[["Ht"]])
qqnorm(log(train_data_initial[["Ht"]]))
qqnorm(exp(train_data_initial[["Ht"]]))



# Summarize data				
summary(train_data_initial)		
# Basic Scatterplot Matrix																													
pairs(~Ins_Age + BMI + Ht,data=train_data_initial,main="Simple Scatterplot Matrix")			

# Check for Multocollinearity
cor(cbind( train_data_initial$BMI, train_data_initial$Ht,train_data_initial$Ins_Age))
vif(lm(Response ~ Ins_Age + BMI + Ht, data = train_data_initial))




BP1 <- boxplot(train_data_initial[,categoricalVariables ], main = "boxplot of all categoricalVariables ",notch = TRUE, col = 3:7)
BP2 <- boxplot(train_data_initial[,continuousVariables  ], main = "boxplot of all continuousVariables  ",notch = TRUE, col = 3:7)
BP3 <- boxplot(train_data_initial[,discreteVariables    ], main = "boxplot of all discreteVariables    ",notch = TRUE, col = 3:7)

BP_Ht <- boxplot(train_data_initial$Ht)
BP_BMI <-boxplot(train_data_initial$BMI)
BP_Ins_Age <-boxplot(train_data_initial$Ins_Age)


BP_Ht$out
BP_BMI$out
BP_Ins_Age$out

#####################################  
# 1.2. Data Cleansing
#####################################
# Delete duplicated
unique(train_data_initial)																																
unique(validation)																																		

# convert all categorical (nominal) data into variables /factors
# Replace null with 0 and mark categorical variables as such
# Note that "Product_Info_2"has now become numeric 


# Fill up NA cells with 0
for (f in categoricalVariables) {
  train_data_initial[f] [is.na(train_data_initial[f] )] <- 0																							
  validation[f] [is.na(validation[f] )]                 <- 0 	
# Mark all categorical variables as factors.																						
  train_data_initial[[f]] <- as.factor( train_data_initial[[f]] )	
# Mark all categorical variables as factors. 																					 				
  validation[[f]]         <- as.factor( validation[[f]]  )																								
}

for (f in continuousVariables) {
  train_data_initial[f] [is.na(train_data_initial[f] )] <- 0																							
  validation[f] [is.na(validation[f] )]                 <- 0 																							
}	


# Fill up NA cells with 0
for (f in discreteVariables) {
  train_data_initial[f] [is.na(train_data_initial[f] )] <- 0																							
  validation[f] [is.na(validation[f] )]                 <- 0 																							
}	
	# Dealing with outliers
	# Intsall data.table package and create simple function

	outlierReplace = function(dataframe, cols, rows, newValue = NA) {
		if (any(rows)) {
			set(dataframe, rows, cols, newValue)
		}
	}

	# Identifying and removing outliers from data set !!! Be Careful !!!
	# Replacing outliers with 0
	#outlierReplace(train_data_initial,"BMI", which(train_data_initial["BMI"] > 0.6), NA)



	# Delete Null rows. since we have replaced some outliers with NA, we now need to remove those rows entirely
	nrow(train_data_initial)
	train_data_initial <- na.omit(train_data_initial)
				nrow(train_data_initial)


#Investigate the medical keyword fields, would the number of medical keywords equal to 1 have predictive power?

#Function to sum across rows for variables defined
psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) }
  
#Make a new variable which sums across all of the Medical_Keyword dummy variables on an application
train_data_initial$Number_medical_keywords <- psum(train_data_initial[,c(paste("Medical_Keyword_",1:48,sep=""))])
#table(train_data_initial$Number_medical_keywords)

#####################################  
# 1.3. Split data into train and test sets
#####################################
	
# Split the trainings set into train and test	 
#train_idx <- sample(1:nrow(train_data_initial),nrow(train_data_initial)*0.7,replace=FALSE)
train_idx <- sample(1:nrow(train_data_initial),1000,replace=FALSE)
train_data <- train_data_initial[train_idx,] # select all these rows
test_data <- train_data_initial[-train_idx,] # select all but these rows

				#summary(train_data)
				nrow(train_data)
				nrow(test_data)

#####################################  
# 1.4. Data modelling (Modellanpassung)
#####################################

				##################
				# 1.4.1. Single model
				##################

				# print density distributions
				densities <- dnorm(train_data$BMI, 0,1)
				cumulative<-pnorm(train_data$BMI, 0, 1)
				randomdeviates<-rnorm(1000,0,1)
				 
				plot(train_data$Ins_Age, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, main="PDF of Standard Normal")
				plot(train_data$Ins_Age, cumulative, col="darkorange", xlab="", ylab="Cumulative Probability",type="l",lwd=2, cex=2, main="CDF of Standard Normal", cex.axis=.8)



				# Create a simple OLS regression with the trainings data
				ols1 <- lm(Response ~ Ins_Age + BMI, data = train_data)
				summary(ols1 )
				summary(ols1)$r.squared # summary of R2 
				summary(ols1)$adj.r.squared #summary of R2 adjusted 
				AIC(ols1)

				# Print dynamic 3D plot of all Variables 
				
				open3d()
				s3d <- plot3d(train_data$Ins_Age,train_data$BMI,train_data$Response, type="p", col="red", xlab="Ins_Age", ylab="BMI", zlab="Response", site=5, lwd=15)


				# Print static 3D plot of ols1 with surface

				s3d <- scatterplot3d(x=train_data$Ins_Age, y=train_data$BMI, z=train_data$Response, highlight.3d=TRUE, pch=19)
				s3d$plane3d(ols1, lty="solid")  # add the regression plane from model 1 above


				##################
				# 1.4.2. Multiple models
				##################
				ols2 <- lm(Response ~ Ins_Age + BMI + Ht , data = train_data)
				ols3 <- lm(Response ~ Ins_Age + BMI + Ht + Wt, data = train_data)
				ols4 <- lm(Response ~ Ins_Age + BMI + Ht + Wt + Employment_Info_1, data = train_data)
				ols5 <- lm(Response ~ Ins_Age + BMI + Ht + Wt + Employment_Info_1 +Employment_Info_2, data = train_data)
				anova(ols2, ols3, ols4, ols5)


				##################
				# 1.4.3. All models
				##################
				olsAll <- lm(Response ~ Number_medical_keywords + Product_Info_1 + Product_Info_2 + Product_Info_3 + Product_Info_5 + Product_Info_6 + Product_Info_7 + Employment_Info_2 + Employment_Info_3 + Employment_Info_5 + InsuredInfo_1 + InsuredInfo_2 + InsuredInfo_3 + InsuredInfo_4 + InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + Insurance_History_1 + Insurance_History_2 + Insurance_History_3 + Insurance_History_4 + Insurance_History_7 + Insurance_History_8 + Insurance_History_9 + Family_Hist_1 + Medical_History_2 + Medical_History_3 + Medical_History_4 + Medical_History_5 + Medical_History_6 + Medical_History_7 + Medical_History_8 + Medical_History_9 + Medical_History_11 + Medical_History_12 + Medical_History_13 + Medical_History_14 + Medical_History_16 + Medical_History_17 + Medical_History_18 + Medical_History_19 + Medical_History_20 + Medical_History_21 + Medical_History_22 + Medical_History_23 + Medical_History_25 + Medical_History_26 + Medical_History_27 + Medical_History_28 + Medical_History_29 + Medical_History_30 + Medical_History_31 + Medical_History_33 + Medical_History_34 + Medical_History_35 + Medical_History_36 + Medical_History_37 + Medical_History_38 + Medical_History_39 + Medical_History_40 + Medical_History_41 +Product_Info_4 + Ins_Age + Ht + Wt + BMI + Employment_Info_1 + Employment_Info_4 + Employment_Info_6 + Insurance_History_5 + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5 +Medical_History_1 + Medical_History_10 + Medical_History_15 + Medical_History_24 , data = train_data)

				# Compare models using step-wise AIC method
				step <- stepAIC(olsAll , direction="both")


				
#####################################  
# 1.5. Model validation (Modellvergleich)
#####################################

#step <- stepAIC(olsAll , direction="both",trace=0,steps = 1)
step$anova # display results

# Test residuals for heterogeneity
plot(fitted(ols1), residuals(ols1), xlab="Predicted scores", ylab="Residuals")

# Test normal distribution of residuals
hist(residuals(ols1))

# test your final model 
olsFinalTest <- lm(Response ~ InsuredInfo_2 + InsuredInfo_5 + InsuredInfo_6 + Insurance_History_1 + 
    Insurance_History_3 + Insurance_History_4 + Family_Hist_1 + 
    Medical_History_4 + Medical_History_7 + Medical_History_11 + 
    Medical_History_13 + Medical_History_14 + Medical_History_20 + 
    Medical_History_22 + Medical_History_23 + Medical_History_30 + 
    Medical_History_31 + Medical_History_35 + Medical_History_39 + 
    Medical_History_40 + Product_Info_4 + Ins_Age + Ht + Wt + 
    Family_Hist_2 + Family_Hist_4 + Medical_History_1 + Medical_History_15
	, data = test_data)
	
olsFinalTrain <- lm(Response ~ InsuredInfo_2 + InsuredInfo_5 + InsuredInfo_6 + Insurance_History_1 + 
    Insurance_History_3 + Insurance_History_4 + Family_Hist_1 + 
    Medical_History_4 + Medical_History_7 + Medical_History_11 + 
    Medical_History_13 + Medical_History_14 + Medical_History_20 + 
    Medical_History_22 + Medical_History_23 + Medical_History_30 + 
    Medical_History_31 + Medical_History_35 + Medical_History_39 + 
    Medical_History_40 + Product_Info_4 + Ins_Age + Ht + Wt + 
    Family_Hist_2 + Family_Hist_4 + Medical_History_1 + Medical_History_15
	, data = train_data)

summary(olsFinalTest )$adj.r.squared #summary of R2 adjusted
summary(olsFinalTrain)$adj.r.squared #summary of R2 adjusted

#####################################  
# 1.6. Prediction
#####################################
# Predict data 
# Select Variable ID from validation set
ID <- validation[1] 	
# Predict Response variable for validation set													
Response <- c(predict(olsFinalTest , validation)) 
# Create a new data frame with both columns							 
prediction_data <- data.frame(ID, Response)	
# Use built-in R functions to sort descending								
prediction_data <- prediction_data[ order(-prediction_data$Response), ] 	
# Summarize data
summary(prediction_data)													


