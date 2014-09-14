

# Data Understanding/Preparation
# Using the existing data set, we will
# • Load the data into R 
# • Evaluate the missing fields and determine the best methods to handle (remove or fill with similar)
# • Create the target variable (currently free-form text)
# •	Investigate the data to look for correlations, outliers, etc that may yield insights
# •	Plot the data geographically.  (This is not to identify states as risk factor, rather to understand the nature of the data.)
# •	Plot relevant distributions (i.e. age of victim, age of offender)


# loadData ----------------------------------------------------------------
## Load the data into R
data <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/Sample_data.csv", stringsAsFactors=TRUE, na.strings = c("NA",""," "))
names.data <- c("Date","Victim.Age","Victim.Race","Victim.Gender","Harm","Publicized","Location","Region",
                "Relationship","RSO","Offender.Age","Offender.Race","Offender.Gender","Rural.City","Missing.Location",
                "Recovery.Location", "Recovery", "Number.Victims")
colnames(data) <- names.data
summary(data)


# missingData -------------------------------------------------------------
## Evaluate missing data and determine best method to handle

# Remove lines where victim info and relationship is not provided
data <- data[complete.cases(data[,c(2,3,4,5,9)]),]

# Count NA by variable and isolate fields with at least 1 NA
data.empty = NULL
for (i in 1:length(data))
{
  data.empty[i] <- length(which(is.na(data[i])))  
}
x <- as.data.frame(cbind(names.data, data.empty)[order(-data.empty),])
x <- x[which(x$data.empty != 0),]


# additionalVariables -----------------------------------------------------
## Create additional variable groups as needed

# Create victim age group
data$Victim.AgeGroup <- cut(data$Victim.Age, 
                            breaks=c(-0.5,5.5,10.5,Inf), 
                            labels=c('0-5','6-10','11-15'))

table(data$Victim.AgeGroup)


# targetVariable ----------------------------------------------------------
# Create a list of harm responses and identify them as 1/0
harm.list <- as.data.frame(unique(unlist(data$Harm), use.names=FALSE))
colnames(harm.list) <- c("Harm")
harm.list$target <- c(0,1,1,1,0,1)
data <- merge(data, harm.list, by = "Harm")
table(data$target)


# investigateData ---------------------------------------------------------
#### INVESTIGATE DATA HERE



# geographicPlot ----------------------------------------------------------
####  PLOT DATA GEOGRAPHICALLY



# distributionsPlot -------------------------------------------------------
####  PLOT DISTRIBUTIONS
##



# We will use the performanceEstimation package and evaluate several 
#     common classification models
# • SVM
# •	Decision trees (rpart)  #Removed randomForest due to size of data set
# •	NaïveBayes
# We will evaluate methods for up sampling.  
#   Recommendations to use
# The models will be evaluated based on Precision and Recall, 
#     with Recall having a higher weight 



# modelBuild --------------------------------------------------------------
####  CREATE MODELS HERE



# modelEvaluate -----------------------------------------------------------
#### EVALUATE MODELS HERE
