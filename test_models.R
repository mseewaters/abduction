library(DMwR)
library(ggplot2)
library(e1071)
library(performanceEstimation)
library(randomForest)
library(rpart)


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
data.raw <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/Sample_data.csv", stringsAsFactors=TRUE, na.strings = c("NA","U",""," "))
names.data <- c("Date","Victim.Age","Victim.Race","Victim.Gender","Harm","Publicized","Location","Region",
                "Relationship","RSO","Offender.Age","Offender.Race","Offender.Gender","Rural.City","Missing.Location",
                "Recovery.Location", "Recovery", "Number.Victims")
colnames(data.raw) <- names.data
summary(data.raw)


# missingData -------------------------------------------------------------
## Evaluate missing data and determine best method to handle

# Remove lines where victim info and relationship is not provided
data <- data.raw[complete.cases(data.raw[,c(2,3,4,5,9)]),]

# Count NA by variable and isolate fields with at least 1 NA
data.empty = NULL
for (i in 1:length(data))
{
  data.empty[i] <- length(which(is.na(data[i])))  
}
x <- as.data.frame(cbind(names.data, data.empty)[order(-data.empty),])
x <- x[which(x$data.empty != 0),]
x

# Remove Recovery.Location and Missing.Location
# Almost half of observations missing data
data <- data[,-which(names(data) %in% c("Recovery.Location","Missing.Location"))]

# Fill RSO and Rural.City using nearest neighbors
data <- knnImputation(data, k=3)
check <- nrow(data[!complete.cases(data),])

summary(data)

# additionalVariables -----------------------------------------------------
## Create additional variable groups as needed

# Create victim age group
data$Victim.AgeGroup <- cut(data$Victim.Age, 
                            breaks=c(-0.5,5.5,10.5,Inf), 
                            labels=c('0-5','6-10','11-15'))

table(data$Victim.AgeGroup)

data$Offender.AgeGroup <- cut(data$Offender.Age,
                            breaks=c(20,30,40,50,Inf), 
                            labels=c('<30','30-40','40-50','>50'))

table(data$Offender.AgeGroup)


relate.list <- as.data.frame(unique(unlist(data$Relationship), use.names=FALSE))
colnames(relate.list) <- c("Relationship")
relate.list$Relate.Group <- as.factor(c("Family Friend", "Relative", "Parent","Parent","Family Friend","Family Friend","Family Friend","Family Friend","Relative","Parent"))
data <- merge(data, relate.list, by = "Relationship")
table(data$Relate.Group)


# targetVariable ----------------------------------------------------------
# Create a list of harm responses and identify them as 1/0
harm.list <- as.data.frame(unique(unlist(data$Harm), use.names=FALSE))
colnames(harm.list) <- c("Harm")
harm.list$target <- c(0,1,1,1,0,1)
harm.list$target2 <- c("NoThreat","Threat","Threat","Threat","NoThreat","Threat")
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
# •	Decision trees (rpart, randomForest)
# •	NaïveBayes
# We will evaluate methods for up sampling.  
#   Recommendations to use bootstrapping
# The models will be evaluated based on Precision and Recall, 
#     with Recall having a higher weight 



# modelEvaluate --------------------------------------------------------------

# Remove extra attributes and those not known at the time of abduction
data.m <- data[,-which(names(data) %in% c("RSO", "Harm","Date","Victim.Age","Offender.Age", "Publicized", "Relationship", "Location", "Recovery","Number.Victims","target2"))]

# Bug in SMOTE, must be factor and last item in dataframe
data.m$target <- as.factor(data.m$target)
data.smote <- SMOTE(target ~ ., data.m, perc.over = 200)
table(data.smote$target)
write.csv(data.smote, file = 'data.smote.csv')

# rpart requires non factored target
data.smote.nf <- data.smote
data.smote.nf$target <- as.numeric(data.smote.nf$target)


# Build and evaluation for SVM, NaiveBayes, RandomForest
res <- performanceEstimation(
  c(PredTask(target ~ ., data.m),PredTask(target ~ ., data.smote)),
  c(workflowVariants("standardWF", learner = "svm",
           learner.pars=list(cost=c(1,10), gamma=c(0.1,0.01))),
    workflowVariants("standardWF", learner = "randomForest"),
    workflowVariants("standardWF", learner = "naiveBayes")),
  CvSettings(nReps = 3, nFolds = 4))
plot(res)

# Build and evaluation for rpart (requires non factored target)
res2 <- performanceEstimation(
  PredTask(target ~ ., data.smote.nf),
  workflowVariants("standardWF", learner = "rpart"),
  CvSettings(nReps = 3, nFolds = 4))
plot(res2)

topPerformers(res)
topPerformers(res2)


# modelFinalBuild -----------------------------------------------------------

# Best model seems to be Naive Bayes

model.nb <- naiveBayes(target ~ ., data.smote)
pred.nb <- predict(model.nb, data.smote)
table(pred.nb, data.smote$target)
model.nb$tables

# Conclusions from evidence...

library("RWeka")
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes") 
model.weka <- NB(target ~ ., data.smote)
infogain <- as.data.frame(InfoGainAttributeEval(target ~ ., data.smote))
ig <- cbind(rownames(infogain),infogain)
colnames(ig) <- c("attribute","gain")

ig$attribute <- factor(ig$attribute, levels = ig[order(-ig$gain),]$attribute)
ggplot(data=ig, aes(x=attribute, y=gain, fill=-gain)) + geom_bar(stat="identity")
