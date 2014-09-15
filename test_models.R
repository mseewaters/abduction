library(DMwR)
library(ggplot2)
library(e1071)
library(performanceEstimation)


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
data.raw <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/Sample_data.csv", stringsAsFactors=TRUE, na.strings = c("NA",""," "))
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
# •	Decision trees (rpart)  #Removed randomForest due to size of data set
# •	NaïveBayes
# We will evaluate methods for up sampling.  
#   Recommendations to use
# The models will be evaluated based on Precision and Recall, 
#     with Recall having a higher weight 



# modelBuild --------------------------------------------------------------
####  CREATE MODELS HERE

# Remove Harm (text), Date, Location (17 factors for 20 incidents), Recovery (not known at time)
data.m <- data[,-which(names(data) %in% c("Harm","Date","Location", "Recovery","target2"))]
data.m2 <- data[,-which(names(data) %in% c("Harm","Date","Location", "Recovery","target"))]

model.tree <- rpartXse (target ~ ., data=data.m)
pred.tree <- predict(model.tree, data.m)

model.svm <- svm(target ~ ., data=data.m)
pred.svm <- predict(model.svm, data.m)

model.nb <- naiveBayes(target2 ~ ., data.m2)
data.ms.test <- data.m2[,1:13]
pred.nb <- predict(model.nb, data.ms.test, type="raw")

model.glm <- glm(target ~ ., data.m, family = binomial(link = "logit"))
pred.glm <- predict(model.glm, data.m)

res.test <- performanceEstimation(
  PredTask(target ~ ., data.m),
  workflowVariants("standardWF", learner = "naiveBayes", learner.pars=list(type="raw")),
  CvSettings(nReps =1, nFolds = 10))
summary(res.test)
plot(res.test)


res <- performanceEstimation(
  PredTask(target ~ ., data.m),
  c(workflowVariants("standardWF", learner = "svm",
           learner.pars=list(cost=c(1,10), gamma=c(0.1,0.01))),
    workflowVariants("standardWF", learner = "rpartXse",
                     learner.pars=list(se=c(0,1)))),
  CvSettings(nReps = 2, nFolds = 10))
summary(res)
plot(res)

# modelEvaluate -----------------------------------------------------------
#### EVALUATE MODELS HERE
