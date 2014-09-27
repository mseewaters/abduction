library(DMwR)
library(ggplot2)
library(e1071)
library(performanceEstimation)
library(randomForest)
library(rpart)
library(plyr)
library(corrplot)
library(ggmap)
library(gridExtra)

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
data.raw <- read.csv("Sample_data.csv", stringsAsFactors=TRUE, na.strings = c("NA","U",""," "))
names.data <- c("Date","Victim.Age","Victim.Race","Victim.Gender","Harm","Publicized","Location","Region",
                "Relationship","RSO","Offender.Age","Offender.Race","Offender.Gender","Rural.City","Missing.Location",
                "Recovery.Location", "Recovery", "Number.Victims")
colnames(data.raw) <- names.data
summary(data.raw)

# Remove all carriage returns
for (i in 1:length(data))
{
  if (class(data[,i]) == "factor") levels(data[,i]) <- gsub("\n","", levels(data[,i]))
}

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

# Fill RSO, Offender.Race, Offender.Age, Rural.City using nearest neighbors
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
data <- merge(data, harm.list, by = "Harm")
table(data$target)


# investigateData ---------------------------------------------------------
# Correlations - factors converted to numeric
cor.data <- data[,c(4:7,11:14,9,19,20)]
for (i in 1:length(cor.data))
{
  cor.data[,i] <- as.numeric(cor.data[,i])
}
c <- cor(cor.data)
corrplot(c)

# detailed bar charts of interesting correlations
p1 <- ggplot(data, aes(x=Rural.City, fill=Victim.Gender)) + geom_bar(stat='bin') + ggtitle("Victim Gender and Rural/City")
p2 <- ggplot(data, aes(x=Offender.Race, fill=Victim.Race)) + geom_bar(stat='bin') + ggtitle("Victim and Offender Race")
p3 <- ggplot(data, aes(x=Offender.Age, fill=Publicized)) + geom_bar(stat='bin') + ggtitle("Offender Age and Publicized")
p4 <- ggplot(data, aes(x=Relate.Group, fill=Offender.Gender)) + geom_bar(stat='bin') + ggtitle("Relationship and Offender Gender")
grid.arrange(p1,p2,p3,p4,ncol=2)

#Factors with insuffiencient representative data
p1 <- ggplot(data, aes(x=Recovery, fill=Recovery)) + geom_bar(stat="bin")
p2 <- ggplot(data, aes(x=RSO, fill=RSO)) + geom_bar(stat="bin")
grid.arrange(p1,p2,ncol=2)

# geographicPlot ----------------------------------------------------------
library(ggmap)
loc <- data.frame()
all_locs <- paste0(as.character(data$Location),", US")
unique_locs <- unique(all_locs)
for (i in 1:length(unique_locs))
{
  geo.res <- geocode(unique_locs[i], messaging = FALSE)
  loc[i,1] <- geo.res[1]
  loc[i,2] <- geo.res[2]
  loc[i,3] <- length(all_locs[all_locs==unique_locs[i]])
}

map <- get_map("united states", zoom = 4)
ggmap(map, extent="device") + geom_point(aes(x=lon, y=lat), 
              data=loc, color="darkred", size=3+3*loc$V3)


# distributionsPlot -------------------------------------------------------

#The father appears to be the highest abduction rate, however statically its the mother
ggplot(data, aes(x=Victim.Age, y=Relationship)) + geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)

#Higher counts in the Souteast and South Central and again the father appears to be the abductor
ggplot(data, aes(x=Relationship)) + geom_histogram(binwidth=.5, colour="black", fill="blue") + 
  facet_grid(Region ~ .)


#Density Plots of age of Victim and Offender with Mean line
ggplot(data, aes(x=Victim.Age)) + geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Victim.Age, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

ggplot(data, aes(x=Offender.Age)) + geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Victim.Age, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

#Our data show generally no harm is done to the child
ggplot(data, aes(x=Harm, fill=Victim.Gender)) + geom_histogram(binwidth=.5, position="dodge")

#Interesting for the most part children are recovered, girls are the only missing in our data
ggplot(data, aes(x=target, fill=Victim.Gender)) + geom_histogram(binwidth=.5, position="dodge")

#The majority of cases are pubicized with the highest being in the South Central Region
ggplot(data, aes(x=Publicized, fill=Region)) + geom_histogram(binwidth=.5, position="dodge")

#Southeast and West appear to have the highest rates of abduction
ggplot(data, aes(x=Victim.Age, fill=Region)) + geom_histogram(binwidth=.5, position="dodge")

#Switching the axis South Central appears to have the highest overall counts
ggplot(data, aes(x=Region, fill=Victim.Age)) + geom_histogram(binwidth=.5, position="dodge")


#Whites and Blacks dominate abduction cases with whites dominating in Southeast, Central and
#blacks dominating in South Central and West, hispanic abduction appears rare in our data from
#my additional research with DCFS this is due to non reporting and immigration status
ggplot(data, aes(x=Region, fill=Victim.Race)) + geom_histogram(binwidth=.5, position="dodge")




# We will use the performanceEstimation package and evaluate several 
#     common classification models
# • SVM
# •	Decision trees (rpart, randomForest)
# •	NaïveBayes
# We will evaluate methods for up sampling.  
##  Recommendation to use bootstrapping for sampling
# The models will be evaluated based on Precision and Recall, 
#     with Recall having a higher weight 


# modelBuildAndEvaluate --------------------------------------------------------------

# Remove extra attributes and those not known at the time of abduction
data.m <- data[,-which(names(data) %in% c("Publicized", "RSO", "Harm","Date","Victim.Age","Offender.Age", "Relationship", "Location", "Recovery","Number.Victims"))]

# In SMOTE, target must be factor and last item in dataframe
data.m$target <- as.factor(data.m$target)
data.smote <- SMOTE(target ~ ., data.m, perc.over = 500)
data.smote2 <- SMOTE(target ~ ., data.m, perc.over = 500, perc.under = 150)
smote.output <- as.data.frame(rbind(table(data.m$target),table(data.smote$target),table(data.smote2$target)))
rownames(smote.output) <- c("original data","upsampling","upsampling/downsampling")

# rpart requires non factored target
data.m.nf <- data.m
data.m.nf$target <- as.numeric(data.m.nf$target)
data.smote.nf <- data.smote
data.smote.nf$target <- as.numeric(data.smote.nf$target)
data.smote2.nf <- data.smote2
data.smote2.nf$target <- as.numeric(data.smote2.nf$target)


# Build and evaluation for SVM, NaiveBayes, RandomForest
res <- performanceEstimation(
  c(PredTask(target ~ ., data.m),PredTask(target ~ ., data.smote),
    PredTask(target ~ ., data.smote2)),
  c(workflowVariants("standardWF", learner = "svm",
           learner.pars=list(cost=c(1,10,100), gamma=c(0.1,0.01))),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(5,50,200), nodesize = c(2,5))),
    workflowVariants("standardWF", learner = "naiveBayes")),
  BootSettings(type=".632", nReps=200))


# Build and evaluation for rpart (requires non factored target)
res.nf <- performanceEstimation(
  c(PredTask(target ~ ., data.m.nf),PredTask(target ~ ., data.smote.nf),
    PredTask(target ~ ., data.smote2.nf)),
  workflowVariants("standardWF", learner = "rpartXse", learner.pars=list(se=c(0,0.5,1))),
  BootSettings(type=".632", nReps=200))

#Show results
plot(res)
plot(res.nf)

# Show best model
topPerformers(res)
topPerformers(res.nf)

# Generally svm, randomForest, or naiveBayes has lowest error
getWorkflow("svm.v6", res)

# modelFinalBuild -----------------------------------------------------------

# Best model seems to be randomForest 
# although NaiveBayes sometimes rises to the top
# Will evaluate all models on additional data
# Commentary on need to understand factors
# Evaluation of information gain using naiveBayes

library("RWeka")
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes") 
model.weka <- NB(target ~ ., data.smote2)
infogain <- as.data.frame(InfoGainAttributeEval(target ~ ., data.smote2))
ig <- cbind(rownames(infogain),infogain)
colnames(ig) <- c("attribute","gain")
summary(model.weka)
ig$attribute <- factor(ig$attribute, levels = ig[order(-ig$gain),]$attribute)
ggplot(data=ig, aes(x=attribute, y=gain, fill=gain)) + geom_bar(stat="identity")


# Include comments from Ashli-Jade and our preliminary findings