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

# loadData ----------------------------------------------------------------
## Load the data into R
data.raw <- read.csv("2013_data_full.csv", stringsAsFactors=TRUE, na.strings = c("NA","N/A","U",""," ","None","Unknown","Unkn\nown","Unkno\nwn","Unknow\nn"))
names.data <- c("Victim.Age","Victim.Race","Victim.Gender","Relationship", "Harm",
                "Location","Region","RSO","Offender.Age","Offender.Race",
                "Offender.Gender","Rural.City","Missing.Location",
                "Recovery.Location", "Recovery.Status", "Motive")

colnames(data.raw) <- names.data


# Remove lines where victim info and relationship is not provided
data <- data.raw[complete.cases(data.raw[,c(1:4)]),]

# Remove all carriage returns
for (i in 1:length(data))
{
  if (class(data[,i]) == "factor") levels(data[,i]) <- gsub("\n","", levels(data[,i]))
}
summary(data)

# targetVariable ----------------------------------------------------------
# Create a list of harm responses and identify them as 1/0
harm.list <- as.data.frame(unique(unlist(data$Harm), use.names=FALSE))
colnames(harm.list) <- c("Harm")
harm.list$target <- c(0,0,1,1,1,0,1)
data <- merge(data, harm.list, by = "Harm")
table(data$target)

w <- which(data$target==1 | (data$Motive == "Sexual") | (is.na(data$Motive) & data$Recovery.Status == "Deceased") | !(data$Motive == "False Allegation" | data$Motive == "Runaway") & 
  data$Recovery.Status == "Deceased")
data$target[w] = 1
table(data$target)

# Count NA by variable and isolate fields with at least 1 NA
data.empty = NULL
data.empty.perc = NULL
for (i in 1:length(data))
{
  data.empty[i] <- length(which(is.na(data[i])))
  data.empty.perc[i] <- round(length(which(is.na(data[i])))/nrow(data)*100,1) 
}
x <- as.data.frame(cbind(colnames(data), data.empty, data.empty.perc)[order(-data.empty),])
x <- x[which(x$data.empty != 0),]
x

# Remove mostly NA
data <- data[,-which(names(data) %in% x[1:4,1])]

# Fill others
data <- knnImputation(data, k=5)
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
                              breaks=c(0,20,30,40,50,Inf), 
                              labels=c('<20','20-30','30-40','40-50','>50'))

table(data$Offender.AgeGroup)


relate.list <- as.data.frame(unique(unlist(data$Relationship), use.names=FALSE))
colnames(relate.list) <- c("Relationship")
relate.list$Relate.Group <- as.factor(c("Family Friend","Parent","Family Friend", "Parent", "Relative", 
                                        "Family Friend","Relative", "Known","Relative","Parent",
                                        "Known","Known", "Other", "Known", "Stranger", 
                                        "Relative", "Known","Known", "Family Friend", "Other", 
                                        "Known", "Known", "Other", "Family Friend"))
                                        
data <- merge(data, relate.list, by = "Relationship")
table(data$Relate.Group)

#colnames(data)

cor.data <- data[,c("Victim.Age","Victim.Race","Victim.Gender","Region","Offender.Age","Offender.Race",
                    "Offender.Gender","Rural.City", "Relate.Group", "target")]
for (i in 1:length(cor.data))
{
  cor.data[,i] <- as.numeric(cor.data[,i])
}
c <- cor(cor.data)
corrplot(c)

# modelBuildAndEvaluate --------------------------------------------------------------
# Remove extra attributes and those not known at the time of abduction
data.m <- data[,c("Victim.AgeGroup","Victim.Race","Victim.Gender","Region","Offender.AgeGroup","Offender.Race",
                  "Offender.Gender","Rural.City", "Relate.Group", "target")]
data.m$target <- as.factor(data.m$target)
data.smote <- SMOTE(target ~ ., data.m, perc.over = 500)
data.smote2 <- SMOTE(target ~ ., data.m, perc.over = 500)
data.smote3 <- SMOTE(target ~ ., data.m, perc.over = 500)
data.smote4 <- SMOTE(target ~ ., data.m, perc.over = 500, perc.under = 150)
smote.output <- as.data.frame(rbind(table(data.m$target),table(data.smote$target),table(data.smote4$target)))
rownames(smote.output) <- c("original data","upsampling","upsampling/downsampling")
smote.output

# Build and evaluation for SVM, NaiveBayes, RandomForest
library(adabag)
library(ada)
res <- performanceEstimation(
  c(PredTask(target ~ ., data.m),PredTask(target ~ ., data.smote),PredTask(target ~ ., data.smote2),PredTask(target ~ ., data.smote3),
    PredTask(target ~ ., data.smote4)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(1,10,100), gamma=c(0.1,0.01)),
                     evaluator.pars=list(stats=c("rec","prec", "F"))),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(5,50,200), nodesize = c(2,5)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"))),
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F"))),
    workflowVariants("standardWF", learner = "ada", learner.pars=list(iter=c(10,50)), 
                     evaluator.pars=list(stats=c("rec","prec", "F")))),
  BootSettings(type=".632", nReps=100))
plot(res)
topPerformers(res)

model.svm <- svm(target ~ ., data=data.smote2, cost=10, gamma=0.1)
pred.svm <- predict(model.svm, data.smote)
classificationMetrics(data.smote$target,pred.svm,stats=c("rec","prec","F"))
table(data.smote$target,pred.svm)

model.svm <- svm(target ~ ., data=data.m, cost=10, gamma=0.1)
pred.svm <- predict(model.svm, data.m)
classificationMetrics(data.m$target,pred.svm,stats=c("rec","prec","F"))
table(data.m$target,pred.svm)

model.nb <- naiveBayes(target ~ ., data.smote2)
pred.nb <- predict(model.nb, data.smote)
table(data.smote$target, pred.nb)

model.tree <- rpart(target ~ ., data=data.smote2, method="class", control = rpart.control(minsplit = 20, minbucket = 10))
pred.tree <- predict(model.tree, data.smote2, type="class")
prettyTree(model.tree, cex=0.75, margin=0.05, compress=TRUE, fheight=1)
printcp(model.tree)
classificationMetrics(data.smote2$target,pred.tree,stats=c("rec","prec","F"))
table(data.smote2$target,pred.tree)

model.tree <- rpart(target ~ ., data=data.m, method="class", control = rpart.control(minsplit = 5, minbucket = 2))
pred.tree <- predict(model.tree, data.m, type="class")
prettyTree(model.tree, cex=0.75, margin=0.05, compress=TRUE, fheight=1)
printcp(model.tree)
classificationMetrics(data.m$target,pred.tree,stats=c("rec","prec","F"))
table(data.m$target,pred.tree)
