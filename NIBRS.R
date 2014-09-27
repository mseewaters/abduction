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

data.raw <- read.csv("D:/0 Stern MSBA/0.2 Capstone/DS0001/data_final.csv", 
                       header=FALSE, na.strings=c("NA"))

names.data <- c('STATE', 'ORI', 'INCNUM', 'INCDATE', 'B1006', 
                    'B1007', 'B1009', 'B1010', 'B1011', 'B2005', 'B2006', 
                    'B2009', 'B2010', 'V1006', 'V1007', 'V20061', 'V20062', 
                    'V20063', 'V20071', 'V20072', 'V20073', 'V20111', 'V20171', 
                    'V20172', 'V40181', 'V40182', 'V40183', 'V40191', 'V40192', 
                    'V40193', 'V40201', 'V40202', 'V40203', 'V40221', 'V40222', 
                    'V40223', 'V40261', 'V40271', 'V40281', 'V40311', 'V40312', 
                    'V40313', 'V40321', 'V40322', 'V40323', 'V40331', 'V40332', 
                    'V40333', 'V40341', 'V40342', 'V40343', 'V50071', 'V50072', 
                    'V50081', 'V50082', 'V50091', 'V50092')

colnames(data.raw) <- names.data


# Filter for all victims age <= 15 and Family Relationship (NIBRS definition)
data <- data.raw
m <- data[,c("V40181","V40182","V40183")]
data$agemax <- apply(m,1,max,na.rm=TRUE)
  
data.c <- subset(data, agemax <= 15 & V50071 >= 21 & V40321 %in% c(5,7,10,12,14,15,16,17,19), na.rm=TRUE)
table(data.c$V40181)
table(data.c$V40321)

# Set up target variable
data.c$target <- ifelse((data.c$V40261>1 | (data.c$V20171>0 & data.c$V20171<400) | (data.c$V20062>0 & data.c$V20062<110)), 1, 0)
sum(data.c$target)



# Reclass variety of unknowns to NA
data <- as.data.frame(lapply(data.c, function(x){replace(x ,x < 0, NA)}))                   

# Count NA by variable and isolate fields with at least 1 NA
data.empty = NULL
for (i in 1:length(data))
{
  data.empty[i] <- length(which(is.na(data[i])))  
}
x <- as.data.frame(cbind(names.data, data.empty)[order(-data.empty),])

x <- x[which(x$data.empty != 0),]
x

# Add in groupings
data$VNumVG <- as.factor(ifelse(!is.na(data$V40182), "Multiple Victims", "Single Victim"))
data$VNumOG <- as.factor(ifelse(!is.na(data$V40331), "Multiple Victims", "Single Victim"))

data.t <- data[,-which(names(data) %in% x[1:31,1])]

data.t$V20111G <- as.factor(ifelse(data.t$V20111 == 20, "Home", ifelse((data.t$V20111 == 44 | data.t$V20111 == 53 | data.t$V20111 == 57), "School","Other")))

data.t$V40181G <- cut(data.t$V40181, 
                            breaks=c(-0.5,5.5,10.5,Inf), 
                            labels=c('0-5','6-10','11-15'))

table(data.t$V40181G)

data.t$V50071G <- cut(data.t$V50071,
                              breaks=c(0,20,30,40,50,Inf), 
                              labels=c('<20','20-30','30-40','40-50','>50'))

table(data.t$V50071G)

data.t$B2005G <- cut(data.t$B2005,
                     quantile(data.t$B2005,probs=seq(0,1,0.25)), 
                      labels=c('Pop1','Pop2','Pop3','Pop4'))

table(data.t$B2005G)

data.t$V40321G <- cut(data.t$V40321,
                      breaks=c(0,6,13,Inf), 
                      labels=c('Parent','Family','Known'))

table(data.t$V40321G)

data.t$V1007G <- cut(data.t$V1007,
                      breaks=c(-1,1,6,12,18,Inf), 
                      labels=c('Night1','Early','Morning','Afternoon','Night2'))

table(data.t$V1007G)
xtabs(target~V1007, data.t)

check <- nrow(data[!complete.cases(data.t),])

data.t <- centralImputation(data.t)
check <- nrow(data[!complete.cases(data.t),])


library(corrplot)

#data.m <- data.c[,c("V40181","V40191","V40201","B1011","V50071","V50081","V50091","V40261","V20171", "V20062")]
# Correlations - factors converted to numeric
cor.data <- data.t[,c("V40181G","V40191","V40201","V50071G","V50081","V50091","B1010", 
                    "B2005G","V1007G","V20071", "V20111G", "V40321G", "VNumVG","VNumOG","target")]
for (i in 1:length(cor.data))
{
  cor.data[,i] <- as.numeric(cor.data[,i])
}
c <- cor(cor.data)
corrplot(c)

colnames(data.t)
data.m <- data.t[,c("V40181G","V40191","V40201","V50071G","V50081","V50091","B1010", 
                    "B2005G","V1007G","V20071", "V20111G", "V40321G", "VNumVG","VNumOG","target")]
data.m$target <- as.factor(data.m$target)
data.m$B1010 <- as.factor(data.m$B1010)
data.m$V20071 <- as.factor(data.m$V20071)
data.m$V40191 <- as.factor(data.m$V40191)
data.m$V40201 <- as.factor(data.m$V40201)
data.m$V50081 <- as.factor(data.m$V50081)
data.m$V50091 <- as.factor(data.m$V50091)


data.smote <- SMOTE(target ~ ., data.m, perc.over = 200)
data.smote2 <- SMOTE(target ~ ., data.m, perc.over = 500)
smote.output <- as.data.frame(rbind(table(data.m$target),table(data.smote$target),table(data.smote2$target)))
rownames(smote.output) <- c("original data","upsampling","upsampling/downsampling")
smote.output

# rpart requires non factored target
data.m.nf <- data.m
data.m.nf$target <- as.numeric(data.m.nf$target)
data.smote.nf <- data.smote
data.smote.nf$target <- as.numeric(data.smote.nf$target)
data.smote2.nf <- data.smote2
data.smote2.nf$target <- as.numeric(data.smote2.nf$target)



library("RWeka")
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes") 

model.weka <- NB(target ~ ., data.smote)
infogain <- as.data.frame(InfoGainAttributeEval(target ~ ., data.smote))
ig <- cbind(rownames(infogain),infogain)
colnames(ig) <- c("attribute","gain")
summary(model.weka)
ig$attribute <- factor(ig$attribute, levels = ig[order(-ig$gain),]$attribute)
ggplot(data=ig, aes(x=attribute, y=gain, fill=gain)) + geom_bar(stat="identity")


res <- performanceEstimation(
  c(PredTask(target ~ ., data.m),PredTask(target ~ ., data.smote),
    PredTask(target ~ ., data.smote2)),
  c(workflowVariants("standardWF", learner = "svm",
                     learner.pars=list(cost=c(1,10,100), gamma=c(0.1,0.01)),
                      evaluator.pars=list(stats=c("rec","prec", "F"))),
    workflowVariants("standardWF", learner = "randomForest",
                     learner.pars=list(ntree = c(5,50,200), nodesize = c(2,5)), 
                     evaluator.pars=list(stats=c("rec","prec", "F"))),
    workflowVariants("standardWF", learner = "naiveBayes", evaluator.pars=list(stats=c("rec","prec","F")))),
  CvSettings(nReps =1, nFolds = 10))

plot(res)


# Build and evaluation for rpart (requires non factored target)
res.nf <- performanceEstimation(
  c(PredTask(target ~ ., data.m),PredTask(target ~ ., data.smote),
    PredTask(target ~ ., data.smote2)),
  workflowVariants("standardWF", learner = "rpart", evaluator.pars=list(stats=c("err"))),
  CvSettings(nReps =1, nFolds = 10))

plot(res.nf)

library(adabag)
library(ada)
# Build and evaluation for adaboost
res.ada <- performanceEstimation(
  c(PredTask(target ~ ., data.m),PredTask(target ~ ., data.smote),
    PredTask(target ~ ., data.smote2)),
  workflowVariants("standardWF", learner = "ada", learner.pars=list(iter=c(10,50)), 
                   evaluator.pars=list(stats=c("rec","prec", "F"))),
  CvSettings(nReps =1, nFolds = 10))


HldSettings(nReps = 5, hldSz=0.25))

plot(res.ada)


# Some test models
model.tree <- rpart(target ~ ., data=data.smote, method="class", control = rpart.control(minsplit = 50, minbucket = 10))
pred.tree <- predict(model.tree, data.m, type="class")
prettyTree(model.tree, cex=0.75, margin=0.05, compress=TRUE, fheight=1)
printcp(model.tree)
classificationMetrics(data.m$target,pred.tree,stats=c("rec","prec","F"))

idx <- sample(1:nrow(data.smote2), as.integer(0.75*nrow(data.smote2)))
train <- data.smote2[idx,]
test <- data.smote2[-idx,]

model.ada <- boosting(target ~ ., data=data.smote, mfinal=10)
pred.ada <- predict(model.ada, data.m)
classificationMetrics(data.m$target,pred.ada$class,stats=c("rec","prec","F"))
pred.ada$confusion
