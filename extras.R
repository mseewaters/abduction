data <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/Sample_data.csv", stringsAsFactors=FALSE)

names.data <- c("Date","Victim.Age","Victim.Race","Victim.Gender","Harm","Publicized","Location","Region",
                "Relationship","RSO","Offender.Age","Offender.Race","Offender.Gender","Rural.City","Missing.Location",
                "Recovery.Location", "Recovery", "Number.Victims")

colnames(data) <- names.data

summary(data)
data$Victim.AgeGroup <- cut(data$Victim.Age, 
                            breaks=c(-0.5,5.5,10.5,Inf), 
                            labels=c('0-5','6-10','11-15'))

table(data$Victim.AgeGroup)

res <- performanceEstimation(
  PredTask(target ~ ., data.m),
  Workflow("standardWF", learner = "rpartXse"),
  CvSettings(nReps =1, nFolds = 10))
summary(res)
plot(res)

res <- performanceEstimation(
  PredTask(target ~ ., data.m),
  Workflow("standardWF", learner = "naiveBayes"),
  CvSettings(nReps =1, nFolds = 10))
summary(res)
plot(res)

data.m2$target2 <- as.factor(data.m2$target2)
data.smote2 <- SMOTE(target2 ~ ., data.m2, perc.over = 200)
table(data.smote2$target2)

# Some test models
model.tree <- rpartXse (target ~ ., data=data.smote, method="class")
pred.tree <- predict(model.tree, data.smote)
prettyTree(model.tree)
printcp(model.tree)

model.svm <- svm(target ~ ., data=data.m)
pred.svm <- predict(model.svm, data.m)

model.nb <- naiveBayes(target ~ ., data.smote)
pred.nb <- predict(model.nb, data.smote)
table(pred.nb, data.smote$target)
summary(model.nb)

model.nb$tables


model.glm <- glm(target ~ ., data.smote, family = binomial(link = "logit"))
pred.glm <- predict(model.glm, data.smote)


model.nb <- naiveBayes(target ~ ., data.smote)
pred.nb <- predict(model.nb, data.smote)
table(pred.nb, data.smote$target)
model.nb$tables


