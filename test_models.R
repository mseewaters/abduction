data <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/Sample_data.csv", stringsAsFactors=TRUE, na.strings = c("NA",""," "))

names.data <- c("Date","Victim.Age","Victim.Race","Victim.Gender","Harm","Publicized","Location","Region",
                "Relationship","RSO","Offender.Age","Offender.Race","Offender.Gender","Rural.City","Missing.Location",
                "Recovery.Location", "Recovery", "Number.Victims")

colnames(data) <- names.data

summary(data)

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

# Create victim age group
data$Victim.AgeGroup <- cut(data$Victim.Age, 
                            breaks=c(-0.5,5.5,10.5,Inf), 
                            labels=c('0-5','6-10','11-15'))

table(data$Victim.AgeGroup)

# Create a list of harm responses and identify them as 1/0
harm.list <- as.data.frame(unique(unlist(data$Harm), use.names=FALSE))
colnames(harm.list) <- c("Harm")
harm.list$target <- c(0,1,1,1,0,1)
data.a <- merge(data, harm.list, by = "Harm")
table(data.a$target)
