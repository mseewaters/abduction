data <- read.csv("D:/0 Stern MSBA/0.2 Abduction/abduction2/Sample_data.csv", stringsAsFactors=FALSE)

names.data <- c("Date","Victim.Age","Victim.Race","Victim.Gender","Harm","Publicized","Location","Region",
                "Relationship","RSO","Offender.Age","Offender.Race","Offender.Gender","Rural.City","Missing.Location",
                "Recovery.Location", "Recovery", "Number.Victims")

colnames(data) <- names.data

summary(data)
data$Victim.AgeGroup <- cut(data$Victim.Age, 
                         breaks=c(-0.5,5.5,10.5,Inf), 
                         labels=c('0-5','6-10','11-15'))