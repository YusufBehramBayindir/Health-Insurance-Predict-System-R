#Yusuf Behram Bayındır
# Çağan Tezcan 

install.packages("e1071")
install.packages("stats")
install.packages("Amelia")
install.packages("gridExtra")
install.packages("caTools")
install.packages("ROCR")

library(e1071) 
#Package for Skewness function used for data analysis
library(stats) 
#Package for finding cook's distance
library(ggplot2)
#Package for visualisation of data
library(Amelia)
#package to visually display the missing values
library(gridExtra)
#Package for arranging different plots in a single grid
library(caTools)
# Package for validation of models
library(pscl)
# Package for Mc-Fadden R Test
library(ROCR)
#Package for ROC graphs


HealthInsurance <- read.csv("/Users/yusufbayindir/Downloads/HealthInsurance.csv", header = TRUE, sep="," , stringsAsFactors = T)

gender <- c("Male","Female")
gender.factor <- factor(gender)
gender.factor
str(gender.factor)

insurance <- c("yes", "no")
insurance.factor <- factor(insurance)
insurance.factor
str(insurance.factor)

married <- c("yes", "no")
married.factor <- factor(married)
married.factor
str(married.factor)

selfemp <- c("yes", "no")
selfemp.factor <- factor(selfemp)
selfemp.factor
str(selfemp.factor)

region <- c("east", "north", "south","west")
region.factor <- factor(region)
region.factor
str(region.factor)

ethnicity <-c("afam","cauc" , "other")
ethnicity.factor <- factor(ethnicity)
ethnicity.factor
str(ethnicity.factor)

education <- c("none","highschool","master","bachelor","ged","other", "phd")
education.factor <- factor(education)
education.factor
str(education.factor)

limit <- c("no","yes")
limit.factor <- factor(limit)
limit.factor
str(limit.factor)


summary(HealthInsurance)
HealthInsurance$age[HealthInsurance$age==0]<- NA
HealthInsurance$family[HealthInsurance$family==0]<- NA

head(HealthInsurance)
str(HealthInsurance)

hist(HealthInsurance$age)
hist(HealthInsurance$family)

install.packages("Amelia")
missmap(HealthInsurance, main = "Missing values vs observed")


is.factor(HealthInsurance$ethnicity)
is.factor(HealthInsurance$health)
is.factor(HealthInsurance$limit)
is.factor(HealthInsurance$gender)
is.factor(HealthInsurance$age)
is.factor(HealthInsurance$insurance)
is.factor(HealthInsurance$selfemp)
is.factor(HealthInsurance$family)
is.factor(HealthInsurance$region)
is.factor(HealthInsurance$married)
is.factor(HealthInsurance$education)

#We can see that all the variables are factor except age and family variables. 


contrasts(HealthInsurance$health)
contrasts(HealthInsurance$limit)
contrasts(HealthInsurance$gender)
contrasts(HealthInsurance$insurance)
contrasts(HealthInsurance$married)
contrasts(HealthInsurance$selfemp)
contrasts(HealthInsurance$region)
contrasts(HealthInsurance$ethnicity)
contrasts(HealthInsurance$education)

#It can be said that the raw data taken is a processed data and does not need any cleaning or formatting.



pairs(~health,age,limit,gender,insurance,married,selfemp,family,region,ethnicity,education , data=HealthInsurance)

plot1 = qplot(age, data = HealthInsurance, xlab = "age")
plot1
plot2 = qplot(age, data = HealthInsurance, geom = "density", fill = "red")
plot2
plot3 = qplot(sample = age, data = HealthInsurance) 
plot3
plot4 = qplot(family, data = HealthInsurance, xlab = "Evaluation")
plot4
plot5 = qplot(family, data = HealthInsurance, geom = "density", fill = "red")
plot5
plot6 = qplot(sample = family, data = HealthInsurance) 
plot6
grid.arrange(plot1, plot2, plot3, ncol = 3)
grid.arrange(plot4, plot5, plot6, ncol = 3)

#Skewness

skewness(HealthInsurance$age)
skewness(HealthInsurance$family)

log.y<-log(HealthInsurance$family)
plot(log.y,xlab="family",ylab="Log(family)")
sqrt.y<-sqrt(HealthInsurance$family)
plot(sqrt.y,xlab="famlily",ylab="Sqrt(family)")


plot4<- qplot(insurance,family, data=HealthInsurance, geom=c("boxplot"))
plot5<- qplot(insurance,age, data=HealthInsurance, geom=c("boxplot"))
grid.arrange(plot4, plot5)
numeric_data <- HealthInsurance[,c("family","age")]
numeric_data <- data.frame(scale(numeric_data ))
stripchart(numeric_data,
           vertical = TRUE, 
           method = "jitter", 
           col = "orange", 
           pch=1,
           main="Stripcharts")

healthinsurance_r = data.frame(scale(numeric_data))
summary(numeric_data)

boxplot(numeric_data, main = "Boxplot of re-scaled variables",col = (c("gold","darkgreen")))
stripchart(numeric_data, vertical = TRUE, method = "jitter", col = (c("gold","darkgreen")), pch = 1, main = "Stripcharts of re-scaled variables")

cor(numeric_data)


a.data <- data.frame(HealthInsurance$insurance, HealthInsurance$health)
a.data = table(HealthInsurance$insurance, HealthInsurance$health) 
print(a.data)
barplot(a.data, beside = TRUE, legend = levels(unique(HealthInsurance$health)) + title("Health insurance & health")) 
print(chisq.test(a.data))
a.data <- data.frame(HealthInsurance$insurance, HealthInsurance$limit)
a.data = table(HealthInsurance$insurance, HealthInsurance$limit) 
print(a.data)
barplot(a.data, beside = TRUE, legend = levels(unique(HealthInsurance$limit)))
print(chisq.test(a.data))
a.data <- data.frame(HealthInsurance$insurance, HealthInsurance$gender)
a.data = table(HealthInsurance$insurance, HealthInsurance$gender) 
print(a.data)
barplot(a.data, beside = TRUE, legend = levels(unique(HealthInsurance$gender)) + title("Health Insurance & Gender"))
print(chisq.test(a.data))
a.data <- data.frame(HealthInsurance$insurance, HealthInsurance$married)
a.data = table(HealthInsurance$insurance, HealthInsurance$married) 
print(a.data)
barplot(a.data, beside = TRUE, legend = levels(unique(HealthInsurance$married)) + title("Health Insurance & Married"))
print(chisq.test(a.data))
a.data <- data.frame(HealthInsurance$insurance, HealthInsurance$selfemp)
a.data = table(HealthInsurance$insurance, HealthInsurance$selfemp) 
print(a.data)
barplot(a.data, beside = TRUE, legend = levels(unique(HealthInsurance$selfemp)) + title("Health Insurance & Selfemp") )
print(chisq.test(a.data))
a.data <- data.frame(HealthInsurance$insurance, HealthInsurance$region)
a.data = table(HealthInsurance$insurance, HealthInsurance$region) 
print(a.data)
barplot(a.data, beside = TRUE, legend = levels(unique(HealthInsurance$region))+ title("Health Insurance & Region"))
print(chisq.test(a.data))
a.data <- data.frame(HealthInsurance$insurance, HealthInsurance$ethnicity)
a.data = table(HealthInsurance$insurance, HealthInsurance$ethnicity) 
print(a.data)
barplot(a.data, beside = TRUE, legend = levels(unique(HealthInsurance$ethnicity)) + title("Health Insurance & Ethnicity"))
print(chisq.test(a.data))
a.data <- data.frame(HealthInsurance$insurance, HealthInsurance$education)
a.data = table(HealthInsurance$insurance, HealthInsurance$education) 
print(a.data)
barplot(a.data, beside = TRUE, legend = levels(unique(HealthInsurance$education)))
print(chisq.test(a.data))


set.seed(100)
split<- sample.split(HealthInsurance,SplitRatio = 0.8)
training<- subset(HealthInsurance, split=="TRUE")
testing<- subset(HealthInsurance, split=="FALSE")
dim(training)
dim(testing)

model1 <- glm (insurance ~. , data = training, family = binomial(link='logit'))
summary(model1)


#Remove Ethnicity to construct the model
model2 <- glm(insurance ~ health+age+limit+gender+married+selfemp+family+region+education, family =binomial (link='logit'),data=training)
summary(model2)


#Model after removing region variable
model3 <- glm(insurance ~ health+age+limit+gender+married+selfemp+family+education, family =binomial (link='logit'),data=training)
summary(model3)

#Model after removing limit variable
model4 <- glm(insurance ~ health+age+married+gender+selfemp+family+education, family =binomial (link='logit'),data=training)
summary(model4)


# For Alpha=.05
# We find the 95% probable interval from the 0.0 and 0.95 quantiles of the F distribution for (6388,6401) degree of freedom for model 4

anova(model4)
lwr <- qf(0, 6388, model4$df.residual)
upr <- qf(0.95, 6388, model4$df.residual)
c(lwr, upr)

summary(model4)

lwrpf <- pf(0, 6388, model4$df.residual)
uprpf <- pf(0.95, 6388, model4$df.residual)
c(lwrpf, uprpf)

anova(model1, test="Chisq")
anova(model2, test="Chisq")
anova(model3, test="Chisq")
anova(model4, test="Chisq")

pR2(model1)
pR2(model2)
pR2(model3)
pR2(model4)

(plot1 <- qplot(insurance, model4$fitted.values, geom = "boxplot", data=training)+labs(y="Fitted Values")+ggtitle("Residuals vs Test Plot"))

pred<- predict(model4, training, type= 'response')
pred<- prediction(pred, training$insurance)
eval<- performance(pred,'tpr','fpr')
plot(eval, colorize = TRUE)


model.probs=predict(model4,training,type="response")
# misclassification error:train data
pred1<- ifelse(model.probs>0.5, 1, 0)
#Confusion Matrix 
tab1<- table(Predicted= pred1, Actual= training$insurance)
tab1
# misclassification error:train data
trainerror<- 1- sum(diag(tab1))/ sum(tab1)
trainerror
#Accuracy of Training data
print(paste('Accuracy',1-trainerror))
# Test Error
model.test=predict(model4,testing,type="response")
# misclassification error:test data
pred_test<- ifelse(model.test>0.5, 1, 0)
#Confusion Matrix 
tab_test<- table(Predicted= pred_test, Actual = testing$insurance)
tab_test
# misclassification error:test data
testerror<- 1- sum(diag(tab_test))/ sum(tab_test)
testerror
#Accuracy of Training data
print(paste('Accuracy',1-testerror))


auc<- performance(pred,"auc")
auc <- auc@y.values[[1]]
auc 

# Technique of Insurance Prediction Algorithm

predict(model4,newdata=data.frame(health= "yes",age=20,family=3,gender="male",education="bachelor",married="yes",selfemp="no"),data=testing,type="response")
predict(model4,newdata=data.frame(health= "yes",age=35,family=4,gender="female",education="phd",married="yes",selfemp="yes"),data=testing,type="response")
predict(model4,newdata=data.frame(health= "yes",age=15,family=8,gender="male",education="highschool",married="no",selfemp="yes"),data=testing,type="response")
predict(model4,newdata=data.frame(health= "yes",age=48,family=5,gender="female",education="master",married="yes",selfemp="no"),data=testing,type="response")
predict(model4,newdata=data.frame(health= "no",age=76,family=2,gender="female",education="none",married="no",selfemp="yes"),data=testing,type="response")
predict(model4,newdata=data.frame(health= "no",age=76,family=2,gender="female",education="none",married="no",selfemp="no"),data=testing,type="response")

#Decison Tree

library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

head(HealthInsurance)

sample_data = sample.split(HealthInsurance, SplitRatio = 0.8)
train_data <- subset(HealthInsurance, sample_data == TRUE)
test_data <- subset(HealthInsurance, sample_data == FALSE)

model<- ctree(insurance ~ ., train_data)
plot(model)





