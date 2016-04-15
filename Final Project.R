#1:(Ex 14.3 R Handbook)-The data shown in Table 14.3 were collected in a follow-up study of women patients 
#with schizophrenia (Davis, 2002). The binary response recorded at 0, 2, 6, 8 and 10 months after
#hospitalization was thought disorder (absent or present). The single covariate is the factor indicating
#whether a patient had suffered early or late onset of her condition (age of onset less than 20 years or 
#age of onset 20 yearsor above). The question of interest is whether the course of the illness differs 
#between patients with early and late onset? Investigate this question using the GEE approach. 
#i.Provide a two-page write-up (including graphs) explaining your analysis of the experiment and the 
#conclusions you can draw from it.  
#ii. As a secondary component provide annotated code that replicates your analysis.

library(HSAUR3)
library(MASS)
attach(schizophrenia2)
library(gee)
library(geepack)
library(graphics)
library(MuMIn)
data("schizophrenia2")
s2= subset(schizophrenia2, schizophrenia2$disorder=="present")
n<- table(s2$month)
n
levels(s2$month) <- abbreviate(levels(s2$month), 4)

#Boxplot patients who are suffering from the disorder.

plot(s2$month~s2$onset, data = s2, varwidth = TRUE,
     ylab = "Disorder(No. of months)",
     xlab = "Early onset or Late onset",
     main="Comparison of early and late onset")
a<- as.numeric(s2$onset)
plot(s2$onset,col=ifelse(a==1,"purple","red"), pch=16, xlab="no. of patients with disorder",
     main="Comparison of onsets for patients with disorder.")
legend(x=5,y=1.8,legend=c("< 20 yrs","> 20 yrs"),col=c("purple","red"),lty=c(6,4))
summary(s2)
summary(schizophrenia2)
#Mosaic Plot
mosaicplot(~s2$onset, data = s2, color = TRUE, las = 1,
           main= "Mosaic plot for early onset or late onset",
           xlab ="age")

# the GEE approach

sc2=schizophrenia2
sc2$disorder<- as.character(sc2$disorder)
sc2$disorder[sc2$disorder=="present"]<-1
sc2$disorder[sc2$disorder=="absent"]<-0
sc2$disorder<-as.numeric(sc2$disorder)




sc2_gee1 <- gee(disorder ~ month + onset, data = sc2, family = "binomial", id = subject,
                corstr = "independence", scale.fix = TRUE,
                scale.value = 1)
sc2_gee2 <- gee(disorder ~ month + onset, data = sc2, family = "binomial", id = subject,
                corstr = "exchangeable", scale.fix = TRUE,
                scale.value = 1)
sc2_gee3 <- gee(disorder ~ month + onset, data = sc2, family = "binomial", id = subject,
                corstr = "unstructured", scale.fix = TRUE,
                scale.value = 1)

summary(sc2_gee1)
summary(sc2_gee2)
summary(sc2_gee3)

qic<-sapply(list(sc2_gee1, sc2_gee2, sc2_gee3), QIC)

qic


*******************************
#  2: The file "y.dat" contains a dataset consisting of 150 4-variate observations. True
#group labels are provided for the first 135 data points in the file "idy.dat". Classify the
#remaining 15 observations. There are three groups in this problem.
#i. Provide a page write-up (including graphs) explaining what methods you used
#to model the groups and how you predicted the identity of the remaining 15
#observations.
#ii. As a secondary component provide annotated code that replicates your
#analysis.


library(rpart)

y <- read.table("C:/Users/dheeraj/Google Drive/Data Science/Stat 701/11 Project/y.dat", header=FALSE, sep="")
idy <- read.table("C:/Users/dheeraj/Google Drive/Data Science/Stat 701/11 Project/idy.dat", header=FALSE, sep="")
idy
ydata<-y
idy1<-idy
ydata
idy1
idy1[c(136:150),1]=0
ydata$id<- idy1$V1
ydata

dat1=ydata[c(1:135),]
dat2=ydata[c(136:150),]
dat2

  dat_rpart=rpart(id~V1+V2+V3+V4,data=dat1)
id1=predict(dat_rpart,newdata=dat2)
id1

newdata2=ydata[c(1:5,46:50,91:95),]
newdata1=ydata[-c(1:5,46:50,91:95),]

sample1=ydata[c(1:45),]
s1<- sample1[sample(nrow(sample1), size=5, replace=TRUE),]

sample2=ydata[c(46:90),]
s2<- sample2[sample(nrow(sample2), size=5, replace=TRUE),]

sample3=ydata[c(91:135),]
s3<- sample3[sample(nrow(sample3), size=5, replace=TRUE),]

sample15<- rbind(s1,s2,s3)

sample15


model= rpart(id~V1+V2+V3+V4,data=sample15)
id2=predict(dat_rpart,newdata=sample15)
id2



####
#3. Develop a GLM model from the 89 specimens that you can use to predict the group membership of 
#the remaining 199 specimens'. 
#i. Explain your GLM and assess the quality of the fit with the classified observations. 
#. Use Cross Validation to predict the accuracy of your model. 

library(boot)
library(Flury)
data(microtus)
a=microtus
b=a[c(1:89),]
c=a[-c(1:89),]

newdata3=b[c(1:4,44:49),]
sample1=b[-c(1:4,44:49),]

b


b=glm(Group~.,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

#Using K-fold cross validation for the models.
m=glm(Group~M1Left+M3Left+Foramen+Pbone+Length+ Height,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

m=glm(Group~M1Left+Foramen+Pbone+Length+ Height,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]


m=glm(Group~M1Left+Foramen,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]
summary(m)


m=glm(Group~M1Left+M2Left,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

m=glm(Group~M1Left+M2Left+M3Left,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

m=glm(Group~M1Left+M2Left+M3Left+Foramen,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

m=glm(Group~M1Left+M2Left+M3Left+Foramen+Pbone,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

m=glm(Group~M1Left+M2Left+M3Left+Foramen+Pbone+Length,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

m=glm(Group~M1Left+M2Left+M3Left+Foramen+Pbone+Length+ Height,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

m=glm(Group~M1Left+M2Left+M3Left+Foramen+Pbone,data=b,family="binomial")
p=(cv.glm(b,m,K=89))
p$delta[1]

#iii. Provide predictions for the unclassified observations.


dat_rpart1=rpart(Group~M1Left+Foramen,data=b)
id1=predict(dat_rpart1,newdata=c, type= "class")
id1
write.table(id1, "C:/Users/dheeraj/Google Drive/Data Science/Stat 701/11 Project/mydata.xls", sep="\t")

