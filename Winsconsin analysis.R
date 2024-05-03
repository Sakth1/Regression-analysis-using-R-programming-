#NAME: Sakthi Murugan C
#ROLL NO: 23121127

#installing package
install.packages("readxl")
install.packages("dplyr")
install.packages("corrplot")
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

#importing data
data1<-read_excel("E:\\Users\\Desktop\\Professional\\EDA\\wisconsin.xlsx")

#missing values
d1<-mutate(data1)

#data summary
summary(data1)
summary(d1)

#Exclude 's.no' and 'diagnosis' to perfor correlation
d1 <- d1[, -1]
d1 <- d1[, -ncol(d1)]

#Correlation
cr <- cor(d1)
corrplot(cr,method="color", order = "hclust",tl.cex = 0.7, tl.col = "black")
?corrplot()

#Visualizing data
ggplot(data=data1, aes(x=x.texture_mean,fill=diagnosis))+ 
  geom_boxplot()+
  labs(title="Mean Texture Distribution", x="Mean Texture", y="Diagnosis",fill="Diagnosis")+
  scale_fill_discrete(labels=c('Malignant ', 'Benign '))

ggplot(data=data1, aes(x=x.area_mean,fill=diagnosis))+ 
  geom_histogram()+
  labs(title="Mean area of the tumor cells Distribution", x="Mean area of the tumor cells", y="Count",
       fill = "Diagnosis")+ theme(legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))+
  scale_fill_discrete(labels=c('Malignant ', 'Benign '))

ggplot(data=data1, aes(x=x.radius_mean, fill=diagnosis))+
  geom_histogram(bins=50)+
  labs(title="Mean radius of the tumor cells Distribution", x="Mean radius of the tumor cells", y="Count",
       fill = "Diagnosis")+ theme(legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))+
  scale_fill_discrete(labels=c('Malignant ', 'Benign '))

ggplot(data=data1, aes(x=x.perimeter_mean,fill=diagnosis))+ 
  geom_boxplot()+
  labs(title="Mean perimeter of the tumor cells Distribution", x="Mean perimeter of the tumor cells", fill="Diagnosis")+
  scale_fill_discrete(labels=c('Malignant ', 'Benign '))

ggplot(data=data1, aes(x=x.area_se,y=x.radius_se))+ 
  geom_point()+
  labs(title="SE area of the tumor cells Distribution", x="Standard error of the area of the tumor cells",
       y="Standard error of the radius of the tumor cells")

ggplot(data=data1, aes(x=x.concave_pts_mean,y=x.smoothness_mean))+ 
  geom_point()+
  labs(title="Mean number of concave portions", x="Mean number of concave portions of the contour of the tumor cells",
       y="Mean smoothness of the tumor cells")

ggplot(data=data1, aes(x=x.concave_pts_worst,y=x.concavity_mean))+ 
  geom_point()+
  labs(title="Worst number of concave portions", x="Worst number of concave portions of the contour of the tumor cells",
       y="Mean concavity of the tumor cells",fill="Diagnosis")

ggplot(data=data1, aes(x=x.perimeter_se,fill=diagnosis))+ 
  geom_boxplot()+
  labs(title="Standard error of the perimeter", x="Standard error of the perimeter of the tumor cells",fill="Diagnosis")+
  scale_fill_discrete(labels=c('Malignant ', 'Benign '))


#Regression
dim(data1)
xx<-data1$diagnosis["B"]
xx
data1$diagnosis <- as.numeric(as.factor(data1$diagnosis))
sum(is.na(data1$diagnosis))
reg<-lm(diagnosis ~ x.texture_mean+ x.area_se+ x.radius_se+ x.perimeter_se+
          x.area_mean+ x.radius_mean+ x.perimeter_mean+ x.concave_pts_worst+
          x.concavity_mean+ x.concave_pts_mean+ x.smoothness_mean, data =data1)
reg2<-lm(diagnosis ~ x.texture_mean,data=data1)
summary(reg)
summary(reg2)

plot(reg)

#residual KDE plot
d<-density(reg[['residuals']])
plot(d,main='Residual KDE Plot',xlab='Residual value')
summary(reg)

