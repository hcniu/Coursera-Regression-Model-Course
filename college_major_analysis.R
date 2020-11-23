install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)
devtools::install_github("jhudsl/matahari")
library(matahari)
library(ggplot2)
dance_start(value = FALSE, contents = FALSE)
str(college)
head(college)
college$major<-as.factor(college$major)
college$major_category<-as.factor(college$major_category)
fit<-lm(median~major_category+perc_employed+perc_men+perc_college_jobs,college)
summary(fit)
ggplot(data = college,aes(x=major_category,y=median))+
  geom_boxplot()
library(MASS)
library(agricolae)
library(car)
model <- aov( median ~ major_category , data = college)
LSD <- LSD.test( model,  "major_category", group = FALSE)
comparison<-as.data.frame(LSD$comparison)
comparisonSig<-dplyr::filter(comparisonSig,pvalue<0.05)
comparisonSig<-dplyr::mutate(comparison,"Compare"=rownames(comparison))###From the LSD Test we find out the Business Majors have well better salary

college$major_category<-as.character(college$major_category)
college$major_category[which(is.na(college$major_category))]<-"Others"
fit2<-lm(median~major_category,college)
summary(fit2)

dance_save("~/Desktop/college_major_analysis.rds")


