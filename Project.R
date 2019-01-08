library(readr)
GPA_Food <- read.csv("D:/Naresh/MS BAPM/Statistical Analysis using R/Project/GPA Food.csv")
View(GPA_Food)

install.packages("corrgram") # install corrgram package for correlation between variables
library(corrgram) # Load corrgram



table(GPA_Food$calories_chicken) # answer is 720, highest responses gave answer as 610
table(GPA_Food$calories_day) # 63 respondents feel consuming calories per day is moderately important 
table(GPA_Food$calories_scone) # answer is 420, highest responses gave answer as 420
table(GPA_Food$coffee) # 94 selected coffee correctly
table(GPA_Food$comfort_food_reasons_coded) # 49 said boredom was reason for comfort food,stress & depression these three are 90%
prop.table(table(GPA_Food$cook))*100 #40% - whenever they can, 28-Couple times a week
prop.table(table(GPA_Food$cuisine))*100 # 80% american, 12% mexican,spanish
prop.table(table(GPA_Food$diet_current_coded))*100 # 48% unhealthy, 40% healthy
prop.table(table(GPA_Food$drink))*100 # 56% soda, 44% juice
prop.table(table(GPA_Food$eating_changes_coded1))*100 # 35% worse quality, 26% healthier
prop.table(table(GPA_Food$eating_out))*100 #48% - 1-2 times a week
prop.table(table(GPA_Food$employment))*100 #47% no employment, 52% parttime
prop.table(table(GPA_Food$ethnic_food))*100 # 82% neutral to verylikely, in increasing order
prop.table(table(GPA_Food$exercise))*100 # 51% everyday, 39% twice a week``
prop.table(table(GPA_Food$father_education))*100 # 37% college, 23% graduate, 27 High school
prop.table(table(GPA_Food$fav_cuisine_coded))*100 # 47% mexican
prop.table(table(GPA_Food$fav_food))*100 # 60% home cooked, 31% store baught cooked home
prop.table(table(GPA_Food$fruit_day))*100 # 50% very likely, 26% likely
prop.table(table(GPA_Food$grade_level))*100 # 1 - freshman, 2 -Sophomore, 3 - Junior, 4 - Senior
prop.table(table(GPA_Food$healthy_feeling))*100 #1 to 10 where 1 is strongly feel healthy   
prop.table(table(GPA_Food$sports))*100 # 61% sports yes
prop.table(table(GPA_Food$vitamins))*100 # do you take vitamins



#_____________________________________________________________________________________________________________________________________________________

# data preprocessing : removed null values from gpa, father and mothers education

#H0: Mothers education has no relation with GPA of childerens 

# Parametric

GPA_Mother <- GPA_Food[which(is.na(GPA_Food$GPA=='FALSE')&is.na(GPA_Food$mother_education=='FALSE')),]

nohighschool = GPA_Food[GPA_Food$mother_education==1,1]
highschool = GPA_Food[GPA_Food$mother_education!=1,1]
mean(nohighschool$GPA,na.rm = T)
sd(nohighschool$GPA,na.rm = T)
mean(highschool$GPA,na.rm = T)
sd(highschool$GPA,na.rm = T)
nrow(highschool)
mean(GPA_Food$GPA,na.rm = T)

f1=function()
{
  s1=rnorm(5,mean=3.4278,sd=0.7539647)
  s2=rnorm(112,mean=3.4278,sd=0.3604)
  return(abs(mean(s1)-mean(s2)))
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(nohighschool$GPA,na.rm = T)-mean(highschool$GPA,na.rm = T))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.176 : Hence we can not reject the null hypothesis that  there is no relationship between child's GPA and mothers education

# Non - Parametric

#H0: Mothers education has no relation with GPA of childerens

g=GPA_Food$GPA
sample(g)   # this randomly shuffles the values of g (So we achieve objective of randomly shuffling the data)
length(g)

f1=function()
{
  x=sample(g,replace = TRUE)
  z=abs(mean(x[1:5]-mean(x[6:117])))  # after random shuffle we take first 21 rows as one collection and last 23 rows as other collection, and check mean difference
  return(z)
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(nohighschool$GPA)-mean(highschool$GPA))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.01011 : Hence We can reject the null hypothesis that there is no relationship between child's GPA and mothers education

# we trust nonparametric test more and based on that we can say that there is relationship between GPA and mothers education

#_____________________________________________________________________________________________________________________________________________________

#H0: Fathers education has no relation with GPA of childerens

# Parametric

nohighschool = GPA_Food[GPA_Food$father_education==1,1]
highschool = GPA_Food[GPA_Food$father_education!=1,1]
mean(nohighschool$GPA,na.rm = T)
sd(nohighschool$GPA,na.rm = T)
mean(highschool$GPA,na.rm = T)
sd(highschool$GPA,na.rm = T)
nrow(highschool)
mean(GPA_Food$GPA,na.rm = T)

f1=function()
{
  s1=rnorm(4,mean=3.4278,sd=0.76268)
  s2=rnorm(113,mean=3.4278,sd=0.3714)
  return(abs(mean(s1)-mean(s2)))
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(nohighschool$GPA,na.rm = T)-mean(highschool$GPA,na.rm = T))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.3438 : Hence we can not reject the null hypothesis
# Non - Parametric

#H0: Fathers education has no relation with GPA of childeren

g=GPA_Food$GPA
sample(g)   # this randomly shuffles the values of g (So we achieve objective of randomly shuffling the data)
length(g)

f1=function()
{
  x=sample(g)
  z=abs(mean(x[1:4]-mean(x[4:117])))  # after random shuffle we take first 21 rows as one collection and last 23 rows as other collection, and check mean difference
  return(z)
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(nohighschool$GPA)-mean(highschool$GPA))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.0605 : Hence We can almost reject the null hypothesis 

# we trust nonparametric test more and based on that we can say that there is relationship between GPA and fathers education



#_____________________________________________________________________________________________________________________________________________________


#H0: Interest in sports has no relation with GPA of children 

# Parametric

GPA_Sports <- GPA_Food[which((is.na(GPA_Food$GPA)=='FALSE')&(is.na(GPA_Food$sports)=='FALSE')),]

No = GPA_Sports[GPA_Sports$sports==2,1]
Yes = GPA_Sports[GPA_Sports$sports==1,1]
mean(No$GPA,na.rm = T)
sd(No$GPA,na.rm = T)
mean(Yes$GPA,na.rm = T)
sd(Yes$GPA,na.rm = T)
nrow(Yes)
mean(GPA_Food$GPA,na.rm = T)

f1=function()
{
  s1=rnorm(41,mean=3.4278,sd=0.406)
  s2=rnorm(74,mean=3.4278,sd=0.358)
  return(abs(mean(s1)-mean(s2)))
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(No$GPA,na.rm = T)-mean(Yes$GPA,na.rm = T))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.207 : Hence we can not reject the null hypothesis 

# Non - Parametric

#H0: Interest in sports has no relation with GPA of children's 

g=GPA_Sports$GPA
sample(g)   # this randomly shuffles the values of g (So we achieve objective of randomly shuffling the data)
length(g)

f1=function()
{
  x=sample(g)
  z=abs(mean(x[1:41]-mean(x[42:115])))  
  return(z)
}

#distribution
dist=replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="red")

# compute the test statistic - tstat
tstat=abs(mean(No$GPA)-mean(Yes$GPA))
abline(v=tstat)

# computing the p-value
gap = abs(mean(dist)-tstat)  # find the difference between mean of sampling distribution and given sample value on distribution
gap
#lside = dist[dist<mean(dist)-gap] # finding the number of observations which falls on left side of the given sample value on sampling distribution
rside = dist[dist>mean(dist)+gap] # finding the number of observations which falls on right side of the given sample value on sampling distribution

pvalue = length(rside)/length(dist)
pvalue   # P value 0.21366 : Hence We can  reject the null hypothesis 



#_____________________________________________________________________________________________________________________________________________________



