# Assignment Par1 - Hypothesis Testing with R


# B] - One Sample test 
chem
CI(chem,ci=0.95)
chem= c(2.9, 3.1, 3.4, 3.4, 3.7 ,3.7 ,2.8 ,2.5, 2.4, 2.4, 2.7, 2.2 ,5.28 
,3.37, 3.03, 3.03, 2.95, 3.77, 3.4 ,2.2, 3.5, 3.6, 3.7, 3.7)
t.test(chem, alternative = "greater", mu = 1)



#confidence Intervallibrary(MASS)

library(Rmisc)
CI(chem,ci=0.95)

#boxplot
options(repr.plot.width=5, repr.plot.height=4)
boxplot(chem,xlab = 'Chem Data')

# c] - Two Sample Test
cats
summary(cats)
female<- subset(cats,subset=(cats$Sex=="F"))
male<- subset(cats,subset=(cats$Sex=="M"))
t.test(male["Bwt"] ,female["Bwt"] )


#D] - Paired T test
shoes
t.test(shoes[["A"]] ,shoes[["B"]] ,alternative = "greater",paired=TRUE)



# E] - Test if equal or given proportions
bacteria
View(bacteria)
my_table<- table(bacteria$y,bacteria$ap)
View(my_table)
prop.test(x=c(177,96),n=c(220,220),alternative = "two.sided",conf.level = 0.95)


# F]- F-test
male1 <-subset(cats,subset=(cats$Sex=="M"))
female1 <-subset(cats,subset=(cats$Sex=="F"))
var.test(male1$Bwt,female1$Bwt)



# Assignment Par2 - Inferential Statistics
insurance_data <- read.csv('insurance.csv')
head(insurance_data)

# one sample t-test

male_customers <- subset(insurance_data, subset=(insurance_data$sex == 'male'))
male_sample_bmi <- sample(male_customers$bmi, size = 40)# Random sampling
t.test(male_sample_bmi, alternative = "greater", mu = 29)

#boxplot
options(repr.plot.width=5, repr.plot.height=4)
boxplot(male_sample_bmi,xlab = 'Body Mass Index')

# two sample t-test

male_smokers <- subset(male_customers, subset=(male_customers$smoker == 'yes'))
male_non_smokers <- subset(male_customers, subset=(male_customers$smoker == 'no'))

# sampling
male_smokers_sample = sample(male_smokers$bmi, size = 40)
male_non_smokers_sample = sample(male_non_smokers$bmi, size = 40)

#boxplot
options(repr.plot.width=5, repr.plot.height=5)
boxplot(male_smokers_sample,male_non_smokers_sample, xlab = 'Male smokers and non Smokers')

t.test(male_smokers_sample ,male_non_smokers_sample, conf.level = 0.95)

#F test

male_customers <- subset(insurance_data, subset=(insurance_data$sex == 'male'))
female_customers <- subset(insurance_data, subset=(insurance_data$sex == 'female'))

# sampling
male_customers_sample = sample(male_customers$bmi, size = 40)
female_customers_sample = sample(female_customers$bmi, size = 40)

options(repr.plot.width=5, repr.plot.height=5)
boxplot(male_customers_sample,female_customers_sample, xlab = 'Male and female customers')

var.test(male_customers_sample, female_customers_sample)