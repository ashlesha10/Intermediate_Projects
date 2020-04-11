#Part A
#1Invoke R and use the "trees" dataset  
data <- trees

#2.Find the 5 summary numbers in the data 
summary(data) 

#3.Graph a straight line regression 
#Linear regression 1
plot(data$Height, data$Volume,pch=16, cex=1.5, xlab = '',ylab = '', main = 'Linear Regression of Height over Volume')
mtext(side = 1, line = 2, 'Height', font = 2)
mtext(side = 2, line = 2, 'Volume', font = 2)
Lin_reg1 <- lm(Volume ~ Height, data = data)
abline(Lin_reg1, col = 'red', lwd=2)
summary(Lin_reg1)

#Linear regression 2
plot(data$Girth, data$Volume,pch=16, cex=1.5,xlab = '',ylab = '', main = 'Girth vs Volume')
mtext(side = 1, line = 2, 'Girth', font = 2)
mtext(side = 2, line = 2, 'Volume', font = 2)
Lin_reg2 <- lm(Volume ~ Girth, data = data)
abline(Lin_reg2, col = 'red', lwd=2)
summary(Lin_reg2)

#multiple regression
plot(data,cex= 1,pch=16)
mul_reg <- lm(Volume ~ Height + Girth, data = data)
summary(mul_reg)

#4.Create Histograms and density plots

H_Dens <- density(data$Height)
hist(data$Height,freq = FALSE,xlab = '',ylab = '', main = 'Height Distribution of Trees')
mtext(side = 1, line = 2, 'Height', font = 2)
mtext(side = 2, line = 2, 'Density', font = 2)
lines(H_Dens, lwd = 2)

G_Dens <- density(data$Girth)
hist(data$Girth,freq = FALSE, xlab = '',ylab = '', main = 'Girth Distribution of Trees')
mtext(side = 1, line = 2, 'Girth', font = 2)
mtext(side = 2, line = 2, 'Density', font = 2)
lines(G_Dens, lwd = 2)

V_Dens <- density(data$Volume)
hist(data$Volume,freq = FALSE,xlab = '',ylab = '', main = 'Volume Distribution of Trees')
mtext(side = 1, line = 2, 'Volume', font = 2)
mtext(side = 2, line = 2, 'Density', font = 2)
lines(V_Dens, lwd = 2)

#5.Create Boxplots 

boxplot(data$Height, main = "Height of Trees", col = "darkorange3")
boxplot(data$Girth, main = "Girth(diameter) of Trees", col = "darkorange3")
boxplot(data$Volume, main = "Volume of Trees", col = "darkorange3")

#6.Normal Probability plots

#height
qqnorm(data$Height,pch=16, font = 2)
qqline(data$Height, col= 'red', lwd =2)

#Girth
qqnorm(data$Girth,pch=16)
qqline(data$Girth, col= 'red', lwd =2)

#volume
qqnorm(data$Volume,pch=16)
qqline(data$Volume, col= 'red', lwd =2)

#Part B
library(MASS)
library(ggplot2)
library(ggcorrplot)

rubber_data <- Rubber
ob_data <- read.csv('C:\\Users\\Ashlesha\\Desktop\\CPS\\Intermediate\\Week 1\\dataset-60960.csv')
summary(rubber_data)
#rubber data

corr <- cor(rubber_data)
ggcorrplot(corr, lab = TRUE)


# multiple regression 

log_rub <- log(rubber_data)
plot(log_rub)
mul_reg2 <- lm(loss ~ hard + tens, data = log_rub)
summary(mul_reg2)

#oddbooks
summary(ob_data)
corr <- cor(ob_data)
ggcorrplot(corr, lab = TRUE)

# multiple regression 

log_rub <- log(ob_data)
plot(log_rub)
mul_reg2 <- lm(weight ~ height + breadth + thick, data = log_rub)
summary(mul_reg2)


