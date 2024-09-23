library(ggplot2)
library(e1071)
library(dplyr)
library(GGally)
library(vioplot)
library(fmsb)


data <- read.csv("T:/CodeBase/R/Assesment1/Correlation between Heart Disease factors/heart_disease_health_indicators_BRFSS2015.csv",header = TRUE)

data <- na.omit(data)


head(data)

getMode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v,uniq_v)))]
}

ggplot(data, aes(x=BMI)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogram of BMI")

ggplot(data, aes(x=BMI)) +
  geom_histogram(aes(y=..density..), binwidth=1, fill="blue", color="black", alpha=0.7) +
  stat_function(fun=dnorm, args=list(mean=mean(data$BMI, na.rm=TRUE), 
                                     sd=sd(data$BMI, na.rm=TRUE)), color="red", size=1) +
  ggtitle("BMI with Normal Distribution Curve")

meanBmi <- mean(data$BMI, na.rm=TRUE)
medianBmi <- median(data$BMI, na.rm=TRUE)
modeBmi <- as.numeric(names(sort(table(data$BMI),decreasing=TRUE))[1])
skewnessBmi <- skewness(data$BMI, na.rm=TRUE)

skewnessLabelBMI <- ifelse(skewnessBmi > 0, "(Positively Skewed)", "(Negatively Skewed)")

ggplot(data,aes(x=BMI)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  geom_vline(aes(xintercept=meanBmi), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=medianBmi), color="green", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=modeBmi), color="orange", linetype="dashed", size=1) +
  ggtitle(paste("Skewness: ", round(skewnessBmi, 2), skewnessLabelBMI,
                "\nMean: ", round(meanBmi, 2), 
                " Median: ", round(medianBmi, 2), 
                " Mode: ", round(modeBmi, 2))) +
  xlab("BMI") +
  ylab("Frequency")



ggplot(data,aes(x=BMI)) +
  geom_bar(fill="steelblue", alpha=0.7) +
  ggtitle("Bar Plot of BMI") +
  xlab("BMI ") +
  ylab("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))  


ggplot(data,aes(y=BMI)) +
  geom_boxplot(fill="lightblue", color="darkblue") +
  ggtitle("Boxplot of BMI")




ggplot(data,aes(x=Age)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  ggtitle("Histogram of Age")

ggplot(data, aes(x=Age)) +
  geom_histogram(aes(y=..density..), binwidth=1, fill="blue", color="black", alpha=0.7) +
  stat_function(fun=dnorm, args=list(mean=mean(data$Age, na.rm=TRUE), 
                                     sd=sd(data$Age, na.rm=TRUE)), color="red", size=1) +
  ggtitle("Age with Normal Distribution Curve")


meanAge <- mean(data$Age,na.rm=TRUE)
medianAge <- median(data$Age, na.rm=TRUE)
modeAge <- as.numeric(names(sort(table(data$Age),decreasing=TRUE))[1])
skewnessAge <- skewness(data$Age,na.rm=TRUE)

skewnessLabelAge <- ifelse(skewnessAge > 0, "(Positively Skewed)", "(Negatively Skewed)")

ggplot(data,aes(x=Age)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  geom_vline(aes(xintercept=meanAge), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=medianAge), color="green", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=modeAge), color="orange", linetype="dashed", size=1) +
  ggtitle(paste("Skewness: ", round(skewnessAge,2), skewnessLabelAge,
                "\nMean: ", round(meanAge,2), 
                " Median: ", round(medianAge,2), 
                " Mode: ", round(modeAge,2))) +
  xlab("Age") +
  ylab("Frequency")


ggplot(data, aes(x=Age)) +
  geom_bar(fill="orange", alpha=0.7) +
  ggtitle("Bar Plot of Age") +
  xlab("Age ") +
  ylab("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))  

ggplot(data,aes(y=Age)) +
  geom_boxplot(fill="lightblue", color="darkblue") +
  ggtitle("Boxplot of Age")

#

ggplot(data,aes(x=Age, fill = factor(Smoker))) +
  geom_histogram(binwidth=1, fill="lightblue", color="black", alpha=0.7) +
  ggtitle("Histogram of Smoking over Age")

ggplot(data,aes(x=BMI, fill = factor(Smoker))) +
  geom_histogram(binwidth=2, fill="lightblue", color="black", alpha=0.7) +
  ggtitle("Histogram of Smoking over BMI")




ggplot(data, aes(x=Income)) +
  geom_histogram(binwidth=1, fill="red", color="black", alpha=0.7) +
  ggtitle("Histogram of Income")

ggplot(data, aes(x=Income)) +
  geom_histogram(aes(y=..density..), binwidth=1, fill="blue", color="black", alpha=0.7) +
  stat_function(fun=dnorm, args=list(mean=mean(data$Income, na.rm=TRUE), 
                                     sd=sd(data$Income,na.rm=TRUE)), color="red", size=1) +
  ggtitle("Income with Normal Distribution Curve")

meanIncome <- mean(data$Income, na.rm=TRUE)
medianIncome <- median(data$Income, na.rm=TRUE)
modeIncome <- as.numeric(names(sort(table(data$Income),decreasing=TRUE))[1])
skewnessIncome <- skewness(data$Income,na.rm=TRUE)

skewnessLabelIncome <- ifelse(skewnessIncome > 0, "(Positively Skewed)", "(Negatively Skewed)")

ggplot(data,aes(x=Income)) +
  geom_histogram(binwidth=1, fill="blue",color="black", alpha=0.7) +
  geom_vline(aes(xintercept=meanIncome),color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=medianIncome),color="green", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=modeIncome),color="orange", linetype="dashed", size=1) +
  ggtitle(paste("Skewness: ", round(skewnessIncome, 2),skewnessLabelIncome,
                "\nMean: ", round(meanIncome,2), 
                " Median: ", round(medianIncome, 2), 
                " Mode: ", round(modeIncome,2))) +
  xlab("Income") +
  ylab("Frequency")



ggplot(data,aes(x=Income)) +
  geom_bar(fill="steelblue", alpha=0.7) +
  ggtitle("Bar Plot of Income") +
  xlab("Income ") +
  ylab("Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 


ggplot(data,aes(y=Income)) +
  geom_boxplot(fill="lightblue", color="darkblue") +
  ggtitle("Boxplot of Income")


ggplot(data,aes(x=factor(Smoker))) +
  geom_bar(fill="steelblue", alpha=0.7) +
  ggtitle("Bar Plot of Smoking Status") +
  xlab("Smoker (0 = No, 1 = Yes)") +
  ylab("Count") +
  theme_minimal()


ggplot(data, aes(x=factor(Smoker), y=BMI)) +
  geom_boxplot(fill="lightgreen", alpha=0.7) +
  ggtitle("Box Plot of BMI by Smoking Status") +
  xlab("Smoker (0 = No, 1 = Yes)") +
  ylab("BMI") +
  theme_minimal()



numericCols <- select(data,Age,BMI,MentHlth,PhysHlth,Income,Education)
sampleData <- numericCols %>% sample_n(5000)  

ggpairs(sampleData) + 
  ggtitle("Scatterplot Matrix: (Sample of 5000 Rows)")


ggplot(data, aes(x=factor(Stroke), y=BMI)) +
  geom_violin(fill="lightgreen", alpha=0.7) +
  stat_summary(fun = median, geom = "point", color = "red", size = 2) + 
  stat_summary(fun.data = function(y) {
    return(data.frame(ymin = quantile(y, 0.25), 
                      ymax = quantile(y, 0.75), 
                      y = median(y)))
  }, geom = "errorbar", width = 0.2, color = "blue") +  
  ggtitle("Violin Plot of BMI by Stroke Status") +
  xlab("Stroke (0 = No, 1 = Yes)") +
  ylab("BMI")


ggplot(data, aes(x=factor(Diabetes), y=BMI)) +
  geom_violin(fill="lightcoral", alpha=0.7) +
  stat_summary(fun = median, geom = "point", color = "red", size = 2) +
  stat_summary(fun.data = function(y) {
    return(data.frame(ymin = quantile(y, 0.25), 
                      ymax = quantile(y, 0.75), 
                      y = median(y)))
  }, geom = "errorbar", width = 0.2, color = "blue") + 
  ggtitle("Violin Plot of BMI by Diabetes Status") +
  xlab("Diabetes (0 = No, 1 = Yes)") +
  ylab("BMI")

ggplot(data, aes(x=factor(HeartDiseaseorAttack), y=Veggies)) +
  geom_violin(fill="orange", alpha=0.7) +
  stat_summary(fun = median, geom = "point", color = "red", size = 2) +  
  stat_summary(fun.data = function(y) {
    return(data.frame(ymin = quantile(y, 0.25), 
                      ymax = quantile(y, 0.75), 
                      y = median(y)))
  }, geom = "errorbar", width = 0.2, color = "blue") +  
  ggtitle("Violin Plot of Veggies by Heart Disease or Attack Status") +
  xlab("Heart Disease or Attack (0 = No, 1 = Yes)") +
  ylab("Veggies")


ggplot(data, aes(x=factor(Education), y=Income)) +
  geom_violin(fill="purple", alpha=0.7) +
  stat_summary(fun = median, geom = "point", color = "red", size = 2) +  
  stat_summary(fun.data = function(y) {
    return(data.frame(ymin = quantile(y, 0.25), 
                      ymax = quantile(y, 0.75), 
                      y = median(y)))
  }, geom = "errorbar", width = 0.2, color = "blue") +  
  ggtitle("Violin Plot of Income by Education") +
  xlab("Education") +
  ylab("Income")



radarData <- data %>%
  summarise(Age=mean(Age,na.rm = TRUE), 
            BMI=mean(BMI,na.rm = TRUE), 
            MentHlth=mean(MentHlth,na.rm = TRUE), 
            PhysHlth=mean(PhysHlth,na.rm = TRUE),
            Education=mean(PhysHlth,na.rm = TRUE),
            Income=mean(PhysHlth,na.rm = TRUE))

radarData <- rbind(rep(100,4),rep(0,4),radarData)

radarchart(radarData, 
           axistype=1, 
           title="Radar Chart for Health Attributes ",
           pfcol=rgb(1, 0, 0, 0.5),  
           pcol="darkred",           
           plwd=2,                   
           cglcol="grey",            
           cglty=1,                 
           axislabcol="black",      
           caxislabels=seq(0, 100, 25),  
           cglwd=0.8)        



AvgIncome <- data %>%
  group_by(Education) %>%
  summarize(avgIncome = mean(Income, na.rm = TRUE), .groups = 'drop')


AvgBmi <- data %>%
  group_by(Age) %>%
  summarize(avg_bmi = mean(BMI, na.rm = TRUE), .groups = 'drop')

incomePlot <- ggplot(AvgIncome, aes(x = Education,y = avgIncome)) +  
  geom_line() +
  geom_point() +
  labs(title = "Average Income vs Education Level",
       x = "Education Level (1-10)",
       y = "Average Income") +
  theme_minimal()


bmiPlt <- ggplot(AvgBmi, aes(x = Age, y = avg_bmi)) +  
  geom_line() +
  geom_point() +
  labs(title = "Average BMI vs Age",
       x = "Age",
       y = "Average BMI") +
  theme_minimal()

print(incomePlot)
print(bmiPlt)

