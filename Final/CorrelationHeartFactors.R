
library(dplyr)
library(caret)
library(corrplot)
library(ggplot2)
library(reshape2)


data <- read.csv("T:/CodeBase/R/Assesment1/Correlation between Heart Disease factors/heart_disease_health_indicators_BRFSS2015.csv",header = TRUE)


head(data)
str(data)

sum(is.na(data))

subData <- data[, c('Age','BMI','PhysHlth','MentHlth','Sex','HighChol','HeartDiseaseorAttack','Smoker')]


subData$Sex <- as.factor(subData$Sex) 
subData$HighChol <- as.factor(subData$HighChol)  
subData$HeartDiseaseorAttack <- as.factor(subData$HeartDiseaseorAttack)  
subData$Smoker <- as.factor(subData$Smoker)  


corMatrix <- cor(subData[, c('Age','BMI','PhysHlth','MentHlth')], method = "pearson")
print("Pearson Correlation Matrix (Numerical Variables):")
print(corMatrix)

corrplot(corMatrix,method = "circle",type = "upper",tl.col = "black",tl.srt = 45,title ="Pearson Correlation Matrix")


spearman_corMatrix <- cor(subData[, c('Age','BMI','PhysHlth','MentHlth')],method = "spearman")
print("Spearman Correlation Matrix (Numerical Variables):")
print(spearman_corMatrix)


corrplot(spearman_corMatrix,method = "circle",type = "upper",tl.col = "black",tl.srt = 45,title = "Spearman Correlation Matrix")

results <- list()

logicalVars <- c('Sex','HighChol','HeartDiseaseorAttack','Smoker')

for (i in logicalVars) {
  for (j in c('Age','BMI','PhysHlth','MentHlth')) {
    test <- cor.test(as.numeric(subData[[i]]), subData[[j]], method = "pearson")
    results[[paste(i,j,sep = "_vs_")]] <- list(
      correlation = test$estimate,
      p_value = test$p.value,
      t_value = test$statistic
    )
  }
}


anovaResults <- list()

for (numericVar in c('Age','BMI','PhysHlth','MentHlth')) {
  for (logicalVar in logicalVars) {
    formula <- as.formula(paste(numericVar,"~",logicalVar))
    anovaTest <- aov(formula, data = subData)
    anovaResults[[paste(numericVar,logicalVar, sep = "_vs_")]] <- summary(anovaTest)
  }
}


for (var1 in logicalVars) {
  for (var2 in logicalVars) {
    if (var1 != var2) {
      chi_test <- chisq.test(table(subData[[var1]], subData[[var2]]))
      results[[paste(var1,var2,sep = "_vs_")]] <- list(
        chi_squared = chi_test$statistic,
        p_value = chi_test$p.value
      )
    }
  }
}


print("Correlation Test Results (Logical vs Continuous Variables):")
print("Variable Pair              Correlation   P value      T value")

for (pair in names(results)) {
  res <- results[[pair]]
  if (!is.null(res$correlation)) {
    cat(sprintf("%-30s %.4f       %e   %.4f\n", pair,res$correlation,res$p_value,res$t_value))
  }
}


print("ANOVA Results:")
for (pair in names(anova_results)) {
  cat("\n", pair, ":\n")
  print(anova_results[[pair]])
}

print("Chi-squared Test Results (Logical vs Logical Variables):")
print("Variable Pair              Chi-Squared   P value")

for(pair in names(results)) {
  res <- results[[pair]]
  if (!is.null(res$chi_squared)) {
    cat(sprintf("%-30s %.4f        %e\n", pair, res$chi_squared, res$p_value))
  }
}

getMode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v,uniq_v)))]
}


meanBmi <- mean(subData$BMI, na.rm = TRUE)
medianBmi <- median(subData$BMI, na.rm = TRUE)
modeBmi <- getMode(subData$BMI)

meanAge <- mean(subData$Age, na.rm = TRUE)
medianAge <- median(subData$Age, na.rm = TRUE)
modeAge <- getMode(subData$Age)


ggplot(data, aes(x=Age, y=BMI)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  labs(title="Scatter plot of Age vs. BMI",
       x="Age", y="BMI")

ggplot(subData,aes(x = Age)) +
  geom_histogram(binwidth = 5,fill = "blue",color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = meanAge),color = "red", linetype = "dashed", size = 1) +   
  geom_vline(aes(xintercept = medianAge),color = "green", linetype = "dotted", size = 1) +  
  geom_vline(aes(xintercept = modeAge),color = "purple", linetype = "solid", size = 1) +
  labs(title = "Histogram of Age with Mean, Median, and Mode",
       x = "Age", y = "Frequency",
       caption = "Red = Mean, Green = Median, Purple = Mode") +
  theme_minimal()


ggplot(subData, aes(x = BMI)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = meanBmi),color = "blue", linetype = "dashed",size = 1,show.legend = TRUE) +
  geom_vline(aes(xintercept = medianBmi),color = "red", linetype = "dotted",size = 1,show.legend = TRUE) +
  geom_vline(aes(xintercept = modeBmi),color = "purple", linetype = "solid",size = 1,show.legend = TRUE) +
  labs(title = "Histogram of BMI with Mean, Median, and Mode",
       x = "BMI", y = "Frequency",
       caption = "Blue = Mean, Red = Median, Purple = Mode") +
  theme_minimal()


ggplot(subData, aes(x = Sex, y = BMI)) +
  geom_violin(trim = FALSE, fill = "lightblue") +
  labs(title = "Violin Plot: BMI by Sex",x = "Sex",y = "BMI") +
  theme_minimal()


ggplot(subData, aes(x = Sex, y = PhysHlth)) +
  geom_violin(trim = FALSE, fill = "lightgreen") +
  labs(title = "Violin Plot: Physical Health by Sex",x = "Sex",y = "Physical Health") +
  theme_minimal()

