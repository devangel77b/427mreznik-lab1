library(ggplot2)
library(dplyr)
library(broom)

raw <- read.csv('timesAll.csv',header=TRUE)
data <- tibble(raw)
# this has 5 replicates for each type

model1 <- lm(y~I(t^2),data)
model2 <- lm(y~I(t^2):type,data)
print(anova(model1,model2))



grouped <- group_by(data,type)
results <- grouped %>%
	group_modify(~ broom::tidy(lm(y~I(t^2):as.factor(trial), data = .x)))

print(results)


data2 <- tibble(read.csv('slopes.csv',header=TRUE))
model3 <- aov(slope~as.factor(type),data2)
print(summary(model3))
