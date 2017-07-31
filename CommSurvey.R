# ANALYSIS of Community Survey
# 1,500 surveys sent out, ~300 responses


# Read in file
survey <- read.csv("Kzoo Final Comm Survey.csv", head = T, sep = ",", stringsAsFactors = T)

complete <- na.omit(survey)

View(complete)
View(survey)
str(survey)

# Mutate to factors
library(dplyr)
surveyf <- survey %>% mutate_if(is.integer, as.factor)


# Analysis of full dataset first
require(ggplot2)
require(ggthemes)



## q2a --> Overall feeling of safety in Kalamazoo, (1 - Excellent, 2 = good, 3 = fair, 4 = poor)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q2areg <- lm(q2a ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
names(surveyf)
summary(q2areg)

# Race (-), length of res - , health +, and willingness to vote locally - impact feeling of safety in Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q2a)), aes(x = age, fill = q2a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Overall feeling of safety in Kalamazoo (1 = Excellent, 4 = Poor)") 
# Younger pop is more likely to feel "unsafe"

# Calc numbers for each subset of age, Q2a
age1 <- subset(surveyf, age == 1)
age2 <- subset(surveyf, age == 2)
age3 <- subset(surveyf, age == 3)
q2a1 <- table(age1$q2a)
q2a2 <- table(age2$q2a)
q2a3 <- table(age3$q2a)
# Calc %s
# Age = 1
out <- NULL
for(i in 1:length(q2a1)){
  results <- q2a1[i]/sum(q2a1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q2a1)
out <- rbind(out, q2a1)
out
# Age = 2
out2 <- NULL
for(i in 1:length(q2a2)){
  results <- q2a2[i]/sum(q2a2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q2a2)
out2 <- rbind(out2, q2a2)
out2
# Age = 3
out3 <- NULL
for(i in 1:length(q2a3)){
  results <- q2a3[i]/sum(q2a3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q2a3)
out3 <- rbind(out3, q2a3)
out3

q2abyage <- rbind(out, out2, out3)
q2abyage$Age <- c(1, 1, 2, 2, 3, 3)
q2abyage

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q2a)), aes(x = aWhite, fill = q2a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Overall feeling of safety in Kalamazoo (1 = Excellent, 4 = Poor)") 
# Nonwhite pop has much higher share feeling "unsafe" 

# Calc numbers for each subset of race, Q15a
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq2a1 <- table(race1$q2a)
raceq2a2 <- table(race2$q2a)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq2a1)){
  results <- raceq2a1[i]/sum(raceq2a1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq2a1)
out <- rbind(out, raceq2a1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq2a2)){
  results <- raceq2a2[i]/sum(raceq2a2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq2a2)
out2 <- rbind(out2, raceq2a2)
out2

q2abyRace <- rbind(out, out2)
q2abyRace$Race <- c(0, 0, 1, 1)
q2abyRace

# Breakdown of results by sex (0 = female, 1 = male)
ggplot(subset(surveyf, !is.na(sex) & !is.na(q2a)), aes(x = sex, fill = q2a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "GENDER (0 = Female, 1 = Male)", y = "Percent", title = "Overall feeling of safety in Kalamazoo (1 = Excellent, 4 = Poor)") 
# Roughly the same, no additional table


# Breakdown of results by Income (1 = < $25k, 2 = $25 - $50, 3 = $50-99, 4 = $100-150, 5 = > $150K)
ggplot(subset(surveyf, !is.na(income) & !is.na(q2a)), aes(x = income, fill = q2a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "INCOME (1 = <$25K, 5 = >$150K)", y = "Percent", title = "Overall feeling of safety in Kalamazoo (1 = Excellent, 4 = Poor)") 
# Low income (2, 1) are much more likely to feel unsafe

# Calc numbers for each subset of Income, Q2a
income1 <- subset(surveyf, income == 1)
income2 <- subset(surveyf, income == 2)
income3 <- subset(surveyf, income == 3)
income4 <- subset(surveyf, income == 4)
income5 <- subset(surveyf, income == 5)
incomeq2a1 <- table(income1$q2a)
incomeq2a2 <- table(income2$q2a)
incomeq2a3 <- table(income3$q2a)
incomeq2a4 <- table(income4$q2a)
incomeq2a5 <- table(income5$q2a)

# Calc %s
# Income = 1
out <- NULL
for(i in 1:length(incomeq2a1)){
  results <- incomeq2a1[i]/sum(incomeq2a1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(incomeq2a1)
out <- rbind(out, incomeq2a1)
out
# Income = 2
out2 <- NULL
for(i in 1:length(incomeq2a2)){
  results <- incomeq2a2[i]/sum(incomeq2a2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(incomeq2a2)
out2 <- rbind(out2, incomeq2a2)
out2
# Income = 3
out3 <- NULL
for(i in 1:length(incomeq2a3)){
  results <- incomeq2a3[i]/sum(incomeq2a3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(incomeq2a3)
out3 <- rbind(out3, incomeq2a3)
out3
# Income = 4
out4 <- NULL
for(i in 1:length(incomeq2a4)){
  results <- incomeq2a4[i]/sum(incomeq2a4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(incomeq2a4)
out4 <- rbind(out4, incomeq2a4)
out4
# Income = 5
out5 <- NULL
for(i in 1:length(incomeq2a5)){
  results <- incomeq2a5[i]/sum(incomeq2a5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(incomeq2a5)
out5 <- rbind(out5, incomeq2a5)
out5

q2abyIncome <- rbind(out, out2, out3, out4, out5)
q2abyIncome$Income <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q2abyIncome


# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q2a)), aes(x = LengthofRes, fill = q2a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Overall feeling of safety in Kalamazoo (1 = Excellent, 4 = Poor)") 
# newer residents feel more unsafe

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q2a)), aes(x = Health, fill = q2a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Overall feeling of safety in Kalamazoo (1 = Excellent, 4 = Poor)") 
# People in poor health have a much harder time getting to where they need to go

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q2a)), aes(x = VoteLocal, fill = q2a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "Overall feeling of safety in Kalamazoo (1 = Excellent, 4 = Poor)") 
# The more likely you are to vote, the more likely you are to attend public meetings





## q2b --> Overall ease of getting to places you usually have to visit (1 - Excellent, 2 = good, 3 = fair, 4 = poor)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q2breg <- lm(q2b ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q2breg)

# Health has major impact on ease of getting to places usually have to visit

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q2b)), aes(x = age, fill = q2b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Overall ease of getting to places you visit (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Younger pop is actually more likely to say "fair/poor" for getting to places they need to visit

# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q2b)), aes(x = LengthofRes, fill = q2b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Overall ease of getting to places you visit (1 = Excellent, 4 = Poor)") 
# newer residents feel more unsafe

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q2b)), aes(x = aWhite, fill = q2b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Overall ease of getting to places you visit (1 = Excellent, 4 = Poor)") 
# Nonwhite pop has much higher share feeling "unsafe" 

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q2b)), aes(x = Health, fill = q2b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Overall ease of getting to places you visit (1 = Excellent, 4 = Poor, 5 = N/A)") 
# People in poor health have a much harder time getting to where they need to go

# Calc numbers for each subset of health, Q2b
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q2b1 <- table(health1$q2b)
q2b2 <- table(health2$q2b)
q2b3 <- table(health3$q2b)
q2b4 <- table(health4$q2b)
q2b5 <- table(health5$q2b)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q2b1)){
  results <- q2b1[i]/sum(q2b1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q2b1)
out <- rbind(out, q2b1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q2b2)){
  results <- q2b2[i]/sum(q2b2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q2b2)
out2 <- rbind(out2, q2b2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q2b3)){
  results <- q2b3[i]/sum(q2b3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q2b3)
out3 <- rbind(out3, q2b3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q2b4)){
  results <- q2b4[i]/sum(q2b4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q2b4)
out4 <- rbind(out4, q2b4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q2b5)){
  results <- q2b5[i]/sum(q2b5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q2b5)
out5 <- rbind(out5, q2b5)
out5


q2bbyhealth <- rbind(out, out2, out3, out4, out5)
q2bbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q2bbyhealth



## q2c --> Overall quality of natural environment in Kalamazoo (1 - Excellent, 2 = good, 3 = fair, 4 = poor)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q2creg <- lm(q2c ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q2creg)

# Health, race, and age have impact on view of natural environment in Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q2c)), aes(x = age, fill = q2c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Overall quality of Kalamazoo's natural environment (1 = Excellent, 4 = Poor)") 
# Older pop is actually more likely to say Excellent or Good

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q2c)), aes(x = Health, fill = q2c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Overall quality of Kalamazoo's natural environment (1 = Excellent, 4 = Poor)") 
# People in poor health have much worse views of the natural env in Kzoo

# Calc numbers for each subset of health, Q2c
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q2c1 <- table(health1$q2c)
q2c2 <- table(health2$q2c)
q2c3 <- table(health3$q2c)
q2c4 <- table(health4$q2c)
q2c5 <- table(health5$q2c)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q2c1)){
  results <- q2c1[i]/sum(q2c1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q2c1)
out <- rbind(out, q2c1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q2c2)){
  results <- q2c2[i]/sum(q2c2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q2c2)
out2 <- rbind(out2, q2c2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q2c3)){
  results <- q2c3[i]/sum(q2c3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q2c3)
out3 <- rbind(out3, q2c3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q2c4)){
  results <- q2c4[i]/sum(q2c4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q2c4)
out4 <- rbind(out4, q2c4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q2c5)){
  results <- q2c5[i]/sum(q2c5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q2c5)
out5 <- rbind(out5, q2c5)
out5


q2cbyhealth <- rbind(out, out2, out3, out4, out5)
q2cbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q2cbyhealth


# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q2c)), aes(x = aWhite, fill = q2c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Overall quality of Kalamazoo's natural environment (1 = Excellent, 4 = Poor)") 
# Nonwhite pop has much higher share seeing natural environment as fair or poor 

# Calc numbers for each subset of race, Q2c
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq2c1 <- table(race1$q2c)
raceq2c2 <- table(race2$q2c)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq2c1)){
  results <- raceq2c1[i]/sum(raceq2c1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq2c1)
out <- rbind(out, raceq2c1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq2c2)){
  results <- raceq2c2[i]/sum(raceq2c2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq2c2)
out2 <- rbind(out2, raceq2c2)
out2

q2cbyRace <- rbind(out, out2)
q2cbyRace$Race <- c(0, 0, 1, 1)
q2cbyRace




## q2d --> Overall quality of built environment in Kalamazoo (1 - Excellent, 2 = good, 3 = fair, 4 = poor)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q2dreg <- lm(q2d ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q2dreg)

# Health, sex, and age have impact on view of built environment in Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q2d)), aes(x = age, fill = q2d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Overall quality of Kalamazoo's built environment (1 = Excellent, 4 = Poor)") 
# Older pop is actually more likely to say Excellent or Good

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q2d)), aes(x = Health, fill = q2d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Overall quality of Kalamazoo's built environment (1 = Excellent, 4 = Poor, 5 = N/A)") 
# People in poor health have much worse views of the built env in Kzoo

# Calc numbers for each subset of health, Q2c
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q2c1 <- table(health1$q2c)
q2c2 <- table(health2$q2c)
q2c3 <- table(health3$q2c)
q2c4 <- table(health4$q2c)
q2c5 <- table(health5$q2c)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q2c1)){
  results <- q2c1[i]/sum(q2c1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q2c1)
out <- rbind(out, q2c1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q2c2)){
  results <- q2c2[i]/sum(q2c2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q2c2)
out2 <- rbind(out2, q2c2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q2c3)){
  results <- q2c3[i]/sum(q2c3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q2c3)
out3 <- rbind(out3, q2c3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q2c4)){
  results <- q2c4[i]/sum(q2c4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q2c4)
out4 <- rbind(out4, q2c4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q2c5)){
  results <- q2c5[i]/sum(q2c5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q2c5)
out5 <- rbind(out5, q2c5)
out5


q2cbyhealth <- rbind(out, out2, out3, out4, out5)
q2cbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q2cbyhealth


# Breakdown of results by sex (0 = F, 1 = M)
ggplot(subset(surveyf, !is.na(sex) & !is.na(q2d)), aes(x = sex, fill = q2d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "SEX (0 = female, 1 = male)", y = "Percent", title = "Overall quality of Kalamazoo's built environment (1 = Excellent, 4 = Poor)") 
# Females are more likely to say poor or fair built environment





## q2e --> Health and wellness opportunities in Kalamazoo (1 - Excellent, 2 = good, 3 = fair, 4 = poor)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q2ereg <- lm(q2e ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q2ereg)

# Health, race, and age have impact on view of health/wellness opps in Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q2e)), aes(x = age, fill = q2e)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Rate health/wellness opportunities in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Older pop is more likely to say Excellent or Good

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q2e)), aes(x = Health, fill = q2e)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Rate health/wellness opportunities in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# People in poor health have much worse views of the health/wellness opps here

# Calc numbers for each subset of health, Q2e
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q2e1 <- table(health1$q2e)
q2e2 <- table(health2$q2e)
q2e3 <- table(health3$q2e)
q2e4 <- table(health4$q2e)
q2e5 <- table(health5$q2e)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q2e1)){
  results <- q2e1[i]/sum(q2e1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q2e1)
out <- rbind(out, q2e1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q2e2)){
  results <- q2e2[i]/sum(q2e2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q2e2)
out2 <- rbind(out2, q2e2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q2e3)){
  results <- q2e3[i]/sum(q2e3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q2e3)
out3 <- rbind(out3, q2e3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q2e4)){
  results <- q2e4[i]/sum(q2e4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q2e4)
out4 <- rbind(out4, q2e4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q2e5)){
  results <- q2e5[i]/sum(q2e5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q2e5)
out5 <- rbind(out5, q2e5)
out5


q2ebyhealth <- rbind(out, out2, out3, out4, out5)
q2ebyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q2ebyhealth


# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q2e)), aes(x = aWhite, fill = q2e)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Rate health/wellness opportunities in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Nonwhite pop much less likely to rate as "excellent" or "good" 

# Calc numbers for each subset of race, Q2e
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq2e1 <- table(race1$q2e)
raceq2e2 <- table(race2$q2e)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq2e1)){
  results <- raceq2e1[i]/sum(raceq2e1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq2e1)
out <- rbind(out, raceq2e1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq2e2)){
  results <- raceq2e2[i]/sum(raceq2e2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq2e2)
out2 <- rbind(out2, raceq2e2)
out2

q2ebyRace <- rbind(out, out2)
q2ebyRace$Race <- c(0, 0, 1, 1)
q2ebyRace




## q2f --> Education and Enrichment opportunities in Kalamazoo (1 - Excellent, 2 = good, 3 = fair, 4 = poor)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q2freg <- lm(q2f ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q2freg)

# Age, race, length of res, and health have impact on view of education/enrichment opps in Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q2f)), aes(x = age, fill = q2f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Rate education/enrichment opportunities in Kzoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Older pop is more likely to say Excellent or Good

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q2f)), aes(x = Health, fill = q2f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Rate education/enrichment opportunities in Kzoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# People in poor health have much worse views of the education/enrichm opps here

# Calc numbers for each subset of health, Q2f
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q2f1 <- table(health1$q2f)
q2f2 <- table(health2$q2f)
q2f3 <- table(health3$q2f)
q2f4 <- table(health4$q2f)
q2f5 <- table(health5$q2f)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q2f1)){
  results <- q2f1[i]/sum(q2f1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q2f1)
out <- rbind(out, q2f1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q2f2)){
  results <- q2f2[i]/sum(q2f2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q2f2)
out2 <- rbind(out2, q2f2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q2f3)){
  results <- q2f3[i]/sum(q2f3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q2f3)
out3 <- rbind(out3, q2f3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q2f4)){
  results <- q2f4[i]/sum(q2f4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q2f4)
out4 <- rbind(out4, q2f4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q2f5)){
  results <- q2f5[i]/sum(q2f5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q2f5)
out5 <- rbind(out5, q2f5)
out5


q2fbyhealth <- rbind(out, out2, out3, out4, out5)
q2fbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q2fbyhealth


# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q2f)), aes(x = aWhite, fill = q2f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Rate education/enrichment opportunities in Kzoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Nonwhite pop much less likely to rate as "excellent" or "good" 

# Calc numbers for each subset of race, Q2e
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq2f1 <- table(race1$q2f)
raceq2f2 <- table(race2$q2f)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq2f1)){
  results <- raceq2f1[i]/sum(raceq2f1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq2f1)
out <- rbind(out, raceq2f1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq2f2)){
  results <- raceq2f2[i]/sum(raceq2f2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq2f2)
out2 <- rbind(out2, raceq2f2)
out2

q2fbyRace <- rbind(out, out2)
q2fbyRace$Race <- c(0, 0, 1, 1)
q2fbyRace

# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q2f)), aes(x = LengthofRes, fill = q2f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Rate education/enrichment opportunities in Kzoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Interesting dynamic, long term res more likely to say "excellent", but short term less likely to say "poor"





## q2g --> Overall economic health in Kalamazoo (1 - Excellent, 2 = good, 3 = fair, 4 = poor)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q2greg <- lm(q2g ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q2greg)

# Age, sex, race, length of res, and health have impact on view of economic health in Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q2g)), aes(x = age, fill = q2g)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Overall economic health in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Older pop is more likely to say Excellent or Good

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q2g)), aes(x = Health, fill = q2g)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Overall economic health in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# People in poor health have worse views of economic health 

# Breakdown of results by sex (0 = F, 1= M)
ggplot(subset(surveyf, !is.na(sex) & !is.na(q2g)), aes(x = sex, fill = q2g)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "SEX (0 = female, 1 = male)", y = "Percent", title = "Overall economic health in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Female slightly morel ikely to say "poor" economic health

# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q2g)), aes(x = LengthofRes, fill = q2g)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Overall economic health in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Newer residents are less likely to say economic health is excellent or good

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q2g)), aes(x = aWhite, fill = q2g)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Overall economic health in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Nonwhite pop much less likely to rate economic health as "excellent" or "good" 

# Calc numbers for each subset of race, Q2e
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq2g1 <- table(race1$q2g)
raceq2g2 <- table(race2$q2g)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq2g1)){
  results <- raceq2g1[i]/sum(raceq2g1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq2g1)
out <- rbind(out, raceq2g1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq2g2)){
  results <- raceq2g2[i]/sum(raceq2g2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq2g2)
out2 <- rbind(out2, raceq2g2)
out2

q2gbyRace <- rbind(out, out2)
q2gbyRace$Race <- c(0, 0, 1, 1)
q2gbyRace




## q2h --> Sense of community in Kalamazoo (1 - Excellent, 2 = good, 3 = fair, 4 = poor)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q2hreg <- lm(q2h ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q2hreg)

# Age, race, local voters, and health have impact on view of sense of community in Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q2h)), aes(x = age, fill = q2h)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Sense of community in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Older pop is more likely to feel sense of community is excellent or good

# Calc numbers for each subset of age, Q2h
age1 <- subset(surveyf, age == 1)
age2 <- subset(surveyf, age == 2)
age3 <- subset(surveyf, age == 3)
q2h1 <- table(age1$q2h)
q2h2 <- table(age2$q2h)
q2h3 <- table(age3$q2h)
# Calc %s
# Age = 1
out <- NULL
for(i in 1:length(q2h1)){
  results <- q2h1[i]/sum(q2h1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q2h1)
out <- rbind(out, q2h1)
out
# Age = 2
out2 <- NULL
for(i in 1:length(q2h2)){
  results <- q2h2[i]/sum(q2h2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q2h2)
out2 <- rbind(out2, q2h2)
out2
# Age = 3
out3 <- NULL
for(i in 1:length(q2h3)){
  results <- q2h3[i]/sum(q2h3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q2h3)
out3 <- rbind(out3, q2h3)
out3

q2hbyage <- rbind(out, out2, out3)
q2hbyage$Age <- c(1, 1, 2, 2, 3, 3)
q2hbyage


# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q2h)), aes(x = aWhite, fill = q2h)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Sense of community in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Nonwhite pop much less likely to rate sense of community as "excellent" or "good" 

# Calc numbers for each subset of race, Q2h
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq2h1 <- table(race1$q2h)
raceq2h2 <- table(race2$q2h)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq2h1)){
  results <- raceq2h1[i]/sum(raceq2h1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq2h1)
out <- rbind(out, raceq2h1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq2h2)){
  results <- raceq2h2[i]/sum(raceq2h2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq2h2)
out2 <- rbind(out2, raceq2h2)
out2

q2hbyRace <- rbind(out, out2)
q2hbyRace$Race <- c(0, 0, 1, 1)
q2hbyRace


# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q2h)), aes(x = VoteLocal, fill = q2h)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "Sense of community in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# People who vote more often are much more likely to rate sense of community as excellent or good

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q2h)), aes(x = Health, fill = q2h)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Sense of community in Kalamazoo (1 = Excellent, 4 = Poor, 5 = N/A)") 
# People in good health more likely to say sense of community is excellent or good




## q3b --> Remain in Kalamazoo for next 5 years (1 - very likely, 2 = somewhat likely, 3 = somewhat unlikely, 4 = very unlikely, 5 = don't know)

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q3breg <- lm(q3b ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q3breg)

# Tenure, age, length of res, and health have impact on likelihood to stay in Kzoo

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q3b)), aes(x = tenure, fill = q3b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "Odds to remain in Kzoo for 5 years (1 = very likely, 4 = very unlikely, 5 = Not Sure)") 
# Owner pop much more likely to say "very likely" staying in kzoo for next 5 years 

# Calc numbers for each subset of tenure, Q3b
tenure1 <- subset(surveyf, tenure == 1)
tenure2 <- subset(surveyf, tenure == 2)
tenureq3b1 <- table(tenure1$q3b)
tenureq3b2 <- table(tenure2$q3b)
# Calc %s
# Tenure = 1
out <- NULL
for(i in 1:length(tenureq3b1)){
  results <- tenureq3b1[i]/sum(tenureq3b1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(tenureq3b1)
out <- rbind(out, tenureq3b1)
out
# Tenure = 2
out2 <- NULL
for(i in 1:length(tenureq3b2)){
  results <- tenureq3b2[i]/sum(tenureq3b2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(tenureq3b2)
out2 <- rbind(out2, tenureq3b2)
out2

q3bbyTenure <- rbind(out, out2)
q3bbyTenure$Tenure <- c(1, 1, 2, 2)
q3bbyTenure

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q3b)), aes(x = age, fill = q3b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Odds to remain in Kzoo for 5 years (1 = very likely, 4 = very unlikely, 5 = Not Sure)") 
# Older pop, greater share saying very likely staying in Kzoo for 5 years

# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q3b)), aes(x = LengthofRes, fill = q3b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Odds to remain in Kzoo for 5 years (1 = very likely, 4 = very unlikely, 5 = Not Sure)") 
# Long term residents are more likely to be planning to stay in Kzoo for 5 years

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q3b)), aes(x = Health, fill = q3b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Odds to remain in Kzoo for 5 years (1 = very likely, 4 = very unlikely, 5 = Not Sure)") 
# People in good health more likely to be planning to stay in Kzoo for 5 years

# Calc numbers for each subset of health, Q3b
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q3b1 <- table(health1$q3b)
q3b2 <- table(health2$q3b)
q3b3 <- table(health3$q3b)
q3b4 <- table(health4$q3b)
q3b5 <- table(health5$q3b)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q3b1)){
  results <- q3b1[i]/sum(q3b1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q3b1)
out <- rbind(out, q3b1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q3b2)){
  results <- q3b2[i]/sum(q3b2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q3b2)
out2 <- rbind(out2, q3b2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q3b3)){
  results <- q3b3[i]/sum(q3b3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q3b3)
out3 <- rbind(out3, q3b3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q3b4)){
  results <- q3b4[i]/sum(q3b4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q3b4)
out4 <- rbind(out4, q3b4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q3b5)){
  results <- q3b5[i]/sum(q3b5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q3b5)
out5 <- rbind(out5, q3b5)
out5


q3bbyhealth <- rbind(out, out2, out3, out4, out5)
q3bbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q3bbyhealth




## q4a --> How safe do you feel in your neighborhood during the day (1 = very safe, 2 = somewhat safe, 3 = neither safe nor unsafe, 4 = somewhat unsafe, 5 = very unsafe) )

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q4areg <- lm(q4a ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q4areg)

# Health and tenure have impact on feelings of safety in one's neighborhood

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q4a)), aes(x = tenure, fill = q4a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "How safe do you feel in your neighborhood (1 = very safe, 5 = very unsafe)") 
# Renter pop much more likely to say "very unsafe" 

# Calc numbers for each subset of tenure, Q3b
tenure1 <- subset(surveyf, tenure == 1)
tenure2 <- subset(surveyf, tenure == 2)
tenureq4a1 <- table(tenure1$q4a)
tenureq4a2 <- table(tenure2$q4a)
# Calc %s
# Tenure = 1
out <- NULL
for(i in 1:length(tenureq4a1)){
  results <- tenureq4a1[i]/sum(tenureq4a1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(tenureq4a1)
out <- rbind(out, tenureq4a1)
out
# Tenure = 2
out2 <- NULL
for(i in 1:length(tenureq4a2)){
  results <- tenureq4a2[i]/sum(tenureq4a2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(tenureq4a2)
out2 <- rbind(out2, tenureq4a2)
out2

q4abyTenure <- rbind(out, out2)
q4abyTenure$Tenure <- c(1, 1, 2, 2)
q4abyTenure


# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q4a)), aes(x = Health, fill = q4a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "How safe do you feel in your neighborhood (1 = very safe, 5 = very unsafe)") 
# People in good health more likely feel very safe





## q5j --> How would you rate the overall appearance of Kalamazoo? (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q5jreg <- lm(q5j ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q5jreg)

# Age, Race, length of res, and health have impact on overall appearance of Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q5j)), aes(x = age, fill = q5j)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Please rate the overall apperance of Kzoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Older pop, greater share saying excellent or good

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q5j)), aes(x = aWhite, fill = q5j)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Please rate the overall apperance of Kzoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Nonwhite pop much less likely to rate appearance of community as "excellent" or "good" 

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q5j)), aes(x = Health, fill = q5j)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Please rate the overall apperance of Kzoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# People in poor health are much less likely to rate appearance as excellent or good
# Calc numbers for each subset of health, Q5j
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q5j1 <- table(health1$q5j)
q5j2 <- table(health2$q5j)
q5j3 <- table(health3$q5j)
q5j4 <- table(health4$q5j)
q5j5 <- table(health5$q5j)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q5j1)){
  results <- q5j1[i]/sum(q5j1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q5j1)
out <- rbind(out, q5j1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q5j2)){
  results <- q5j2[i]/sum(q5j2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q5j2)
out2 <- rbind(out2, q5j2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q5j3)){
  results <- q5j3[i]/sum(q5j3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q5j3)
out3 <- rbind(out3, q5j3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q5j4)){
  results <- q5j4[i]/sum(q5j4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q5j4)
out4 <- rbind(out4, q5j4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q5j5)){
  results <- q5j5[i]/sum(q5j5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q5j5)
out5 <- rbind(out5, q5j5)
out5


q5jbyhealth <- rbind(out, out2, out3, out4, out5)
q5jbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q5jbyhealth

# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q5j)), aes(x = LengthofRes, fill = q5j)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Please rate the overall apperance of Kzoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Long term residents are more likely to say excellent or good






## q5k --> Public places where people want to spend time (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q5kreg <- lm(q5k ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q5kreg)

# Health and vote local have impact quality of public spaces / places where people want to spend time
# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q5k)), aes(x = Health, fill = q5k)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Quality of public spaces (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Interesting relationship, good health and poor health are largest share of "excellent"

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q5k)), aes(x = VoteLocal, fill = q5k)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "Quality of public spaces (1 = excellent, 4 = poor, 5 = Don't Know)") 
# People who vote more likely to rate public spaces as fair or poor

# Calc numbers for each subset of vote local, Q5k
votelocal1 <- subset(surveyf, VoteLocal == 1)
votelocal2 <- subset(surveyf, VoteLocal == 2)
votelocal3 <- subset(surveyf, VoteLocal == 3)
votelocal4 <- subset(surveyf, VoteLocal == 4)
votelocal5 <- subset(surveyf, VoteLocal == 5)
q5k1 <- table(votelocal1$q5k)
q5k2 <- table(votelocal2$q5k)
q5k3 <- table(votelocal3$q5k)
q5k4 <- table(votelocal4$q5k)
q5k5 <- table(votelocal5$q5k)
# Calc %s
# Vote local = 1
out <- NULL
for(i in 1:length(q5k1)){
  results <- q5k1[i]/sum(q5k1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q5k1)
out <- rbind(out, q5k1)
out
# Vote local = 2
out2 <- NULL
for(i in 1:length(q5k2)){
  results <- q5k2[i]/sum(q5k2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q5k2)
out2 <- rbind(out2, q5k2)
out2
# Vote local = 3
out3 <- NULL
for(i in 1:length(q5k3)){
  results <- q5k3[i]/sum(q5k3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q5k3)
out3 <- rbind(out3, q5k3)
out3
# Vote local = 4
out4 <- NULL
for(i in 1:length(q5k4)){
  results <- q5k4[i]/sum(q5k4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q5k4)
out4 <- rbind(out4, q5k4)
out4
# Vote local = 5
out5 <- NULL
for(i in 1:length(q5k5)){
  results <- q5k5[i]/sum(q5k5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q5k5)
out5 <- rbind(out5, q5k5)
out5


q5kbyvotelocal <- rbind(out, out2, out3, out4, out5)
q5kbyvotelocal$VoteLocal <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q5kbyvotelocal





## q5l --> Variety of housing options (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q5lreg <- lm(q5l ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q5lreg)

# Health and sex have impact on variety of housing options in Kzoo
# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q5l)), aes(x = Health, fill = q5l)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Quality of housing options (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Poor health pop more likely to say "poor"

# Breakdown of results by sex (0 = female, 1 = male)
ggplot(subset(surveyf, !is.na(sex) & !is.na(q5l)), aes(x = sex, fill = q5l)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "GENDER (0 = Female, 1 = Male)", y = "Percent", title = "Quality of housing options (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Roughly the same, no additional table






## q5m --> Availability of affordable quality housing (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q5mreg <- lm(q5m ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q5mreg)

# Tenure, sex, and health have impact on views of affordable quality housing in Kzoo

# Breakdown of results by sex (0 = female, 1 = male)
ggplot(subset(surveyf, !is.na(sex) & !is.na(q5m)), aes(x = sex, fill = q5m)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "GENDER (0 = Female, 1 = Male)", y = "Percent", title = "Availability of affordable quality housing (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Roughly the same, no additional table

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q5m)), aes(x = tenure, fill = q5m)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "Availability of affordable quality housing (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Renter pop much more likely to say "poor" 

# Calc numbers for each subset of tenure, Q5m
tenure1 <- subset(surveyf, tenure == 1)
tenure2 <- subset(surveyf, tenure == 2)
tenureq5m1 <- table(tenure1$q5m)
tenureq5m2 <- table(tenure2$q5m)
# Calc %s
# Tenure = 1
out <- NULL
for(i in 1:length(tenureq5m1)){
  results <- tenureq5m1[i]/sum(tenureq5m1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(tenureq5m1)
out <- rbind(out, tenureq5m1)
out
# Tenure = 2
out2 <- NULL
for(i in 1:length(tenureq5m2)){
  results <- tenureq5m2[i]/sum(tenureq5m2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(tenureq5m2)
out2 <- rbind(out2, tenureq5m2)
out2

q5mbyTenure <- rbind(out, out2)
q5mbyTenure$Tenure <- c(1, 1, 2, 2)
q5mbyTenure

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q5m)), aes(x = Health, fill = q5m)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Availability of affordable quality housing (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Poor health pop more likely to say "poor"





## q5p --> Availability of affordable quality food (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q5preg <- lm(q5p ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q5preg)

# Age, race, health, tenure have impact on affordable quality food perceptions in Kzoo

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q5p)), aes(x = age, fill = q5p)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Availability of affordable quality food (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Older pop is more likely to feel sense of community is excellent or good

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q5p)), aes(x = aWhite, fill = q5p)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Availability of affordable quality food (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop much less likely to rate sense of community as "excellent" or "good" 

# Calc numbers for each subset of race, Q5p
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq5p1 <- table(race1$q5p)
raceq5p2 <- table(race2$q5p)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq5p1)){
  results <- raceq5p1[i]/sum(raceq5p1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq5p1)
out <- rbind(out, raceq5p1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq5p2)){
  results <- raceq5p2[i]/sum(raceq5p2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq5p2)
out2 <- rbind(out2, raceq5p2)
out2

q5pbyRace <- rbind(out, out2)
q5pbyRace$Race <- c(0, 0, 1, 1)
q5pbyRace

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q5p)), aes(x = Health, fill = q5p)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Availability of affordable quality food (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Poor health pop more likely to say "poor"

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q5p)), aes(x = tenure, fill = q5p)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "Availability of affordable quality food (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Renter pop more likely to say "poor", not much difference





## q5q --> Availability of affordable quality health care (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q5qreg <- lm(q5q ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q5qreg)

# Age, Race, and health have impact on affordable quality healthcare in Kzoo

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q5q)), aes(x = Health, fill = q5q)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Availability of affordable quality healthcare (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Poor health pop more likely to say "poor"

# Calc numbers for each subset of health, Q5q
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q5q1 <- table(health1$q5q)
q5q2 <- table(health2$q5q)
q5q3 <- table(health3$q5q)
q5q4 <- table(health4$q5q)
q5q5 <- table(health5$q5q)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q5q1)){
  results <- q5q1[i]/sum(q5q1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q5q1)
out <- rbind(out, q5q1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q5q2)){
  results <- q5q2[i]/sum(q5q2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q5q2)
out2 <- rbind(out2, q5q2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q5q3)){
  results <- q5q3[i]/sum(q5q3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q5q3)
out3 <- rbind(out3, q5q3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q5q4)){
  results <- q5q4[i]/sum(q5q4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q5q4)
out4 <- rbind(out4, q5q4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q5q5)){
  results <- q5q5[i]/sum(q5q5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q5q5)
out5 <- rbind(out5, q5q5)
out5


q5qbyhealth <- rbind(out, out2, out3, out4, out5)
q5qbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q5qbyhealth

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q5q)), aes(x = aWhite, fill = q5q)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Availability of affordable quality healthcare (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop much less likely to rate affordable healthcare "excellent" or "good" 

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q5q)), aes(x = age, fill = q5q)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Availability of affordable quality healthcare (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Younger pop less likely to say excellent or good






## q5r --> Availability of preventative health services (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q5rreg <- lm(q5r ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q5rreg)

# Age, race, tenure, length of res, and health have impact on availability of prevent health services in Kzoo
# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q5r)), aes(x = age, fill = q5r)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Availability of preventative health services (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Tough, lots of don't knows

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q5r)), aes(x = aWhite, fill = q5r)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Availability of preventative health services (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop much less likely to rate preventative health services as "excellent" or "good" 
# Calc numbers for each subset of race, Q5r
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq5r1 <- table(race1$q5r)
raceq5r2 <- table(race2$q5r)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq5r1)){
  results <- raceq5r1[i]/sum(raceq5r1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq5r1)
out <- rbind(out, raceq5r1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq5r2)){
  results <- raceq5r2[i]/sum(raceq5r2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq5r2)
out2 <- rbind(out2, raceq5r2)
out2

q5rbyRace <- rbind(out, out2)
q5rbyRace$Race <- c(0, 0, 1, 1)
q5rbyRace

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q5r)), aes(x = tenure, fill = q5r)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "Availability of preventative health services (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Renter pop more likely to say "poor" or "fair", but not huge diff

# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q5r)), aes(x = LengthofRes, fill = q5r)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Availability of preventative health services (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Lots of don't knows, makes it difficult. But general trend is longer term res feel better

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q5r)), aes(x = Health, fill = q5r)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Availability of preventative health services (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Poor health pop more likely to say "poor"






## q5s --> Availability of affordable quality mental health care (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q5sreg <- lm(q5s ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q5sreg)

# Length of res, tenure have impact on mental health care perceptions of Kzoo

# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q5s)), aes(x = LengthofRes, fill = q5s)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Availability of quality mental health services (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Lots of don't knows, makes it difficult. But general trend is longer term res feel better

# LOTS OF DON'T KNOWS





## q6a --> Availability of affordable quality child care/preschool (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6areg <- lm(q6a ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6areg)

# Length of res has impact on quality child care perceptions of Kzoo
# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q6a)), aes(x = LengthofRes, fill = q6a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Availability of quality child care / pre-K (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Lots of don't knows, makes it difficult. But general trend is longer term res feel better
# lots of don't knows

# MInus length of res, now race is important
q6areg <- lm(q6a ~ age + sex + aWhite + tenure + income + Health + VoteLocal, data = survey)
summary(q6areg)

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q6a)), aes(x = aWhite, fill = q6a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Availability of quality child care / pre-K (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop much less likely to rate preventative health services as "excellent" or "good" 
# LOTS OF Don't KNOWS





## q6b --> Quality of K-12 education (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6breg <- lm(q6b ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6breg)

# Tenure, income, length of res, and vote local have impact on K-12 perceptions in Kzoo
# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q6b)), aes(x = LengthofRes, fill = q6b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "Quality of K-12 education in Kalamazoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Lots of don't knows, makes it difficult. But general trend is longer term res feel better

# Breakdown of results by Income (1 = < $25k, 2 = $25 - $50, 3 = $50-99, 4 = $100-150, 5 = > $150K)
ggplot(subset(surveyf, !is.na(income) & !is.na(q6b)), aes(x = income, fill = q6b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "INCOME (1 = <$25K, 5 = >$150K)", y = "Percent", title = "Quality of K-12 education in Kalamazoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# No major diff

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q6b)), aes(x = tenure, fill = q6b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "Quality of K-12 education in Kalamazoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# No major diff

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q6b)), aes(x = VoteLocal, fill = q6b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "Quality of K-12 education in Kalamazoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# The more likely you are to vote, the more likely you are to attend public meetings




## q6c --> Quality of adult education opportunities  (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6creg <- lm(q6c ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6creg)

# Age, tenure, and health have impact on adult ed perception in Kzoo
# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q6c)), aes(x = age, fill = q6c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Quality of adult education opportunities (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Tough, lots of don't knows

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q6c)), aes(x = tenure, fill = q6c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "Quality of adult education opportunities (1 = excellent, 4 = poor, 5 = Don't Know)") 
# No major diff

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q6c)), aes(x = Health, fill = q6c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Quality of adult education opportunities (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Good health pop more likely to say "excellent" or "good"






## q6d --> Opportunities to attend cultural activities (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6dreg <- lm(q6d ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6dreg)

# Race, income, and health have impact on perceptions of cultural activities in Kzoo

# Breakdown of results by Income (1 = < $25k, 2 = $25 - $50, 3 = $50-99, 4 = $100-150, 5 = > $150K)
ggplot(subset(surveyf, !is.na(income) & !is.na(q6d)), aes(x = income, fill = q6d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "INCOME (1 = <$25K, 5 = >$150K)", y = "Percent", title = "Opportunities to attend cultural activities (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Higher income much more likely to say excellent or good
# Calc numbers for each subset of Income, Q6d
income1 <- subset(surveyf, income == 1)
income2 <- subset(surveyf, income == 2)
income3 <- subset(surveyf, income == 3)
income4 <- subset(surveyf, income == 4)
income5 <- subset(surveyf, income == 5)
incomeq6d1 <- table(income1$q6d)
incomeq6d2 <- table(income2$q6d)
incomeq6d3 <- table(income3$q6d)
incomeq6d4 <- table(income4$q6d)
incomeq6d5 <- table(income5$q6d)

# Calc %s
# Income = 1
out <- NULL
for(i in 1:length(incomeq6d1)){
  results <- incomeq6d1[i]/sum(incomeq6d1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(incomeq6d1)
out <- rbind(out, incomeq6d1)
out
# Income = 2
out2 <- NULL
for(i in 1:length(incomeq6d2)){
  results <- incomeq6d2[i]/sum(incomeq6d2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(incomeq6d2)
out2 <- rbind(out2, incomeq6d2)
out2
# Income = 3
out3 <- NULL
for(i in 1:length(incomeq6d3)){
  results <- incomeq6d3[i]/sum(incomeq6d3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(incomeq6d3)
out3 <- rbind(out3, incomeq6d3)
out3
# Income = 4
out4 <- NULL
for(i in 1:length(incomeq6d4)){
  results <- incomeq6d4[i]/sum(incomeq6d4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(incomeq6d4)
out4 <- rbind(out4, incomeq6d4)
out4
# Income = 5
out5 <- NULL
for(i in 1:length(incomeq6d5)){
  results <- incomeq6d5[i]/sum(incomeq6d5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(incomeq6d5)
out5 <- rbind(out5, incomeq6d5)
out5

q6dbyIncome <- rbind(out, out2, out3, out4, out5)
q6dbyIncome$Income <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q6dbyIncome

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q6d)), aes(x = aWhite, fill = q6d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Opportunities to attend cultural activities (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop much less likely to rate preventative health services as "excellent" or "good" 
# Calc numbers for each subset of race, Q6d
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq6d1 <- table(race1$q6d)
raceq6d2 <- table(race2$q6d)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq6d1)){
  results <- raceq6d1[i]/sum(raceq6d1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq6d1)
out <- rbind(out, raceq6d1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq6d2)){
  results <- raceq6d2[i]/sum(raceq6d2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq6d2)
out2 <- rbind(out2, raceq6d2)
out2

q6dbyRace <- rbind(out, out2)
q6dbyRace$Race <- c(0, 0, 1, 1)
q6dbyRace

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q6d)), aes(x = Health, fill = q6d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Opportunities to attend cultural activities (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Good health pop more likely to say "excellent" or "good"






## q6f --> Employment Opportunities (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6freg <- lm(q6f ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6freg)

# Age and health have impact on perceptions of employment opportunities in Kzoo

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q6f)), aes(x = Health, fill = q6f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Quality of employment opportunities (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Good health pop much more likely to say excellent or good

# Calc numbers for each subset of health, Q6f
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q6f1 <- table(health1$q6f)
q6f2 <- table(health2$q6f)
q6f3 <- table(health3$q6f)
q6f4 <- table(health4$q6f)
q6f5 <- table(health5$q6f)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q6f1)){
  results <- q6f1[i]/sum(q6f1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q6f1)
out <- rbind(out, q6f1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q6f2)){
  results <- q6f2[i]/sum(q6f2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q6f2)
out2 <- rbind(out2, q6f2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q6f3)){
  results <- q6f3[i]/sum(q6f3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q6f3)
out3 <- rbind(out3, q6f3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q6f4)){
  results <- q6f4[i]/sum(q6f4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q6f4)
out4 <- rbind(out4, q6f4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q6f5)){
  results <- q6f5[i]/sum(q6f5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q6f5)
out5 <- rbind(out5, q6f5)
out5


q6fbyhealth <- rbind(out, out2, out3, out4, out5)
q6fbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q6fbyhealth

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q6f)), aes(x = age, fill = q6f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Quality of employment opportunities (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# No major relationship






## q6g --> Shopping Opportunities (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6greg <- lm(q6g ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6greg)

# Race and income have impact views of shopping opportunities in Kzoo
# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q6g)), aes(x = aWhite, fill = q6g)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Quality of shopping opportunities (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop less likely to rate preventative health services as "excellent" or "good" 
# Calc numbers for each subset of race, Q6g
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq6g1 <- table(race1$q6g)
raceq6g2 <- table(race2$q6g)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq6g1)){
  results <- raceq6g1[i]/sum(raceq6g1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq6g1)
out <- rbind(out, raceq6g1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq6g2)){
  results <- raceq6g2[i]/sum(raceq6g2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq6g2)
out2 <- rbind(out2, raceq6g2)
out2

q6gbyRace <- rbind(out, out2)
q6gbyRace$Race <- c(0, 0, 1, 1)
q6gbyRace

# Breakdown of results by Income (1 = < $25k, 2 = $25 - $50, 3 = $50-99, 4 = $100-150, 5 = > $150K)
ggplot(subset(surveyf, !is.na(income) & !is.na(q6g)), aes(x = income, fill = q6g)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "INCOME (1 = <$25K, 5 = >$150K)", y = "Percent", title = "Quality of shopping opportunities (1 = excellent, 4 = poor, 5 = Don't Know)") 
# About the same except for high income, theres a bigger chunk of "poor" 





## q6h --> Cost of living in Kzoo (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6hreg <- lm(q6h ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6hreg)

# Health and income have impact perceptions of cost of living in Kzoo
# Breakdown of results by Income (1 = < $25k, 2 = $25 - $50, 3 = $50-99, 4 = $100-150, 5 = > $150K)
ggplot(subset(surveyf, !is.na(income) & !is.na(q6h)), aes(x = income, fill = q6h)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "INCOME (1 = <$25K, 5 = >$150K)", y = "Percent", title = "Cost of living in Kalamazoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# High income feels the cost of living is excellent or good more than others

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q6h)), aes(x = Health, fill = q6h)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Cost of living in Kalamazoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Good health pop much more likely to say excellent or good

# Calc numbers for each subset of health, Q6f
health1 <- subset(surveyf, Health == 1)
health2 <- subset(surveyf, Health == 2)
health3 <- subset(surveyf, Health == 3)
health4 <- subset(surveyf, Health == 4)
health5 <- subset(surveyf, Health == 5)
q6h1 <- table(health1$q6h)
q6h2 <- table(health2$q6h)
q6h3 <- table(health3$q6h)
q6h4 <- table(health4$q6h)
q6h5 <- table(health5$q6h)
# Calc %s
# Health = 1
out <- NULL
for(i in 1:length(q6h1)){
  results <- q6h1[i]/sum(q6h1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q6h1)
out <- rbind(out, q6h1)
out
# Health = 2
out2 <- NULL
for(i in 1:length(q6h2)){
  results <- q6h2[i]/sum(q6h2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q6h2)
out2 <- rbind(out2, q6h2)
out2
# Health = 3
out3 <- NULL
for(i in 1:length(q6h3)){
  results <- q6h3[i]/sum(q6h3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q6h3)
out3 <- rbind(out3, q6h3)
out3
# Health = 4
out4 <- NULL
for(i in 1:length(q6h4)){
  results <- q6h4[i]/sum(q6h4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q6h4)
out4 <- rbind(out4, q6h4)
out4
# Health = 5
out5 <- NULL
for(i in 1:length(q6h5)){
  results <- q6h5[i]/sum(q6h5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q6h5)
out5 <- rbind(out5, q6h5)
out5


q6hbyhealth <- rbind(out, out2, out3, out4, out5)
q6hbyhealth$Health <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q6hbyhealth






## q6j --> Vibrant / downtown commercial area (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6jreg <- lm(q6j ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6jreg)

# Health and race have impact on thoughts about downtown of Kzoo
# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q6j)), aes(x = aWhite, fill = q6j)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Quality of downtown Kalamazoo (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop less likely to rate downtown as "excellent" or "good" 
# Calc numbers for each subset of race, Q6j
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq6j1 <- table(race1$q6j)
raceq6j2 <- table(race2$q6j)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq6j1)){
  results <- raceq6j1[i]/sum(raceq6j1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq6j1)
out <- rbind(out, raceq6j1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq6j2)){
  results <- raceq6j2[i]/sum(raceq6j2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq6j2)
out2 <- rbind(out2, raceq6j2)
out2

q6jbyRace <- rbind(out, out2)
q6jbyRace$Race <- c(0, 0, 1, 1)
q6jbyRace

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q6j)), aes(x = Health, fill = q6j)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Quality of downtown Kalamazoo (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Good health pop much more likely to say excellent and good





## q6n --> Opportunities to participate in community matters (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6nreg <- lm(q6n ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6nreg)

# Age and health have impact on perceptions of opportunities to participate in community matters of Kzoo

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q6n)), aes(x = Health, fill = q6n)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Ability to participate in community matters (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Good health pop much more likely to say excellent and good

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q6n)), aes(x = age, fill = q6n)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "Ability to participate in community matters (1 = Excellent, 4 = Poor, 5 = N/A)") 
# Older pop is more likely to say excellent or good opportunities

# Calc numbers for each subset of age, Q6n
age1 <- subset(surveyf, age == 1)
age2 <- subset(surveyf, age == 2)
age3 <- subset(surveyf, age == 3)
q6n1 <- table(age1$q6n)
q6n2 <- table(age2$q6n)
q6n3 <- table(age3$q6n)
# Calc %s
# Age = 1
out <- NULL
for(i in 1:length(q6n1)){
  results <- q6n1[i]/sum(q6n1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q6n1)
out <- rbind(out, q6n1)
out
# Age = 2
out2 <- NULL
for(i in 1:length(q6n2)){
  results <- q6n2[i]/sum(q6n2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q6n2)
out2 <- rbind(out2, q6n2)
out2
# Age = 3
out3 <- NULL
for(i in 1:length(q6n3)){
  results <- q6n3[i]/sum(q6n3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q6n3)
out3 <- rbind(out3, q6n3)
out3

q6nbyage <- rbind(out, out2, out3)
q6nbyage$Age <- c(1, 1, 2, 2, 3, 3)
q6nbyage





## q6o --> Openness and acceptance of Kzoo towards diverse backgrounds (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q6oreg <- lm(q6o ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q6oreg)

# Race and health have impact on overall appearance of Kzoo
# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q6o)), aes(x = aWhite, fill = q6o)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "Openness of Kzoo towards diversity (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop less likely to rate downtown as "excellent" or "good" 
# Calc numbers for each subset of race, Q6o
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq6o1 <- table(race1$q6o)
raceq6o2 <- table(race2$q6o)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq6o1)){
  results <- raceq6o1[i]/sum(raceq6o1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq6o1)
out <- rbind(out, raceq6o1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq6o2)){
  results <- raceq6o2[i]/sum(raceq6o2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq6o2)
out2 <- rbind(out2, raceq6o2)
out2

q6obyRace <- rbind(out, out2)
q6obyRace$Race <- c(0, 0, 1, 1)
q6obyRace

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q6o)), aes(x = Health, fill = q6o)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "Openness of Kzoo towards diversity (1 = excellent, 4 = poor, 5 = Don't Know)") 
# Good health pop much more likely to say excellent and good






## q7h --> Contacted the city of Kzoo for help or information (1 = No, 2 = Yes)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q7hreg <- lm(q7h ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q7hreg)

# Tenure and race have impact on likelihood of contacting city of Kzoo for info
# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q7h)), aes(x = aWhite, fill = q7h)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "In the last year, have you contacted the City for help/info? (1 = Yes, 2 = No)") 
# Nonwhite pop less likely to contact city for help/info 

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q7h)), aes(x = tenure, fill = q7h)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "In the last year, have you contacted the City for help/info? (1 = Yes, 2 = No)") 
# Renter pop much more likely to contact city for help/info

# Calc numbers for each subset of tenure, Q7h
tenure1 <- subset(surveyf, tenure == 1)
tenure2 <- subset(surveyf, tenure == 2)
tenureq7h1 <- table(tenure1$q7h)
tenureq7h2 <- table(tenure2$q7h)
# Calc %s
# Tenure = 1
out <- NULL
for(i in 1:length(tenureq7h1)){
  results <- tenureq7h1[i]/sum(tenureq7h1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(tenureq7h1)
out <- rbind(out, tenureq7h1)
out
# Tenure = 2
out2 <- NULL
for(i in 1:length(tenureq7h2)){
  results <- tenureq7h2[i]/sum(tenureq7h2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(tenureq7h2)
out2 <- rbind(out2, tenureq7h2)
out2

q7hbyTenure <- rbind(out, out2)
q7hbyTenure$Tenure <- c(1, 1, 2, 2)
q7hbyTenure





## q7i --> Contacted the city of Kzoo to express your opinion (1 = No, 2 = Yes)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q7ireg <- lm(q7i ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q7ireg)

# Vote local impacts likelihood of contacting city of Kzoo to express opinion

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q7i)), aes(x = VoteLocal, fill = q7i)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "In the last year, have you contacted the City to express an opinion (1 = Yes, 2 = No)") 
# People who never vote local are most likely to contact city to express opinion

# Calc numbers for each subset of vote local, Q5k
votelocal1 <- subset(surveyf, VoteLocal == 1)
votelocal2 <- subset(surveyf, VoteLocal == 2)
votelocal3 <- subset(surveyf, VoteLocal == 3)
votelocal4 <- subset(surveyf, VoteLocal == 4)
votelocal5 <- subset(surveyf, VoteLocal == 5)
q7i1 <- table(votelocal1$q7i)
q7i2 <- table(votelocal2$q7i)
q7i3 <- table(votelocal3$q7i)
q7i4 <- table(votelocal4$q7i)
q7i5 <- table(votelocal5$q7i)
# Calc %s
# Vote local = 1
out <- NULL
for(i in 1:length(q7i1)){
  results <- q7i1[i]/sum(q7i1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q7i1)
out <- rbind(out, q7i1)
out
# Vote local = 2
out2 <- NULL
for(i in 1:length(q7i2)){
  results <- q7i2[i]/sum(q7i2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q7i2)
out2 <- rbind(out2, q7i2)
out2
# Vote local = 3
out3 <- NULL
for(i in 1:length(q7i3)){
  results <- q7i3[i]/sum(q7i3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q7i3)
out3 <- rbind(out3, q7i3)
out3
# Vote local = 4
out4 <- NULL
for(i in 1:length(q7i4)){
  results <- q7i4[i]/sum(q7i4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q7i4)
out4 <- rbind(out4, q7i4)
out4
# Vote local = 5
out5 <- NULL
for(i in 1:length(q7i5)){
  results <- q7i5[i]/sum(q7i5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q7i5)
out5 <- rbind(out5, q7i5)
out5


q7ibyvotelocal <- rbind(out, out2, out3, out4, out5)
q7ibyvotelocal$VoteLocal <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q7ibyvotelocal





## q8a --> Used Kzoo rec centers or their services (1 = 2 times / week or more, 2 = 2-4 times / month, 3 = Once / month or less, 4 = Not at all)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q8areg <- lm(q8a ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q8areg)

# Health has impact on use of kzoo rec centers
# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q8a)), aes(x = Health, fill = q8a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "How often do you use Kzoo rec centers (1 = 2 times / week, 4 = Not at all)") 
# Good health pop uses the services much more often

# Breakdown of results by Income (1 = < $25k, 2 = $25 - $50, 3 = $50-99, 4 = $100-150, 5 = > $150K)
ggplot(subset(surveyf, !is.na(income) & !is.na(q8a)), aes(x = income, fill = q8a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "INCOME (1 = <$25K, 5 = >$150K)", y = "Percent", title = "How often do you use city/neighborhood parks? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Higher income much more likely to use parks frequently




## q8b --> Visited a neighborhood or City park (1 = 2 times / week or more, 2 = 2-4 times / month, 3 = Once / month or less, 4 = Not at all)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q8breg <- lm(q8b ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q8breg)

# Income, health, and age have impact on use of kzoo parks

# Breakdown of results by Income (1 = < $25k, 2 = $25 - $50, 3 = $50-99, 4 = $100-150, 5 = > $150K)
ggplot(subset(surveyf, !is.na(income) & !is.na(q8b)), aes(x = income, fill = q8b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "INCOME (1 = <$25K, 5 = >$150K)", y = "Percent", title = "How often do you use city/neighborhood parks? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Higher income much more likely to use parks frequently
# Calc numbers for each subset of Income, Q8b
income1 <- subset(surveyf, income == 1)
income2 <- subset(surveyf, income == 2)
income3 <- subset(surveyf, income == 3)
income4 <- subset(surveyf, income == 4)
income5 <- subset(surveyf, income == 5)
incomeq8b1 <- table(income1$q8b)
incomeq8b2 <- table(income2$q8b)
incomeq8b3 <- table(income3$q8b)
incomeq8b4 <- table(income4$q8b)
incomeq8b5 <- table(income5$q8b)

# Calc %s
# Income = 1
out <- NULL
for(i in 1:length(incomeq8b1)){
  results <- incomeq8b1[i]/sum(incomeq8b1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(incomeq8b1)
out <- rbind(out, incomeq8b1)
out
# Income = 2
out2 <- NULL
for(i in 1:length(incomeq8b2)){
  results <- incomeq8b2[i]/sum(incomeq8b2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(incomeq8b2)
out2 <- rbind(out2, incomeq8b2)
out2
# Income = 3
out3 <- NULL
for(i in 1:length(incomeq8b3)){
  results <- incomeq8b3[i]/sum(incomeq8b3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(incomeq8b3)
out3 <- rbind(out3, incomeq8b3)
out3
# Income = 4
out4 <- NULL
for(i in 1:length(incomeq8b4)){
  results <- incomeq8b4[i]/sum(incomeq8b4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(incomeq8b4)
out4 <- rbind(out4, incomeq8b4)
out4
# Income = 5
out5 <- NULL
for(i in 1:length(incomeq8b5)){
  results <- incomeq8b5[i]/sum(incomeq8b5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(incomeq8b5)
out5 <- rbind(out5, incomeq8b5)
out5

q8bbyIncome <- rbind(out, out2, out3, out4, out5)
q8bbyIncome$Income <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q8bbyIncome

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q8b)), aes(x = Health, fill = q8b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "How often do you use city/neighborhood parks? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Poor health much more likely to be infrequent users

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q8b)), aes(x = age, fill = q8b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "How often do you use city/neighborhood parks? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Middle aged pop is more likely use parks more





## q8c --> Used Kzoo public libraries or services (1 = 2 times / week or more, 2 = 2-4 times / month, 3 = Once / month or less, 4 = Not at all)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q8creg <- lm(q8c ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q8creg)

# Race, health, vote local have impact on use of libraries

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q8c)), aes(x = aWhite, fill = q8c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "How often do you use the library or its services? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Nonwhite pop uses library and services more often
# Calc numbers for each subset of race, Q8c
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq8c1 <- table(race1$q8c)
raceq8c2 <- table(race2$q8c)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq8c1)){
  results <- raceq8c1[i]/sum(raceq8c1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq8c1)
out <- rbind(out, raceq8c1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq8c2)){
  results <- raceq8c2[i]/sum(raceq8c2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq8c2)
out2 <- rbind(out2, raceq8c2)
out2

q8cbyRace <- rbind(out, out2)
q8cbyRace$Race <- c(0, 0, 1, 1)
q8cbyRace

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q8c)), aes(x = Health, fill = q8c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "How often do you use the library or its services? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Nothing major here, excellent health more likely users

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q8c)), aes(x = VoteLocal, fill = q8c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "How often do you use the library or its services? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# People who never vote local are least likely to use library





## q8e --> Attended a city sponsored event (1 = 2 times / week or more, 2 = 2-4 times / month, 3 = Once / month or less, 4 = Not at all)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q8ereg <- lm(q8e ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q8ereg)

# Race has impact on attending city sponsored events in Kzoo
# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q8e)), aes(x = aWhite, fill = q8e)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "How often do you attend city sponsored events? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Nonwhite pop attends city sponsored events more often
# Calc numbers for each subset of race, Q8e
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq8e1 <- table(race1$q8e)
raceq8e2 <- table(race2$q8e)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq8e1)){
  results <- raceq8e1[i]/sum(raceq8e1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq8e1)
out <- rbind(out, raceq8e1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq8e2)){
  results <- raceq8e2[i]/sum(raceq8e2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq8e2)
out2 <- rbind(out2, raceq8e2)
out2

q8ebyRace <- rbind(out, out2)
q8ebyRace$Race <- c(0, 0, 1, 1)
q8ebyRace

# Breakdown of results by health (1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor)
ggplot(subset(surveyf, !is.na(Health) & !is.na(q8e)), aes(x = Health, fill = q8e)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "HEALTH (1 = excellent, 5 = poor)", y = "Percent", title = "How often do you use the library or its services? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Nothing major here, excellent health more likely users




## q8f --> Used bus / pub transp instead of driving (1 = 2 times / week or more, 2 = 2-4 times / month, 3 = Once / month or less, 4 = Not at all)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q8freg <- lm(q8f ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q8freg)

# Sex, race, tenure have impact on using public transport
# Breakdown of results by sex (0 = female, 1 = male)
ggplot(subset(surveyf, !is.na(sex) & !is.na(q8f)), aes(x = sex, fill = q8f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "GENDER (0 = Female, 1 = Male)", y = "Percent", title = "How often do you use public transportation instead of driving? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Female more likely to use pub transp.

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q8f)), aes(x = aWhite, fill = q8f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "How often do you use public transportation instead of driving? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Nonwhite pop uses public transp much more often

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q8f)), aes(x = tenure, fill = q8f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "How often do you use public transportation instead of driving? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# Renter pop much more likely to use public transp.







## q9a --> Attended a local public meeting (1 = 2 times / week or more, 2 = 2-4 times / month, 3 = Once / month or less, 4 = Not at all)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q9areg <- lm(q9a ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q9areg)

# Vote local has impact on attending local public meetings

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q9a)), aes(x = VoteLocal, fill = q9a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "How often do you attend local public meetings? \n(1 = 2 times / week, 2 = 2-4 times / month, 3 = once a month or less, 4 = Not at all)") 
# The more likely you are to vote, the more likely you are to attend public meetings

# Calc numbers for each subset of vote local, Q9a
votelocal1 <- subset(surveyf, VoteLocal == 1)
votelocal2 <- subset(surveyf, VoteLocal == 2)
votelocal3 <- subset(surveyf, VoteLocal == 3)
votelocal4 <- subset(surveyf, VoteLocal == 4)
votelocal5 <- subset(surveyf, VoteLocal == 5)
q9a1 <- table(votelocal1$q9a)
q9a2 <- table(votelocal2$q9a)
q9a3 <- table(votelocal3$q9a)
q9a4 <- table(votelocal4$q9a)
q9a5 <- table(votelocal5$q9a)
# Calc %s
# Vote local = 1
out <- NULL
for(i in 1:length(q9a1)){
  results <- q9a1[i]/sum(q9a1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(q9a1)
out <- rbind(out, q9a1)
out
# Vote local = 2
out2 <- NULL
for(i in 1:length(q9a2)){
  results <- q9a2[i]/sum(q9a2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(q9a2)
out2 <- rbind(out2, q9a2)
out2
# Vote local = 3
out3 <- NULL
for(i in 1:length(q9a3)){
  results <- q9a3[i]/sum(q9a3)
  out3 <- as.data.frame(cbind(out3, results))
}
colnames(out3) <- 1:length(q9a3)
out3 <- rbind(out3, q9a3)
out3
# Vote local = 4
out4 <- NULL
for(i in 1:length(q9a4)){
  results <- q9a4[i]/sum(q9a4)
  out4 <- as.data.frame(cbind(out4, results))
}
colnames(out4) <- 1:length(q9a4)
out4 <- rbind(out4, q9a4)
out4
# Vote local = 5
out5 <- NULL
for(i in 1:length(q9a5)){
  results <- q9a5[i]/sum(q9a5)
  out5 <- as.data.frame(cbind(out5, results))
}
colnames(out5) <- 1:length(q9a5)
out5 <- rbind(out5, q9a5)
out5


q9abyvotelocal <- rbind(out, out2, out3, out4, out5)
q9abyvotelocal$VoteLocal <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
q9abyvotelocal





## q11a --> How would you rate quality of services by City of Kzoo   (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q11areg <- lm(q11a ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q11areg)

# Race has impact
# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q11a)), aes(x = aWhite, fill = q11a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "How would you rate overall quality of services from the City of Kalamazoo? \n (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop less likely to say excellent or good






## q12b --> How would you rate the overall direction Kzoo is taking (1 = Excellent, 2 = Good, 3 = Fair, 4 = Poor, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q12breg <- lm(q12b ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q12breg)

# Race, length of res, and health have impact on view direction kzoo is taking

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q12b)), aes(x = aWhite, fill = q12b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "How would you rate the overall direction the City of Kalamazoo is taking? \n (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Nonwhite pop isn't as optimistic about direction of kzoo
# Calc numbers for each subset of race, Q12b
race1 <- subset(surveyf, aWhite == 0)
race2 <- subset(surveyf, aWhite == 1)
raceq12b1 <- table(race1$q12b)
raceq12b2 <- table(race2$q12b)
# Calc %s
# Race = 0
out <- NULL
for(i in 1:length(raceq12b1)){
  results <- raceq12b1[i]/sum(raceq12b1)
  out <- as.data.frame(cbind(out, results))
}
colnames(out) <- 1:length(raceq12b1)
out <- rbind(out, raceq12b1)
out
# Race = 1
out2 <- NULL
for(i in 1:length(raceq12b2)){
  results <- raceq12b2[i]/sum(raceq12b2)
  out2 <- as.data.frame(cbind(out2, results))
}
colnames(out2) <- 1:length(raceq12b2)
out2 <- rbind(out2, raceq12b2)
out2

q12bbyRace <- rbind(out, out2)
q12bbyRace$Race <- c(0, 0, 1, 1)
q12bbyRace

# Breakdown of results by Length of Res (1 = < 2, 2 = 2-5, 3 = 6-10, 4 = 11-20, 5 = > 20)
ggplot(subset(surveyf, !is.na(LengthofRes) & !is.na(q12b)), aes(x = LengthofRes, fill = q12b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "Length of Res (1 = < 2 years, 5 = > 20 years)", y = "Percent", title = "How would you rate the overall direction the City of Kalamazoo is taking? \n (1 = Excellent, 4 = Poor, 5 = Don't Know)") 
# Not meaningful





## Q15a --> how likely to use surveys

# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q15areg <- lm(q15a ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
names(surveyf)
summary(q15areg)

# Vote local has impact on likelihood to use surveys
# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q15a)), aes(x = VoteLocal, fill = q15a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "How likely are you to use surveys? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# The more likely you are to vote, the more likely you are to use surveys for the most part




## q15b --> How likely to use online discussions (1 = Very likely, 2 = somewhat likely, 3 = somewhat unlikely, 4 = very unlikely, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q15breg <- lm(q15b ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q15breg)

# Age and vote local have impact on likelihood to use online disc
# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q15b)), aes(x = VoteLocal, fill = q15b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "How likely are you to use online discussions? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# The more likely you are to vote, the more likely you are to use online disc for the most part

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q15b)), aes(x = age, fill = q15b)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "How likely are you to use online discussions? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Honestly, not a noticeable difference




## q15c --> How likely to use face-to-face discussions (1 = Very likely, 2 = somewhat likely, 3 = somewhat unlikely, 4 = very unlikely, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q15creg <- lm(q15c ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q15creg)

# Sex, race, and vote local have impact on likelihood to use face-to-face disc

# Breakdown of results by sex (0 = female, 1 = male)
ggplot(subset(surveyf, !is.na(sex) & !is.na(q15c)), aes(x = sex, fill = q15c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "GENDER (0 = Female, 1 = Male)", y = "Percent", title = "How likely are you to use face-to-face discussions? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Male more likely to use f2f disc

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q15c)), aes(x = aWhite, fill = q15c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "How likely are you to use face-to-face discussions? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Nonwhite pop much more likely to use f2f

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q15c)), aes(x = VoteLocal, fill = q15c)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "How likely are you to use face-to-face discussions? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Except for the never voters, more likely to vote = more likely to use f2f





## q15d --> How likely to attend design workshops (1 = Very likely, 2 = somewhat likely, 3 = somewhat unlikely, 4 = very unlikely, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q15dreg <- lm(q15d ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q15dreg)

# Age, Race, tenure, and vote local have impact on likelihood to use design workshops

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q15d)), aes(x = age, fill = q15d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "How likely are you to attend design workshops? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Honestly, not a major difference

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q15d)), aes(x = aWhite, fill = q15d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "How likely are you to attend design workshops? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Nonwhite pop much more likely to attend design workshops

# Breakdown of results by Tenure (1 = Rent, 2 = Own)
ggplot(subset(surveyf, !is.na(tenure) & !is.na(q15d)), aes(x = tenure, fill = q15d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "TENURE (1 = rent, 2 = own)", y = "Percent", title = "How likely are you to attend design workshops? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Renter pop more likely to attend design workshops

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q15d)), aes(x = VoteLocal, fill = q15d)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "How likely are you to attend design workshops? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Nope





## q15e --> How likely to attend neighborhood meetings (1 = Very likely, 2 = somewhat likely, 3 = somewhat unlikely, 4 = very unlikely, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q15ereg <- lm(q15e ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q15ereg)

# Age and vote local have impact on likelihood to attend neighb meetings

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q15e)), aes(x = age, fill = q15e)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "How likely are you to attend neighborhood meetings? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Younger pop slightly more likely than older

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q15e)), aes(x = VoteLocal, fill = q15e)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "How likely are you to attend neighborhood meetings? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Voters are more likely to attend than non voters





## q15f --> How likely to attend community meetings (1 = Very likely, 2 = somewhat likely, 3 = somewhat unlikely, 4 = very unlikely, 5 = Don't Know)
# Regression of Age, VoteLocal, Health, Length of Res, Income, Tenure, aWhite, Sex
q15freg <- lm(q15f ~ age + sex + aWhite + tenure + income + LengthofRes + Health + VoteLocal, data = survey)
summary(q15freg)

# Age, Race, and local voters have impact on likelihood to attend community meetings

# Breakdown of results by age (1 = 18-34, 2 = 35-54, 3 = 55+)
ggplot(subset(surveyf, !is.na(age) & !is.na(q15f)), aes(x = age, fill = q15f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "AGE (1 = 18-34, 2 = 35-55, 3 = 55+)", y = "Percent", title = "How likely are you to attend community meetings? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Younger pop much more likely than older

# Breakdown of results by race (0 = nonwhite, 1 = white)
ggplot(subset(surveyf, !is.na(aWhite) & !is.na(q15f)), aes(x = aWhite, fill = q15f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "RACE (0 = Nonwhite, 1 = White)", y = "Percent", title = "How likely are you to attend community meetings? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Nonwhite pop slightly more likely to attend

# Breakdown of results by local voters (1 = never, 2 = rarely, 3 = sometimes, 4 = usually, 5 = always)
ggplot(subset(surveyf, !is.na(VoteLocal) & !is.na(q15f)), aes(x = VoteLocal, fill = q15f)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  labs(x = "VOTE LOCAL (1 = never, 5 = always)", y = "Percent", title = "How likely are you to attend community meetings? \n (1 = very likely, 4 = very unlikely, 5 = Don't Know)") 
# Voters are more likely to attend than non voters










############

ggplot(subset(surveyf, !is.na(age) & !is.na(q15a)), aes(x = age, fill = q15a)) + 
  geom_bar(position = "fill", colour = "black") + coord_flip() + scale_fill_brewer(palette = "Blues") + 
  scale_color_fivethirtyeight("cyl") + theme_fivethirtyeight()



library(psych)
1:nlevels(surveyf$q15a)
# iterate over nlevels for each level of age15a, subset and agg by








test2 <- transform(surveyf, Tot.Count = ave())

ggplot(survey, aes(q15a)) + geom_bar(position = "fill", stat = "identity")

comm <- aggregate(data.frame(count = survey$q15a), list(value = survey$q15a), length)
comm
age <- aggregate(data.frame(count = survey$age), list(value = survey$age), length)
age



ggplot(parcelcomp1Q, aes(openM, fill = Status.x)) + geom_density() + facet_wrap(~ Inspector.x, scales = "free_y")







out = NULL
for(i in 1:ncol(survey)){
  results <- table(survey[,i])
  out <- rbind(out, results)
}

View(out)

out = NULL










# Read in file
survey <- read.csv("Kzoo Final Comm Survey.csv", head = T, sep = ",")
View(survey)
complete <- na.omit(survey)
View(complete)


# Replace NAs with column mean 

for(i in 1:ncol(survey)){
  survey[is.na(survey[,i]), i] <- round(mean(survey[,i], na.rm = TRUE))
}

View(survey)

options(scipen = 9999)
# Regression
regression <- lm(Custodianship_2014 ~ homeownership + 
                   medincome + propwhite, data=tracts)

summary(regression)

# Overall confidence in the Kzoo govt
reg <- lm(q12d ~ ., data = survey)
summary(reg)
# most important are 12c, Job kzoo does at welcoming citizen involvement (.17 coef),
# 12e, generally acting in best interest of community (.32 coef), 5j overall apperance of kzoo (-.25 coef)
# 6f employment opps (-.16) and 6g shopping opps (.22), fire services 10b (.13), utility billing 10u (-.16)
# d1b physical activity (.13), d10 (.32) children in household, 

reg2 <- lm(q12d ~ q12c + q12e + q5j + q6f + q6g + q10b + q10u + d1b + d10, data = survey)
summary(reg2)


# Kzoo as a place to live
reg <- lm(q1a ~ ., data = survey)
summary(reg)


# Age
reg <- lm(d15 ~ ., data = survey)
summary(reg)


# FACTOR ANALYSIS

library(psych)
library(GPArotation)
fact <- fa(survey, nfactors = 2)
fact1 <- fact$loadings[,1]

fact1[order(fact1)]


# -One end is age, age, age old, years in kzoo, kzoo good plac to retire, used bus, rail, subway or pub transp
# +Other end is overall confidence in kzoo government, appearance of kzoo, ec health of kzoo, 
# health and wellness of kzoo, being honest, 


fact2 <- fact$loadings[,2]
fact2[order(fact2)]
# -Campaigned or advocated for an issue, income, income, children under 17, recycle, participate in exercise
# +Other end is Neighb gatherings, community meetings, vibrant, surveys, diversity, collaborative, design workshops







# PRINCIPAL COMPONENT ANALYSIS
#estimating principal comp, both built into R using diff approaches

#first uses singular value decomposition (another unsupervised
#approach similar to factor analysis) and second uses cov/eigenvector
#approach.

#prcomp method using SVD
pcaA <- prcomp(survey)
pcaA1 <- pcaA$rotation[,1]

#princomp method using eigen of cov
pcaB <- princomp(survey)
pcaB1 <- pcaB$loadings[,1]

#do it ourselves using cov() to estimate the cov matrix
#and eigen() to get eigenvectors

#Direct eigen of cov
covm <- cov(survey)
eigenm <- eigen(covm)
eigen1 <- eigenm$vectors[,1]

eigenm
eigen1
#how similar are these methods?
f1mat <- cbind(fact1, pcaA1, pcaB1, eigen1)
cor(f1mat)

#fa() slightly diff due to rotation, but not much

#ESTIMATION BONUS

#eigenvecrs are both the underlying truth of the data and
#the equilibrium of any process the data describes, one additional
#way to find first eigenvector/princ comp is to take any 
#random vector v_1 of length p and multiply it by the
#data matrix; then take the resulting vector v_b (now of 
#length n) and multiple it back through the matrix to get another
#vector v'_a, and so on. If you do this a few times, ultimatley
#the process converges so final va is almost identical to
#first eigenvector (basically how SVD works). Its crude
#but effective, and works quickly and well on even large datasets

tm.survey <- t(as.matrix(survey))
m.survey <- as.matrix(survey)

va <- rnorm(ncol(survey))
va

for(i in 1:10){
  vb <- m.survey %*% scale(va)
  va <- tm.survey %*% scale(vb)
}

#not we rescale va and vb each time, otherwise they
#increase because eigenvalue stretches eigenvector eachtime
#its multiplied through the matrix. We only care about direction
#of v (ie, relative sizes of loadings), not absolute size
#so rescale.

#so, does final va match first comp?

cor(va, pcaA1)
#yes

#HOW MANY FACTORS TO KEEP?
#FA can produce as many factors as there were original vars (p)

#we wnt to reduce high-p data to smaller set of factors

#Hard to know, but one way is bigger eigenvalues determine
#relative importance of each of the factors: bigger the 
#eigenvalue the more variation in original data that eigenvector/
#factor explains

#Scree plot

#Plot values, ordered by size, in a scree plot (it looks like
#scree on side of mountain). All estimation methods include
#the values and factors, and ehre is a simple scree plot from
#our manual eigenvector calc

plot(eigenm$values, type = "b")

#look for elbow, steep decline to flat area. Might be
#5 or so factors we want to look at, rest are noise.
#another is to keep those w/ values above 1 (because that
#distinguishes eigenvectors that grow v. those that shrink)
#which is this case is 6 factors

#CUMULATIVE VAR
#plot prop of total variance in data explained by factors as you
#add them. Since eigenvalues are equiv to amount of var
#each comp explains in original data, we can just do same
#plot as before, but use the cumsum function to plot cuml amount
#(cumsum just turns a vector v or i values in a vector c where
#each value is ci is the sum of all vi up to the ith), and
#normalize by dividing by total

plot(cumsum(eigenm$values) / sum(eigenm$values), ylim=c(0, 1))

#elbow around 4/5 value, about same as before. Over 50% of
#variance is explained by first 4, so only 4 out of 71 factors
#are already explaining over half var. This plus previosu tests
#suggests 4-6 factors explain quite a lot of what's going on
#in var of mood among people voer time. 


#LESSON 3: CLUSTERING

#targeted at obs level rather than variable level: we want
#to know what natural groups the obs fall into, and what
#group each obs belong to (more complex, obs can belong
#to greater or lesser degrees to multiple clusters)

#challenge is to find natural clusters. When we seek to
#characterize clusters (ie, interpret), we are doing
#something more like var projection: we want to know
#where the clusters are located in the same var space as the
#obs

#ID the groups and the centerpoints of each group, they
#act like chicken and egg and vice versa

#K MEANS
#simple, only key decision is # of clusters?
#K means algorithm is just 2 steps repeated:
#1. assign a group category at random to every data point
#2.a. for each cat, calc the mean of all points (ie, mean x1,
#mean x2, etc) ie the centroid of those points
#2.b. after calc group centroids, reassign each point to the
#group of the nearest centroid. Go back to 2a and repeat until
#group assignments stop changing (ie, process has converged)

#Usually don't know # of groups, for this eg we'll use 2

#K MEANS ALGORITHM
set.seed(1)
clusterAx <- rnorm(100, 3, .75) 
clusterAy <- rnorm(100, 0, .75)
clusterA <- cbind(clusterAx, clusterAy, "A")
clusterA
clusterB <- cbind(clusterAy, clusterAx, "B")
clusterB

simdat1 <- rbind(clusterA, clusterB)
simdat1

plot(simdat1)

#assign our two clusters using K means, based on random initialization
set.seed(1)
cat <- as.numeric(round(runif(200, min = 0, max = 1)))
simdat2 <- cbind(simdat1, cat)

head(simdat2)
simdat2 <- data.frame(simdat2)
simdat2

#now iterate 2a 2b
#2.a: get centroids of two groups
#2.b.: calc distances of each point tocentroid 1 d1 and centroid 2 d2
#then reassign cat var depending on which centroid is closer

for(i in 1:2){
  centroids <- aggregate(simdat2[,1:2], by = list(cat=simdat2$cat), FUN=mean)
  print(centroids)
  d1 <- sqrt( (simdat2[,1] - centroids[1,2])^2 + (simdat2[,2] - centroids[1,3])^2)
  d2 <- sqrt( (simdat2[,1] - centroids[2,2])^2 + (simdat2[,2] - centroids[2,3])^2)
  simdat2$cat <- as.numeric(d1 < d2)
}

#should be as.factor(as.numeric) at the end and as.factor for cat above


#takes 2 steps for centroids to converge to approx (3,0) and (0,3)

#will take longer with higher dimensional data, more groups, and
#less well-separated obs

#drawbacks - - you have to guess the k ahead of time, and
#depending on random initialization, you can get diff groupings
#if data aren't well separated

#KMEANS IN R
kout <- kmeans(simdat2[,1:2], centers = 2)
as.vector(kout$cluster)

kout$centers

#more complex data, exact cat assignments and centroids can
#vary from run to run. We want placement of centroids such that
#total var within each group is minimized. That is, in 2b
#we add up distances between every member of a group and
#the centroid, for each group; we want  location of centroids
#such that the total sum (ie sum(d1) + sum(d2) in our 
#example) is minimized. We want  lowest level of within-cluster
#variation -- each cluster should be a tight group

#result of K means alg can depend on starting conditions,
#the kmeans function can automatically take a bunch of
#iniital cond, run the whole thing with each, and report
#the best of the runs. 

#EG, run w one random start, then 25, assigning 5 cat to
#our data

set.seed(100)
kout <- kmeans(simdat2[,1:2], centers = 5, nstart = 1)
kout$tot.withinss

kout <- kmeans(simdat2[,1:2], centers = 5, nstart = 25)
kout$tot.withinss

#here, second is better than first, due to running 25 times

#KMEANS WITH PSYCH EG
kout <- kmeans(msq2, centers = 2, nstart = 25)

#lets sort each centroid by its scores on all 71 vars, and
#display the top scoring vars
centroids <- kout$centers
topvars_centroid1 <- centroids[1, order(centroids[1,])]
topvars_centroid2 <- centroids[2, order(centroids[2,])]
tail(topvars_centroid1)
tail(topvars_centroid2)

#first cluster seems to be the sleep/tired, second is happy
#although it seems more about content and happy than 
#energetic and happy, as we saw in first princ comp.

#we often see strong overlap between clusters/factors,
#key diff is that factors are inherently dimensional and 
#oppositional: there are two directions for every factor,
#and we often see clear oppositions at either end,
#such as energetic v. tired (comp 1) and relaxed v 
#anxious (comp 2). Clusters are less oppositional: we can
#talk about vars that score highly, but it is less
#illuminating to look at vars that score weakly, since those
#are just things that aren't as near the cluster, which
#is more of a grab-bag when the cluster is some specific
#set of vars

#HIERARCHICAL CLUSTERING
#How many clusters to choose?
#We can add clusters one at a time (hartigan's rule) see
#what total var is (as we did for diff runs of same cluster
#size) and then accept the additional cluster if it lowers
#the total var enough. Similar to adj R2, which penalizes
#you for adding vars on the assumption that any random var
#can only lower R2

#Hierarchical clustering
#Unlike k menas, this assembles clusters piece by piece: first
#a few similar obs are joined into a multitude of small clusters
#then similar small clusters are attached together to form
#larger, on and upward until just 1 cluster. Then user can
#decide where in the process to draw the line and take 
#clusters at that level of aggregation

#Key idea here is similarity -- we join similar obs, and then
#similar clusters, until we've joined everything.

#similarity between obs is simple; just take distance between
#two obs (ie, Euclidean distance) that's a pretty good measure
#how do we measure two small clusters? or between a cluster
#and single obs we're considering adding

#Easiest to define in terms of dissimilarity, ie, join clusters
#with lowest dissimilarity

#Complete: the dissimilarity between cluster A and B is = to
#the largest distance between a member of group A and member
#og group B.
#Single: similar, but sets the dissimilarity between A and B
#to the smallest distance between member of A and member of B
#Average: average diss between every member of A and every
#member of B
#Centroid: Dis between centroid of A and centroid of B

#USING R
#hclust(), we'll use average method as reasonable middle ground
#among four distance methods. First, we need to calc the complete
#matrix of distances between every obs and every other
#luckily, there's another function to do this, dist()

hout <- hclust(dist(simdat1[,1:2]), method = "average")

#plot via dendrogram
plot(hout)
#height where any two clusters join is = to diss between those
#two clusters. There are a bunch of obs and small clusters
#moderately diff from each other, but two major
#clusters that are very diff (at about height 4)
#reflects 2 main clusters much more diff from each other
#than any subsets of those clusters are from each other

#choose height to slice the tree, or alt, the number of 
#clusters we want (which just causes alg to progressively
#lower the cut line until tree chopped there divides into 
#k branches)

#plot(hout)
plot(hout)
abline(a=3, b = 0, col = "red")

#to get cluster assignments, we apply cutree to hclust
#output, choose either height to cut at, or number of clusters
#number of clusters
as.vector(cutree(hout, 2))
#height
as.vector(cutree(hout, h = 3))

#so, how do we choose k? still no good answer, but rule of thumb
#is to make cut in wide stretch like that between height of 2
#and 4 in previous plot: big enough dist before next set of
#clusters are grouped. 

#PSYCH EXAMPLE
#hclust applied to MSQ
hout2 <- hclust(dist(survey), method = "complete")
plot(hout2, labels = F)

abline(a = 16.5, b = 0, col = "red")
abline(a = 21, b = 0, col = "blue")
#reasonable cuts, which divide tree into 2 and 5 clusters, respectively
#actual choice is up to researcher


