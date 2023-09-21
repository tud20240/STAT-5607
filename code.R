library(tidyverse) # library for data learning , manipulation , and visualization

score <- read.csv("C:\\Users\\tud20\\MSBA\\STAT 5607\\Week 15\\StudentsPerformance.csv", header = TRUE)

## rename columns

score <- score %>% rename('race' = 'race.ethnicity', 'parental_education' = 'parental.level.of.education', 'preparation_course' = 'test.preparation.course', 'math_score' = 'math.score', 'reading_score' = 'reading.score', 'writing_score' = 'writing.score')

## factoring

score$gender <- as.factor(score$gender)
levels(score$gender)

score$race <- as.factor(score$race)
levels(score$race)

## combine 5 levels in 3 levels 

score <- score %>% 
  mutate(parental_education = case_when(
    startsWith(parental_education, "bach") ~ "bachelor_higher",
    startsWith(parental_education, "mast") ~ "bachelor_higher",
    startsWith(parental_education, "asso") ~ "associate_some",
    startsWith(parental_education, "some college") ~ "associate_some",
    startsWith(parental_education, "high") ~ "highschool_lower",
    startsWith(parental_education, "some high") ~ "highschool_lower",
  ))

score$parental_education <- as.factor(score$parental_education)
score$parental_education <- relevel(score$parental_education,3) ## using highschool or lower as the baseline
levels(score$parental_education)

score$lunch <- as.factor(score$lunch)
score$lunch <- relevel(score$lunch,2) ## using standard as the basline
levels(score$lunch)

score$preparation_course <- as.factor(score$preparation_course)
score$preparation_course <- relevel(score$preparation_course,2) ## using none as the baseline
levels(score$preparation_course)

## sort data based on parental_education
score <- score %>% arrange(desc(parental_education))

## summary statistics / graphing

bar <- ggplot(score, aes(x=parental_education)) + geom_bar() + ggtitle('Count in levels of Parental Education')
bar

table(score$parental_education)

histgram1 <- ggplot(score, aes(math_score)) + geom_histogram() + labs(x = "Math score", y = "Frequency") + ggtitle('Distribution of Math Score')
histgram1

#histgram2 <- ggplot(score, aes(reading_score)) + geom_histogram() + labs(x = "Reading score", y = "Frequency") + ggtitle('Distribution of Reading Score')
#histgram2

#Scatter <- ggplot(score, aes(math_score, reading_score))
#Scatter + geom_point() + geom_smooth(method = "lm")+ labs(x = "math score", y = "reading score") + facet_wrap(~parental_education, ncol = 3) + ggtitle('Math and Reading score in different levels of Parental Education')

library(reshape2)
score2 <- score[,c('parental_education','math_score')]
score2_Melt<-melt(score2, id = c("parental_education"), measured = c("math_score"))
names(score2_Melt)<-c("Group", "Outcome_Measure", "Frequency")

Boxplot <- ggplot(score2_Melt, aes(Group, Frequency, colour = Outcome_Measure))
Boxplot + geom_boxplot() + labs(x = "Parental education", y = "score", colour = "Outcome Measure") + ggtitle('Math score for different levels of Parental Education')

library(car)
leveneTest(score$math_score, score$parental_education, center = median)


# Set contrasts

high_school_lower_vs_other<-c(-2, 1, 1)
associate_some_college_vs_Bachelor_higher <-c(0, -1, 1)
contrasts(score$parental_education)<-cbind(high_school_lower_vs_other, associate_some_college_vs_Bachelor_higher)

model1 <-aov(math_score ~ parental_education, data = score)
summary(model1)
summary.lm(model1)
Anova(model1, type = "III")

model2 <-lm(math_score ~ parental_education + gender + race + lunch + preparation_course , data = score)
summary(model2)
plot(model2)

