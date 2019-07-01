library(dplyr)
library(reshape2)
library(ggplot2)
library(ggcorrplot)

train_data <- read.csv("./Galileo/Econometria/Proyecto/train.csv", header = TRUE)
test_data <- read.csv("./Galileo/Econometria/Proyecto/test.csv", header = TRUE)
head(test_data)
head(train_data)
cormat <- round(cor(train_data),2)
head(cormat)

ggcorrplot(cormat, hc.order = TRUE, type = "lower",
           lab = TRUE)

lm.fit <- lm(Chance.of.Admit~TOEFL.Score+GRE.Score+CGPA, data = train_data)
summary(lm.fit)

names(test_data)
names(train_data)
submission <- test_data %>% select(id)
submission$`Chance of Admit`<- runif(nrow(test_data))
write_csv(submission, "sub1.csv")

pred_lm <- predict(lm.fit,test_data)
submission <- test_data %>% select(id)
submission$`Chance of Admit`<- pred_lm
write_csv(submission, "sub2.csv")
