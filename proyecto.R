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
lm2.fit <- lm(Chance.of.Admit~TOEFL.Score+GRE.Score+CGPA+LOR^2+Research^3, data = train_data)
lm3.fit <- lm(Chance.of.Admit~TOEFL.Score^3+GRE.Score+CGPA+LOR+Research^3, data = train_data)
summary(lm2.fit)
summary(lm.fit)
summary(lm3.fit)

plot(lm3.fit)

pred_lm <- predict(lm3.fit,test_data)
submission <- test_data %>% select(id)
submission$`Chance of Admit`<- pred_lm

df <- data.frame(submission$id, submission$`Chance of Admit`)
colnames(df) <- c("id", "Chance of Admit")
head(df)
write.csv(df, "./Galileo/Econometria/Proyecto/sub3.csv",row.names=FALSE)
