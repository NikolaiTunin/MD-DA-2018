#Дисперсионный анализ. Пример

#Загрузим данные (требуется установить Рабочую папку с помощью setwd) или указать полный путь
setwd("C:/Users/Nikolai/Desktop/MD-DA-2018-master/lab5")
data = read.csv("diet.csv",row.names=1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#https://www.sheffield.ac.uk/polopoly_fs/1.547015!/file/Diet_data_description.docx
#https://www.sheffield.ac.uk/mash/data
colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])
data<-na.omit(data)
data$gender <- data$gender + 1
data$gender <- factor(c("female", "male")[data$gender])
#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
#Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#проверим сбалансированные ли данные
table(data$diet.type)

#График групповых средних
install.packages("gplots")
library(gplots) #библиотека устанавлевается с помощью install.packages
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)


#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#тест на межгрупповые различия
fit.1 <- aov(weight.loss ~ diet.type, data=data)
summary(fit.1)

#попарные различия между средними значениями для всех групп
TukeyHSD(fit.1)

#Tukey honest significant differences test)
install.packages('multcomp')
library(multcomp)
par(mar=c(5,4,6,2))
tuk.1 <- glht(fit.1, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk.1, level=.05),col="lightgrey")

#Задание
#Добавить проверку на выборы и избавиться от них
#Исследование данных
par(mfrow=c(1,5))
hist(data$age, main = "Возраст, лет")
hist(data$height, main = "Высота, см")
hist(data$initial.weight, main = "Масса до диеты, кг")
hist(data$final.weight, main = "Масса после диеты, кг")
hist(data$weight.loss, main = "Потеря массы, кг")
#Видим ассиметрию, избавимся от нее

#Визулизируем возможные зависимости
par(mfrow=c(1,4)) 
plot(data$height, data$age,'p',main = "Зависимость возраста от высоты")
plot(data$height, data$initial.weight,'p',main = "Зависимость массы до диеты от высоты")
plot(data$height, data$final.weight,'p',main = "Зависимость массы после диеты от высоты")
plot(data$height, data$weight.loss,'p',main = "Зависимость потери массы от высоты")

# Построим линейную модель, посмотрим её характеристики
linear.model.1 <- lm(height ~ initial.weight, data=data)
linear.model.1
summary(linear.model.1)
plot(linear.model.1)

linear.model.2 <- lm(height ~ weight.loss, data=data)
linear.model.2
summary(linear.model.2)
plot(linear.model.2)

# Избавимся от выборосов, построим ещё модели и проверим их
par(mfrow=c(1,1))
data.noout <- data[data$height > 150 & data$initial.weight < 100 & data$final.weight < 100 & data$weight.loss > -1,]
linear.model.3 <- lm (height ~ initial.weight, data=data.noout)
linear.model.3
summary(linear.model.3)
plot(data.noout$height, data.noout$initial.weight,'p',main = "Зависимость массы до диеты от высоты")

linear.model.4 <- lm (height ~ weight.loss, data=data.noout)
linear.model.4
summary(linear.model.4)
plot(data.noout$height, data.noout$weight.loss,'p',main = "Зависимость потери массы от высоты")

#повторно проверсти все тесты и сравнить результаты с выбросами и без
boxplot(weight.loss~diet.type,data=data.noout,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

table(data.noout$diet.type)

plotmeans(weight.loss ~ diet.type, data=data.noout)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

fit.2 <- aov(weight.loss ~ diet.type, data=data.noout)
summary(fit.2)

TukeyHSD(fit.2)

par(mar=c(5,4,6,2), mfrow=c(1,2))
tuk.2 <- glht(fit.2, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk.1, level=.05),col="lightgrey")
plot(cld(tuk.2, level=.05),col="lightgrey")

#Открыть документ https://www.sheffield.ac.uk/polopoly_fs/1.547015!/file/Diet_data_description.docx
#и попытаться выполнить задания из него
#Различия в потере массы у мужчин и женщин
boxplot(weight.loss~gender,data=data.noout,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Gender")
abline(h=0,col="green")

#Различия в потере массы от разных диет для мужчин и женщин
par(mfrow=c(1,1))
boxplot(weight.loss~diet.type + gender,data=data.noout,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#Различия в потере массы от разных диет для мужчин и женщин, средние значения
par(mfrow=c(1,2))
data.noout.female <- data.noout[data.noout$gender == "female",]
data.noout.male <- data.noout[data.noout$gender == "male",]
plotmeans(weight.loss ~ diet.type, data=data.noout.female)
plotmeans(weight.loss ~ diet.type, data=data.noout.male)
