library(caret)

#Загрузим данные (https://www.kaggle.com/spscientist/students-performance-in-exams)
dataset <- read.csv("StudentsPerformance.csv")
head(dataset)
dataset$parental.level.of.education <- NULL
dataset$lunch <- NULL
dataset$test.preparation.course <- NULL
dataset$race.ethnicity <- NULL

dataset <- dataset[, c(2,3,4,1)]
head(dataset)
#Изучим устройство данных
summary(dataset)
# измерения
dim(dataset)
# список типов каждого атрибута
sapply(dataset, class)
# просмотрим уровни классификатора
levels(dataset$cut)

# Отрежем часть данных для последующей валидации 
# поллучим 80% данных
validation_index <- createDataPartition(dataset$gender, p=0.80, list=FALSE)
# выберем  20% для валидации
validation <- dataset[-validation_index,]
# 80% у нас будут исходные данные
dataset <- dataset[validation_index,]

## Исследуем и визуализируем даные
percentage <- prop.table(table(dataset$gender)) * 100
cbind(freq=table(dataset$gender), percentage=percentage)

# Разобьём данные на переменные и отклик
x <- dataset[,1:3]
y <- dataset[,4]

# Выборки будем визуализировать диаграммой размаха, как обычно
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(dataset)[i])
}
par(mfrow=c(1,1))
plot(y)

# Посмотрим взаимодействие внутри данных
featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")

## Анализ
control <- trainControl(method="cv", number=10)
# Контролируемая метрика
metric <- "Accuracy"

set.seed(13)
fit.lda <- train(gender~., data=dataset, method="lda", metric=metric, trControl=control)
print(fit.lda)

predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$gender)
