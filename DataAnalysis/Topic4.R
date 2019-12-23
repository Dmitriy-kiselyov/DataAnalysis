# Выполните анализ главных компонент(PCA) и факторный анализ(FA) для данных о характеристиках автомобилей mtcars

View(mtcars)
names = c("mpg", "disp", "hp", "drat", "wt", "qsec")

cars = mtcars[names] # только количественные переменные
View(cars)

# Анализ главных компонент (4.1)
# строим новую матрицу собственных значений, а все зависимые значения не учитываются

# Матричная диаграмма
dev.new()
x <- as.matrix(cars)
pairs(x, gap = 0,
      diag.panel = function(x) {
          par(new = TRUE)
          hist(x, col = "light pink", probability = TRUE)
          lines(density(x), col = "red", lwd = 1)
      })
# четко выраженной корреляции нет

pc.cars <- princomp(cars, scores = TRUE)
summary(pc.cars)
# Наибольшая доля дисперсии объясняется первой компонентой

plot(pc.cars) # квадраты
# включаю 1 и 2 компоненты, Согласно критерию Кайзера-Харриса (> 1)

biplot(pc.cars)
head(pc.cars$scores)

# Найдем, на что больше похожи новые компоненты

for (com in c(1, 2)) {
    for (name in names) {
        c = cor(cars[[name]], pc.cars$scores[, com])

        print(paste("Корреляция между компонентой №", com, "и", name, "=", c))
    }
    print("----------------")
}

# выделяем главную компоненту
prcomp(cars, retx = TRUE, tol = 0.5)

# Факторный анализ (4.2)

attach(cars)
carsf <- cbind(mpg, disp, hp, drat, wt, qsec)

pc.cars <- factanal(carsf, factors = 1, start = NULL, scores = "Bartlett", STATISTIC)
pc.cars
# head(pc.cars$scores, n = 32)
hist(pc.cars$scores)

# факторный анализ для 2 главных факторов
pc.cars <- factanal(carsf, factors = 2, start = NULL, scores = "Bartlett", rotation = "varimax", STATISTIC)
pc.cars
# p-value > 0.05, плохие результаты
# head(pc.cars$scores)
dev.new()
plot(pc.cars$scores)

# факторный анализ для 2 главных факторов с вращением типа promax
pc.cars <- factanal(carsf, factors = 2, start = NULL, scores = "Bartlett", rotation = "promax", STATISTIC)
pc.cars
# head(pc.cars$scores)
dev.new()
plot(pc.cars$scores)
# p-value > 0.05, плохие результаты