# Составьте скрипт T1.R, включающий средства предварительного анализа и визуализации для данных satackloss.

library(e1071)
library(pastecs)
library(sm)

View(stackloss)
names = c('Air.Flow', 'Water.Temp', 'Acid.Conc.', 'stack.loss')

# Получите описательную статистику (1.1.3)

ds <- data.frame(row.names = c(
    "Стандартное отклонение",
    "Стандартная ошибка среднего",
    "Дисперсия",
    "Асимметрия",
    "Эксцесс",
    "Среднее",
    "Медиана",
    "1-й квартиль",
    "3-й квартиль",
    "Минимум",
    "Максимум"
))

fillStats = function(columnName) {
    ds[[columnName]][1] <- sd(stackloss[[columnName]]) #стандартное отклонение
    ds[[columnName]][2] <- sd(stackloss[[columnName]]) / sqrt(length(stackloss[[columnName]])) #стандартная ошибка среднего
    ds[[columnName]][3] <- var(stackloss[[columnName]]) #дисперсия
    ds[[columnName]][4] <- kurtosis(stackloss[[columnName]]) #асимметрия
    ds[[columnName]][5] <- skewness(stackloss[[columnName]]) #эксцесс
    ds[[columnName]][6] <- mean(stackloss[[columnName]]) #среднее
    ds[[columnName]][7] <- median(stackloss[[columnName]]) #медиана
    a <- as.numeric(quantile(stackloss[[columnName]]))
    ds[[columnName]][8] <- a[2] #1-й квартиль
    ds[[columnName]][9] <- a[4] #3-й квартиль
    ds[[columnName]][10] <- min(stackloss[[columnName]]) #минимум
    ds[[columnName]][11] <- max(stackloss[[columnName]]) #максимум
    ds
}

for (name in names) {
    ds = fillStats(name)
}

View(ds)

# проверьте гипотезы нормальности (1.1.5)

for (name in names) {
    # Тест Шапиро-Уилкса
    print(paste('Гипотеза нормальности для ', name))
    print(shapiro.test(stackloss[[name]]))
    # p-value < 0.05, отклоняем гипотезу "выборка из совокупности с нормальным распределением"

    #Графический
    dev.new()
    qqnorm(stackloss[[name]], main = name)
    qqline(stackloss[[name]], col = 2)

    dev.cur()
    sm.density(stackloss[[name]], model = "Normal", xlab = name, ylab = "Плотность распределения")
}

# визуализируйте данные (1.2)

for (name in names) {
    dev.new() # new graphical window
    qqnorm(stackloss[[name]], main = name)
    qqline(stackloss[[name]], col = 2)

    sm.density(stackloss[[name]], model = "Normal", xlab = name, ylab = "Плотность распределения")

    # Графики
    dev.cur()
    hist(stackloss[[name]], breaks = 6, freq = FALSE, col = "lightblue", xlab = "Наблюдаемые значения", ylab = name, main = paste("Гистограмма by", name))
    lines(density(stackloss[[name]]), col = "red", lwd = 2) # аппроксимация кривой вероятностей
}