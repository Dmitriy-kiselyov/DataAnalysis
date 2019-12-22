# Составьте скрипт T2.R, включающий средства предварительной обработки и корреляционно-регрессионного анализа для данных stackloss.
# Получите различные модели линейной регрессии



View(stackloss)
names = c('Air.Flow', 'Water.Temp', 'Acid.Conc.', 'stack.loss')

# Предварительная обработка (2.1)

View(summary.data.frame(stackloss))
View(cor(stackloss))
# Максимальный 0.919 (Air.Flow & stack.loss)
# и минимальный 0.39 (Acid.Conc & Water.Temp) коэффициенты корреляции

# Парный регрессионный анализ - линейная модель (2.2)

# stack.loss - зависимая переменная
# Максимальная корреляция - Air.Flow (0.919)
# минимальная - Acid.Conc. (0.5)

linearRegression = function(columnName1, columnName2) {
    column1 = stackloss[[columnName1]]
    column2 = stackloss[[columnName2]]
    pair = column1 ~ column2

    dev.new()
    plot(pair, data = stackloss, ylab = columnName1, xlab = columnName2)
    abline(lm(pair, data = stackloss), col = "blue", lwd = 1)

    # уравнение регрессии
    lm.rat1 <- lm(formula = pair, data = stackloss)
    print(paste("Корреляция столбцов", columnName1, "и", columnName2))
    print(summary(lm.rat1))
    # p-value - значимость, если < 0.05, то корреляция значима

    # доверительные области (CPI)
    CPI.df = cbind(predict(lm.rat1, interval = "conf"), predict(lm.rat1, interval = "pred"))
    CPI.df = CPI.df[, -4];

    dev.cur()
    matplot(column2, CPI.df, type = "l", lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4), ylab = columnName1, xlab = columnName2)
    with(stackloss, matpoints(column2, column1, pch = 20))
}

linearRegression('stack.loss', 'Air.Flow')
# корреляция видна

linearRegression('stack.loss', "Acid.Conc.")
# корреляция не видна

# Множественный регрессионный анализ - Множественная линейная модель

summary(fm1 <- lm(stack.loss ~ ., data = stackloss))
# stack.loss = -39.9 + 0.71*Air.Flow + 1.29*Water.Temp - 0.15*Acid.Conc.
qqplot(stackloss$stack.loss, predict.lm(fm1), main = "QQ-plot")
# Диаграмма - квантиль-квантиль
# Это соответствие наблюдаемых и предсказанных значений

# Множественный регрессионный анализ - Пошаговая множественная регрессия

summary(fm2 <- step(lm(stack.loss ~ ., data = stackloss)))
# stack.loss = -50.35 + 0.67*Air.Flow + 1.29*Water.Temp
# Выкинул Acid.Conc как с маленьким вкладом

qqplot(stackloss$stack.loss, predict.lm(fm2), main = "QQ-plot")