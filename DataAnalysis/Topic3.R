# Используя средства предварительной обработки и дисперсионного анализа изучите данные выборки npk и установите значимость влияния факторов на вариацию зависимой переменной.

library(sm)

# !!! Однофакторный дисперсионный анализ (One-Way ANOVA)
# оценка влияния вариации одного или нескольких факторов на вариацию одной зависимой переменной Y

View(npk)

# Модель включает один фактор и одну зависимую количественную переменную
# yield – урожай (зависимая переменная, Y)
# N – применение азота на участке (номинальная переменная, фактор)

View(npk[c('N', 'yield')])

table(npk$N) # сколько наблюдений

lapply(list(yield ~ N, data = npk), summary) # описательная статистика

plot(yield ~ N, data = npk) # визуально средние

# Для применения дисперсионного анализа должны выполняться условия нормального распределения

# плотность yeild
hist(npk$yield, breaks = 6, freq = FALSE, col = "lightblue", xlab = "yield", ylab = "", main = "Плотность yield")
lines(density(npk$yield), col = "red", lwd = 2)

# плотность yeild по группам
attach(npk)
sm.density.compare(yield, N, lwd = 2, xlab = "yield")
title(main = "Плотность по N")
legend("topright", levels(N), fill = c(2:5))

# для подтверждения нормальной плотности тест Шапиро-Уилкса (p>0,05)
shapiro.test(npk$yield)
# для подтверждния однородной дисперсии (p>0,05)
bartlett.test(yield ~ N, data = npk)

# Дисперсионный анализ
# проверим гипотезу о равенстве средних по группам
# (различия между группами несущественны и вызваны влиянием случайных факторов)
# Ключевая идея в сравнении вариации между группами с вариацией внутри групп, вносящими вклад в общую вариацию переменной
# (p>0,05) - принятие

summary(aov(yield ~ N, data = npk))
# Отклоняем, значит азот влияет на результат

# Линейная модель
# Влияние - линия, смотрим отклонение
summary(lm(yield ~ N, data = npk))

# ВЫВОД: применение азота статистически значимо на вариацию урожайности

# P.S. При невыполнении условий применимости дисперсионного анализа используют непараметрические методы
oneway.test(yield ~ N, data = npk) # метод Уэлча
kruskal.test(yield ~ N, data = npk) # тест Краскала-Уоллиса

# !!! Двухфакторный дисперсионный анализ

# Визуализация средних по блокам
with(npk, interaction.plot(x.factor = block, trace.factor = N, response = yield, main = "N"))
with(npk, interaction.plot(x.factor = block, trace.factor = P, response = yield, main = "P"))
with(npk, interaction.plot(x.factor = block, trace.factor = K, response = yield, main = "K"))

# Тест Бартлетта на однородность дисперсий (p > 0,05)
bartlett.test(yield ~ N, data = npk)
bartlett.test(yield ~ P, data = npk)
bartlett.test(yield ~ K, data = npk)
# все дисперсии однородны

# Дисперсионный анализ (p < 0.05)
summary(aov(yield ~ block + N * P * K, data = npk))
# ВЫВОД: статистически значимы: N, K

# Линейная модель
# Если нарушается условие сбалансированности, следует применять линейную модель
summary(lmnpk <- lm(yield ~ block + N * P * K, data = npk))
# Значимость N=9.8500
anova(lmnpk)
# ВЫВОД: статистически значимы: N, K

# Тесты Краскала-Уоллиса (p < 0.05)
kruskal.test(yield ~ N, data = npk)
kruskal.test(yield ~ P, data = npk)
kruskal.test(yield ~ K, data = npk)
# ВЫВОД: значим N

# Общий вывод: N - значим, P и комбинации - не значимы