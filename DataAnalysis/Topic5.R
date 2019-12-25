# Проведите канонический анализ для подмножеств переменных набора данных LifeCycleSavings
library(CCP)

View(LifeCycleSavings)
lcs = LifeCycleSavings
# sr личные сбережения, pop15 % населения < 15, pop75 % населения > 75, dpi доход на душу населения, dpii % роста dpi

summary(lcs)

# !!! Канонический анализ
# метод анализа связей двух множеств переменных

cor(lcs) # корреляции

x = lcs[c(1, 2, 3)] # sr, pop15, pop75
View(x)

y = lcs[c(4, 5)] # dpi, ddpi
View(y)

cx = cor(x)
cx

cy = cor(y)
cy

cxy <- cancor(x, y)
cxy
# 0,8053328 означает достаточно тесную связь переменных двух множеств, выраженную через связь канонических переменных

# Размерности
N = dim(x)[1] # number of observations
p = dim(x)[2] # number of dependent variables
q = dim(y)[2] # number of independent variable
N;p;q



# асимптотический тест статистической значимости канонических корреляций
p.asym(rho = cxy$cor, N, p, q, tstat = "Wilks") # 2 to 2: без первой переменной
p.asym(rho = cxy$cor, N, p, q, tstat = "Hotelling")
p.asym(rho = cxy$cor, N, p, q, tstat = "Pillai")
p.asym(rho = cxy$cor, N, p, q, tstat = "Roy")

p.asym.out = p.asym(rho = cxy$cor, N, p, q, tstat = "Wilks")
plt.asym(p.asym.out, rhostart = 1) # rhostart - индекс наибольшего канонического коэффициента корреляции

# перестановочного теста
# проверка гипотезы отсутствия корреляции между 2 наборами данных X и Y путем случайных перестановок значения переменной Y
p.perm(x, y, nboot = 999, rhostart = 1)
p.perm(x, y, nboot = 999, rhostart = 2)
# stat0 – исходное значение статистики
# mstat – пересчитанные статистические значения, по одному для каждой перестановки
# nboot – количество пересчитанных перестановочных отсчетов
# nexcess – количество перестановок, которые привели к более экстремальному статистическому значению, чем stat0
# p – p-value, полученное от nexcess

plt.perm(p.perm(x, y, nboot = 999, rhostart = 1, type = "Hotelling"))
# Вертикальная серая пунктирная линия представляет среднее значение распределения перестановок

# !!! Многомерное шкалирование
# позволяет на основе информации о сходстве(различии) объектов восстановить их координаты в пространстве восприятия

# Метрическое шкалирование
# отображение выборки, заданной матрицей расстояний в евкл. пространство так, чтобы расстояния как можно точнее приближали исходные расстояния

# eurodist - расстояние между городами Европы
loc <- cmdscale(eurodist)
View(loc)

# нарисуем
X = loc[, 1]
Y = -loc[, 2]
plot(X, Y, type = "n", xlab = "", ylab = "", asp = 1, axes = FALSE, main = "Типа карта Европы")
text(X, Y, rownames(loc), cex = 0.6)