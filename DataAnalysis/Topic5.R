# ѕроведите канонический анализ дл€ подмножеств переменных набора данных LifeCycleSavings
library(CCP)

View(LifeCycleSavings)
# sr личные сбережени€, pop15 % населени€ < 15, pop75 % населени€ > 75, dpi доход на душу населени€, dpii % роста dpi
cor(LifeCycleSavings)

x = LifeCycleSavings[c(2, 3, 4)] # зависимые
y = LifeCycleSavings[c(1, 5)] # независимые

cx = cor(x)
cx

cy = cor(y)
cy

cxy <- cancor(x, y)
cxy
rho = cxy$cor

N = dim(x)[1] # number of observations
p = dim(x)[2] # number of dependent variables
q = dim(y)[2] # number of independent variable
N;p;q

# ѕроверка значимости канонических коррел€ций
p.asym.out <- p.asym(rho, N, p, q, tstat = "Wilks")
p.asym(rho, N, p, q, tstat = "Hotelling")
p.asym(rho, N, p, q, tstat = "Pillai")
p.asym(rho, N, p, q, tstat = "Roy")
plt.asym(p.asym.out, rhostart = 1)

p.perm(x, y, nboot = 999, rhostart = 1)
p.perm(x, y, nboot = 999, rhostart = 2)
# p.perm(x, y, nboot = 999, rhostart = 3) # p.perm: Parameter rhostart too big
out <- p.perm(x, y, nboot = 999, rhostart = 1, type = "Hotelling")
plt.perm(out)