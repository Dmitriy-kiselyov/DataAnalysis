# ��������� �������� ��������������� ��������� � �������������� ������� ������� ������ ������� npk � ���������� ���������� ������� �������� �� �������� ��������� ����������.

library(sm)

# !!! ������������� ������������� ������ (One-Way ANOVA)
# ������ ������� �������� ������ ��� ���������� �������� �� �������� ����� ��������� ���������� Y

View(npk)

# ������ �������� ���� ������ � ���� ��������� �������������� ����������
# yield � ������ (��������� ����������, Y)
# N � ���������� ����� �� ������� (����������� ����������, ������)

View(npk[c('N', 'yield')])

table(npk$N) # ������� ����������

lapply(list(yield ~ N, data = npk), summary) # ������������ ����������

plot(yield ~ N, data = npk) # ��������� �������

# ��� ���������� �������������� ������� ������ ����������� ������� ����������� �������������

# ��������� yeild
hist(npk$yield, breaks = 6, freq = FALSE, col = "lightblue", xlab = "yield", ylab = "", main = "��������� yield")
lines(density(npk$yield), col = "red", lwd = 2)

# ��������� yeild �� �������
attach(npk)
sm.density.compare(yield, N, lwd = 2, xlab = "yield")
title(main = "��������� �� N")
legend("topright", levels(N), fill = c(2:5))

# ��� ������������� ���������� ��������� ���� ������-������ (p>0,05)
shapiro.test(npk$yield)
# ��� ������������ ���������� ��������� (p>0,05)
bartlett.test(yield ~ N, data = npk)

# ������������� ������
# �������� �������� � ��������� ������� �� �������
# (�������� ����� �������� ������������� � ������� �������� ��������� ��������)
# �������� ���� � ��������� �������� ����� �������� � ��������� ������ �����, ��������� ����� � ����� �������� ����������
# (p>0,05) - ��������

summary(aov(yield ~ N, data = npk))
# ���������, ������ ���� ������ �� ���������

# �������� ������
# ������� - �����, ������� ����������
summary(lm(yield ~ N, data = npk))

# �����: ���������� ����� ������������� ������� �� �������� �����������

# P.S. ��� ������������ ������� ������������ �������������� ������� ���������� ����������������� ������
oneway.test(yield ~ N, data = npk) # ����� �����
kruskal.test(yield ~ N, data = npk) # ���� ��������-�������

# !!! ������������� ������������� ������

# ������������ ������� �� ������
with(npk, interaction.plot(x.factor = block, trace.factor = N, response = yield, main = "N"))
with(npk, interaction.plot(x.factor = block, trace.factor = P, response = yield, main = "P"))
with(npk, interaction.plot(x.factor = block, trace.factor = K, response = yield, main = "K"))

# ���� ��������� �� ������������ ��������� (p > 0,05)
bartlett.test(yield ~ N, data = npk)
bartlett.test(yield ~ P, data = npk)
bartlett.test(yield ~ K, data = npk)
# ��� ��������� ���������

# ������������� ������ (p < 0.05)
summary(aov(yield ~ block + N * P * K, data = npk))
# �����: ������������� �������: N, K

# �������� ������
# ���� ���������� ������� ������������������, ������� ��������� �������� ������
summary(lmnpk <- lm(yield ~ block + N * P * K, data = npk))
# ���������� N=9.8500
anova(lmnpk)
# �����: ������������� �������: N, K

# ����� ��������-������� (p < 0.05)
kruskal.test(yield ~ N, data = npk)
kruskal.test(yield ~ P, data = npk)
kruskal.test(yield ~ K, data = npk)
# �����: ������ N

# ����� �����: N - ������, P � ���������� - �� �������