# ��������� ������ T2.R, ���������� �������� ��������������� ��������� � �������������-�������������� ������� ��� ������ stackloss.
# �������� ��������� ������ �������� ���������



View(stackloss)
names = c('Air.Flow', 'Water.Temp', 'Acid.Conc.', 'stack.loss')

# ��������������� ��������� (2.1)

View(summary.data.frame(stackloss))
View(cor(stackloss))
# ������������ 0.919 (Air.Flow & stack.loss)
# � ����������� 0.39 (Acid.Conc & Water.Temp) ������������ ����������

# ������ ������������� ������ - �������� ������ (2.2)

# stack.loss - ��������� ����������
# ������������ ���������� - Air.Flow (0.919)
# ����������� - Acid.Conc. (0.5)

linearRegression = function(columnName1, columnName2) {
    column1 = stackloss[[columnName1]]
    column2 = stackloss[[columnName2]]
    pair = column1 ~ column2

    dev.new()
    plot(pair, data = stackloss, ylab = columnName1, xlab = columnName2)
    abline(lm(pair, data = stackloss), col = "blue", lwd = 1)

    # ��������� ���������
    lm.rat1 <- lm(formula = pair, data = stackloss)
    print(paste("���������� ��������", columnName1, "�", columnName2))
    print(summary(lm.rat1))
    # p-value - ����������, ���� < 0.05, �� ���������� �������

    # ������������� ������� (CPI)
    CPI.df = cbind(predict(lm.rat1, interval = "conf"), predict(lm.rat1, interval = "pred"))
    CPI.df = CPI.df[, -4];

    dev.cur()
    matplot(column2, CPI.df, type = "l", lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4), ylab = columnName1, xlab = columnName2)
    with(stackloss, matpoints(column2, column1, pch = 20))
}

linearRegression('stack.loss', 'Air.Flow')
# ���������� �����

linearRegression('stack.loss', "Acid.Conc.")
# ���������� �� �����

# ������������� ������������� ������ - ������������� �������� ������

summary(fm1 <- lm(stack.loss ~ ., data = stackloss))
# stack.loss = -39.9 + 0.71*Air.Flow + 1.29*Water.Temp - 0.15*Acid.Conc.
qqplot(stackloss$stack.loss, predict.lm(fm1), main = "QQ-plot")
# ��������� - ��������-��������
# ��� ������������ ����������� � ������������� ��������

# ������������� ������������� ������ - ��������� ������������� ���������

summary(fm2 <- step(lm(stack.loss ~ ., data = stackloss)))
# stack.loss = -50.35 + 0.67*Air.Flow + 1.29*Water.Temp
# ������� Acid.Conc ��� � ��������� �������

qqplot(stackloss$stack.loss, predict.lm(fm2), main = "QQ-plot")