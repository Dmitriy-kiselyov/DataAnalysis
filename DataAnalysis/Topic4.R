# ��������� ������ ������� ���������(PCA) � ��������� ������(FA) ��� ������ � ��������������� ����������� mtcars

View(mtcars)
names = c("mpg", "disp", "hp", "drat", "wt", "qsec")

cars = mtcars[names] # ������ �������������� ����������
View(cars)

# ������ ������� ��������� (4.1)
# ������ ����� ������� ����������� ��������, � ��� ��������� �������� �� �����������

# ��������� ���������
dev.new()
x <- as.matrix(cars)
pairs(x, gap = 0,
      diag.panel = function(x) {
          par(new = TRUE)
          hist(x, col = "light pink", probability = TRUE)
          lines(density(x), col = "red", lwd = 1)
      })
# ����� ���������� ���������� ���

pc.cars <- princomp(cars, scores = TRUE)
summary(pc.cars)
# ���������� ���� ��������� ����������� ������ �����������

plot(pc.cars) # ��������
# ������� 1 � 2 ����������, �������� �������� �������-������� (> 1)

biplot(pc.cars)
head(pc.cars$scores)

# ������, �� ��� ������ ������ ����� ����������

for (com in c(1, 2)) {
    for (name in names) {
        c = cor(cars[[name]], pc.cars$scores[, com])

        print(paste("���������� ����� ����������� �", com, "�", name, "=", c))
    }
    print("----------------")
}

# �������� ������� ����������
prcomp(cars, retx = TRUE, tol = 0.5)

# ��������� ������ (4.2)

attach(cars)
carsf <- cbind(mpg, disp, hp, drat, wt, qsec)

pc.cars <- factanal(carsf, factors = 1, start = NULL, scores = "Bartlett", STATISTIC)
pc.cars
# head(pc.cars$scores, n = 32)
hist(pc.cars$scores)

# ��������� ������ ��� 2 ������� ��������
pc.cars <- factanal(carsf, factors = 2, start = NULL, scores = "Bartlett", rotation = "varimax", STATISTIC)
pc.cars
# p-value > 0.05, ������ ����������
# head(pc.cars$scores)
dev.new()
plot(pc.cars$scores)

# ��������� ������ ��� 2 ������� �������� � ��������� ���� promax
pc.cars <- factanal(carsf, factors = 2, start = NULL, scores = "Bartlett", rotation = "promax", STATISTIC)
pc.cars
# head(pc.cars$scores)
dev.new()
plot(pc.cars$scores)
# p-value > 0.05, ������ ����������