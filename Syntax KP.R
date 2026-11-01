library(corrplot)
library(lmtest)
library(nortest)
library(car)
library(ggplot2)

data = read.csv("D:\\FMIPA - Statistika\\Semester 6\\KP\\data.csv", sep = ";")
data

summary(data[,-1])

corrplot(cor(data[,-1]), method = "number", type = "upper", diag = FALSE)

ggplot(data, aes(x = PDRB.ADHK, y = Persentase.Penduduk.Miskin)) +
  geom_point(alpha = 0.7, size = 2) + labs(x = "PDRB.ADHK", y = "Persentase.Penduduk.Miskin")

ggplot(data, aes(x = Laju.Pertumbuhan.Ekonomi, y = Persentase.Penduduk.Miskin)) +
  geom_point(alpha = 0.7, size = 2) + labs(x = "PDRB.ADHK", y = "Persentase.Penduduk.Miskin")

mult_reg = lm(Persentase.Penduduk.Miskin~PDRB.ADHK+Laju.Pertumbuhan.Ekonomi, data = data)
summary(mult_reg) # Uji F atau overal : < alpha = maka model layak digunakan & Uji t atau parsial : < alpha = koefisien regresi signifikan terhadap model

## Checking Assumptions ##
# Uji Autokorelasi
dwtest(mult_reg) # < alpha = terdapat autokorelasi pada residual, sehingga asumsi kebebasan residual tidak terpenuhi

# Uji Homoskedastisitas
bptest(mult_reg, studentize = TRUE, data = data) # < alpha = asumsi kehomogenan varians residual tidak terpenuhi

# Uji Normalitas
hist(mult_reg$residuals)
qqnorm(mult_reg$residuals, pch = 1, frame = FALSE)
qqline(mult_reg$residuals, col = "steelblue", lwd = 2)
lillie.test(mult_reg$residuals) # < alpha = residual tidak menyebar normal atau asumsi kenormalan residual tidak terpenuhi

# Uji Multikolinieritas
vif(mult_reg) # > VIF (10) = asumsi adanya multikolinearitas 
