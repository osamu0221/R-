# 氏名: 冨江洸希
# 学籍番号: ACA22209

library(data.table)

fraud_dt <- fread("n_of_fraud.csv")
hr_dt <- as.data.table(readRDS(url("https://github.com/yukiyanai/quant-methods-R/raw/refs/heads/master/data/hr96-17.Rds")))

####################
# 第1問
####################
plot(fraud_dt$year, fraud_dt$n, type = "l",
     main = "Trend of Fraud Cases in Japan (1980-2022)",
     xlab = "Year", ylab = "Number of Cases", col = "darkgreen")

####################
# 第2問
####################
ldp_2012_dt <- hr_dt[year == 2012 & party_jpn == "自民党"]
plot(ldp_2012_dt$age, ldp_2012_dt$voteshare,
     main = "Vote Share vs. Age for LDP Candidates (2012)",
     xlab = "Age", ylab = "Vote Share (%)", pch = 16, col = "purple")

####################
# 第3問
####################
t_test_dt <- hr_dt[year == 2012 & party_jpn %in% c("自民党", "民主党")]
mean_voteshares_dt <- t_test_dt[, .(mean_voteshare = mean(voteshare, na.rm = TRUE)), by = party_jpn]
print("各党の平均得票率:")
print(mean_voteshares_dt)

t_test_result_dt <- t.test(voteshare ~ party_jpn, data = t_test_dt)
print("t検定の結果:")
print(t_test_result_dt)

####################
# 第4問
####################
regression_data_2012_dt <- hr_dt[year == 2012]
model_dt <- lm(voteshare ~ expm + age, data = regression_data_2012_dt)
print(summary(model_dt))