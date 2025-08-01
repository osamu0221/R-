# 氏名: 田中 修
# 学籍番号: ACA22182

library(tidyverse)
fraud_data <- read_csv("n_of_fraud.csv")
hr_data <- readRDS(url("https://github.com/yukiyanai/quant-methods-R/raw/refs/heads/master/data/hr96-17.Rds"))

####################
# 第1問
####################

# ggplot2を使ってグラフを作成
fraud_graph <- ggplot(data = fraud_data, aes(x = year, y = n)) +
  geom_line() + 
  labs(
    title = "詐欺事件認知件数の推移（1980年～2022年）",
    x = "年",
    y = "認知件数"
  ) +
  theme_minimal() 

print(fraud_graph)

ggsave("fraud_graph.png", plot = fraud_graph, width = 8, height = 6)

####################
# 第2問
####################

ldp_2012_data <- hr_data %>%
  filter(year == 2012, party_jpn == "自民党")

scatter_plot <- ggplot(data = ldp_2012_data, aes(x = age, y = voteshare)) +
  geom_point() + 
  labs(
    title = "2012年衆院選における自民党候補者の年齢と得票率",
    x = "年齢",
    y = "得票率 (%)"
  ) +
  theme_minimal()

print(scatter_plot)

ggsave("ldp_scatter_2012.png", plot = scatter_plot, width = 8, height = 6)

####################
# 第3問
####################

t_test_data <- hr_data %>%
  filter(year == 2012, party_jpn %in% c("自民党", "民主党"))

mean_voteshares <- t_test_data %>%
  group_by(party_jpn) %>%
  summarize(mean_voteshare = mean(voteshare, na.rm = TRUE))

print("各党の平均得票率:")
print(mean_voteshares)

t_test_result <- t.test(voteshare ~ party_jpn, data = t_test_data)

print("t検定の結果:")
print(t_test_result)

####################
# 第4問
####################

regression_data_2012 <- hr_data %>%
  filter(year == 2012)

model <- lm(voteshare ~ expm + age, data = regression_data_2012)

model_summary <- summary(model)
print(model_summary)