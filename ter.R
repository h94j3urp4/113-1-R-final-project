data <- read.csv("113年11月抽驗結果不合格名冊.csv", header = TRUE, fileEncoding = "UTF-8")
head(data)  # 查看前幾筆資料
str(data)
summary(data)
subset(data, 產地 == "彰化縣")
table(data$檢體名稱)
barplot(table(data$檢體名稱), main = "檢體名稱分布", col = "lightblue", las = 2)
nantou_data <- subset(data, 產地 == "南投縣")
head(nantou_data)  # 確認篩選後的資料
# 計算檢體名稱的頻率表
nantou_samples <- table(nantou_data$檢體名稱)

# 繪製長條圖
barplot(nantou_samples, 
        main = "南投縣檢體名稱分布", 
        col = "lightblue", 
        las = 2, 
        xlab = "檢體名稱", 
        ylab = "數量")
hist(nantou_data$總重量.公斤., 
     main = "南投縣檢體總重量分布", 
     col = "lightgreen", 
     xlab = "總重量（公斤）", 
     ylab = "頻率", 
     breaks = 10)
boxplot(總重量.公斤. ~ 檢體名稱, 
        data = nantou_data, 
        main = "南投縣檢體名稱與總重量分布", 
        col = "orange", 
        xlab = "檢體名稱", 
        ylab = "總重量（公斤）", 
        las = 2)
nantou_reasons <- table(nantou_data$不符規定原因_檢出濃度ppm.容許量ppm.)

# 繪製圓餅圖
pie(nantou_reasons, 
    main = "南投縣不符規定原因分布", 
    col = rainbow(length(nantou_reasons)))
barplot(nantou_reasons, 
        main = "南投縣不符規定原因分布", 
        col = "lightblue", 
        las = 2,  # 讓 X 軸標籤垂直顯示，避免重疊
        xlab = "不符規定原因", 
        ylab = "次數")
city_violations <- table(data$產地)
city_violations  # 檢查每個縣市的違規次數
barplot(city_violations, 
        main = "各縣市違規次數分布", 
        col = "skyblue", 
        las = 2,      # 讓 X 軸標籤垂直顯示
        xlab = "縣市", 
        ylab = "違規次數")
pie(city_violations, 
    main = "各縣市違規比例", 
    col = rainbow(length(city_violations)))
# 載入必要套件
library(tidyverse)

# 讀取資料
data <- read.csv("113年11月抽驗結果不合格名冊.csv", fileEncoding = "UTF-8")

# 篩選有用的欄位
clean_data <- data %>%
  select(檢體名稱, 不符規定原因_檢出濃度ppm（容許量ppm）) %>%
  filter(!is.na(檢體名稱))
names(data)
clean_data <- data %>%
  select(檢體名稱, 不符規定原因_檢出濃度ppm.容許量ppm.) %>%
  filter(!is.na(檢體名稱))
# 計算每種檢體名稱的違規次數
veggie_violation <- clean_data %>%
  group_by(檢體名稱) %>%
  summarise(違規次數 = n()) %>%
  arrange(desc(違規次數))

# 查看結果
print(veggie_violation)
# 繪製長條圖
ggplot(veggie_violation, aes(x = reorder(檢體名稱, 違規次數), y = 違規次數, fill = 檢體名稱)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # 將長條圖水平翻轉，便於顯示
  labs(title = "各蔬菜違規次數統計", x = "檢體名稱", y = "違規次數") +
  theme_minimal() +
  theme(legend.position = "none")  # 隱藏圖例
# 匯出分析結果到 CSV
write.csv(veggie_violation, "蔬菜違規次數統計.csv", row.names = FALSE)

# 保存圖表為圖片
ggsave("蔬菜違規次數統計.png", width = 8, height = 6)
thanks for watch
 