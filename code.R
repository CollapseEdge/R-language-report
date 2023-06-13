## ----clear-----------------------------------------------------------------------------------
#消除之前运行代码留下的环境变量
rm(list = ls())

## ----加载模块,message = FALSE--------------------------------------------------------------------
library(tidyverse)
library(VIM)
library(hexbin)
library(ggridges)
options (warn = -1)

## ----读取数据------------------------------------------------------------------------------------
data1 = read.csv("C:\\Users\\che\\Desktop\\R语言期末报告\\Air_Quality.csv")
summary(data1)
head(data1)
aggr(data1,prop=FALSE,numbers=TRUE,cex.axis=.5)
sum(is.na(data1$Message))


data2 = data1[which(data1$Name== 'PM2.5-Attributable Deaths'), ]
pm0911 = data2[which(data2$Time.Period== '2009-2011'), ]
ggplot(pm0911,mapping=aes(x=Geo.Place.Name,y=Data.Value,fill=Geo.Place.Name))+geom_bar(stat = "identity")+theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1))+labs(x = "地区", y = "每千人中的死亡人数", title = "2009-2011年不同地区的每千人死亡人数-与PM2.5相关")

## ----pic2------------------------------------------------------------------------------------
library(ggplot2)

data3 <- data1[which(data1$Name == 'Ozone (O3)'), ]
o3_ny <- data3[which(data3$Geo.Place.Name == 'New York City'), ]
o3_Bronx <- data3[which(data3$Geo.Place.Name == 'Bronx'), ]
o3_High_Bridge_Morrisania <- data3[which(data3$Geo.Place.Name == 'High Bridge - Morrisania'), ]
o3_Crotona_Tremont <- data3[which(data3$Geo.Place.Name == 'Crotona -Tremont'), ]
o3_East_New_York <- data3[which(data3$Geo.Place.Name == 'East New York'), ]
o3_Sunset_Park <- data3[which(data3$Geo.Place.Name == 'Sunset Park'), ]
o3_Borough_Park <- data3[which(data3$Geo.Place.Name == 'Borough Park'), ]

# 创建一个新的列 "Location"，用于区分折线
o3_ny$Location <- "New York City"
o3_Bronx$Location <- "Bronx"
o3_High_Bridge_Morrisania$Location <- "High Bridge - Morrisania"
o3_Crotona_Tremont$Location <- "Crotona -Tremont"
o3_East_New_York$Location <- "East New York"
o3_Sunset_Park$Location <- "Sunset Park"
o3_Borough_Park$Location <- "Borough Park"

# 合并数据框
o3_combined <- rbind(o3_ny, o3_Bronx, o3_High_Bridge_Morrisania, o3_Crotona_Tremont, o3_East_New_York, o3_Sunset_Park, o3_Borough_Park)

# 绘制折线图
ggplot(data = o3_combined, mapping = aes(x = Time.Period, y = Data.Value, color = Location, group = Location)) +
  geom_line() +
  xlab('年份') +
  ylab('含量') +
  ggtitle('2009-2017年臭氧含量的变化') +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1))

## ----pic3------------------------------------------------------------------------------------
library(ggplot2)

data_filtered <- data1[which(data1$Name == 'O3-Attributable Cardiac and Respiratory Deaths'), ]
o3_location1 <- data_filtered[which(data_filtered$Geo.Place.Name == 'New York City'), ]
o3_location2 <- data_filtered[which(data_filtered$Geo.Place.Name == 'Bronx'), ]
o3_location3 <- data_filtered[which(data_filtered$Geo.Place.Name == 'High Bridge - Morrisania'), ]
o3_location4 <- data_filtered[which(data_filtered$Geo.Place.Name == 'Crotona -Tremont'), ]
o3_location5 <- data_filtered[which(data_filtered$Geo.Place.Name == 'East New York'), ]
o3_location6 <- data_filtered[which(data_filtered$Geo.Place.Name == 'Sunset Park'), ]
o3_location7 <- data_filtered[which(data_filtered$Geo.Place.Name == 'Borough Park'), ]

# 创建一个新的列 "Location"，用于区分折线
o3_location1$Location <- "New York City"
o3_location2$Location <- "Bronx"
o3_location3$Location <- "High Bridge - Morrisania"
o3_location4$Location <- "Crotona -Tremont"
o3_location5$Location <- "East New York"
o3_location6$Location <- "Sunset Park"
o3_location7$Location <- "Borough Park"

# 合并数据框
o3_combined <- rbind(o3_location1, o3_location2, o3_location3, o3_location4, o3_location5, o3_location6, o3_location7)

# 绘制折线图
ggplot(data = o3_combined, mapping = aes(x = Time.Period, y = Data.Value, color = Location, group = Location)) +
  geom_line() +
  xlab('年份') +
  ylab('含量') +
  ggtitle('O3-Attributable Cardiac and Respiratory Deaths') +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1))

## --------------------------------------------------------------------------------------------
ggplot(data1, aes(x = Geo.Type.Name)) + geom_area(stat = "density", fill = "cadetblue1")+
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1))


p <- ggplot(data1, aes(
  x = Data.Value)) 
p + geom_histogram(mapping = aes(fill = Geo.Type.Name))
## ----pic4------------------------------------------------------------------------------------
SO2_data <- data1[which(data1$Name == 'Sulfur Dioxide (SO2)'), ]
line_data <- data.frame(Time.Period = SO2_data$Time.Period,
                        Value = SO2_data$Data.Value,
                        Category = SO2_data$Geo.Place.Name)

ggplot(line_data, aes(x = Time.Period, y = Value, color = Category, group = Category)) +
    geom_line() +
    xlab('Time Period') +
    ylab('Value') +
    theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
          legend.position = "none")


#' ### Data Set - 2
## --------------------------------------------------------------------------------------------
#消除之前运行代码留下的环境变量
rm(list = ls())

## ----加载第二个数据集--------------------------------------------------------------------------------
data <- read.csv("C:\\Users\\che\\Desktop\\R语言期末报告\\rdu-weather-history.csv", sep = ";")

summary(data)
head(data)

## ----检测数据集的缺失情况------------------------------------------------------------------------------
aggr(data,prop=FALSE,numbers=TRUE)
sum(is.na(data$snwd))
sum(is.na(data$awnd))

## ----ds2_pic1--------------------------------------------------------------------------------
# 提取年份信息
data$Year <- format(as.Date(data$date), "%Y")

# 按年份和最高温度分组
grouped_data <- aggregate(tmax ~ Year, data, max)

# 绘制散点图
ggplot(grouped_data, aes(x = Year, y = tmax)) +
  geom_point(size = 3, color = "blue", shape = 16) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "red") +
  xlab("年份") +
  ylab("最高温度") +
  labs(title = "每年的最高温度", subtitle = "华氏度") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 10, color = "black"))

## --------------------------------------------------------------------------------------------
# 提取年份信息
data$Year <- format(as.Date(data$date), "%Y")

# 按年份和最低温度分组
grouped_data <- aggregate(tmin ~ Year, data, min)

ggplot(grouped_data, aes(x = Year, y = tmin)) +
  geom_bar(stat = "identity", fill = grouped_data$Year) +
  xlab("年份") +
  ylab("最低温度") +
  ggtitle("每年的最低温度/华氏度")

## --------------------------------------------------------------------------------------------
library(ggplot2)

# Convert 'date' column to Date class
data$date <- as.Date(data$date)

# Subset data for the year 2017
data_2017 <- subset(data, format(date, "%Y") == "2017")

# Create a new column for month
data_2017$Month <- format(data_2017$date, "%b")

# Plot the line chart
ggplot(data_2017, aes(x = date)) +
  geom_line(aes(y = tmax, color = "最高温度"), linetype = "solid") +
  geom_line(aes(y = tmin, color = "最低温度"), linetype = "dashed") +
  xlab("月份") +
  ylab("温度") +
  ggtitle("2017年每天的最高温度和最低温度") +
  scale_color_manual(values = c("最高温度" = "red", "最低温度" = "blue")) +
  theme(legend.position = "top")

## --------------------------------------------------------------------------------------------
# Subset data for the year 2018
data_2018 <- subset(data, format(date, "%Y") == "2018")

# Create a new column for month
data_2018$Month <- format(data_2018$date, "%b")

ggplot(data = data_2018, aes(x = Month, y = tmax, fill = Month)) +
  geom_boxplot() +
  labs(x = "month", y = "temp") +
  coord_flip()
## --------------------------------------------------------------------------------------------
# Subset data for the year 2019
data_2019 <- subset(data, format(date, "%Y") == "2019")

# Create a new column for month
data_2019$Month <- format(data_2019$date, "%b")

ggplot(data_2019, aes(
  x = tmin, 
  y = Month,
  fill = Month)) +
  geom_density_ridges(alpha = 0.5) +
  guides(fill = FALSE) +
  labs(
    x = "Month",
    y = "temp")

## --------------------------------------------------------------------------------------------
# Subset data for the year 2020
data_2020 <- subset(data, format(date, "%Y") == "2020")

# Create a new column for month
data_2020$Month <- format(data_2020$date, "%b")

ggplot(data_2020, aes(
  x = awnd, 
  y = Month,
  fill = Month)) +
  geom_violin() +
  labs(x = "awnd",
       y = "Month") +
  coord_flip() 

## --------------------------------------------------------------------------------------------
# Subset data for the year 2021
data_2021 <- subset(data, format(date, "%Y") == "2021")

# Create a new column for month
data_2021$Month <- format(data_2021$date, "%b")

ggplot(data_2021, aes(
  x = Month,
  y = (tmax + tmin)/2 )) +
  geom_point(size = 1.2) +
  geom_pointrange(mapping = aes(
    ymin = tmin,
    ymax = tmax)) +
  labs(
    x = "Month",
    y = "Temp")

## ----message=FALSE---------------------------------------------------------------------------
ggplot(data = data_2017, mapping = aes(x = tmax, y = awnd)) + geom_hex(bins = 25) + scale_fill_gradient(
    low = "#9696F2", 
    high = "#0A0A3D")

## --------------------------------------------------------------------------------------------
ggplot(data = data_2017, mapping = aes(x = tmax, y = awnd)) + stat_density_2d(color = "black", size = 0.6) + geom_density_2d_filled(
  mapping = aes(fill = ..level..))

library(GGally)
ggpairs(data)

library(corrplot)
data <- na.omit(data)
data$date <- as.numeric(as.character(data$date))  # 将日期变量转换为字符型，再转换为数值型
data$tmin <- as.numeric(as.character(data$tmin))
data$tmax <- as.numeric(as.character(data$tmax))
data$prcp <- as.numeric(as.character(data$prcp))
data$snow <- as.numeric(as.character(data$snow))
data$snwd <- as.numeric(as.character(data$snwd))
data$awnd <- as.numeric(as.character(data$awnd))
typeof(data)
cor_matrix <- as.matrix(as.numeric(unlist(data)))

# 绘制相关系数矩阵的热力图
corrplot(cor_matrix, method = "color")

#' ## PART 3
## ----准备工作------------------------------------------------------------------------------------
getwd()
setwd("C:\\Users\\che\\Desktop\\R语言期末报告")
getwd()
#消除之前运行代码留下的环境变量
rm(list = ls())
## ----加载库,message = FALSE---------------------------------------------------------------------
library(caret)
library(ggplot2)
library(randomForest)
library(xgboost)
library(VIM)
library(zoo)
library(corrplot)
library(Metrics)
library(dplyr)
## ----载入数据------------------------------------------------------------------------------------
data <- read.csv("C:\\Users\\che\\Desktop\\R语言期末报告\\london_weather.csv")
summary(data)
## ----查看数据缺失情况--------------------------------------------------------------------------------
aggr(data,prop=FALSE,numbers=TRUE,cex.axis=.5)
## ----判断数据缺失具体值-------------------------------------------------------------------------------
print_missing_percentage <- function(data) {
for (column in colnames(data)) {
missing_percentage <- round(sum(is.na(data[[column]]) / length(data$date) * 100), 2)
print(paste(column, "The missing percentage is(%):", paste(missing_percentage, "%", sep = "")))
}
}
#构造一个函数，可以查看数据集的缺失占比情况
print_missing_percentage(data)
## ----数据预处理-----------------------------------------------------------------------------------
# 使用均值填充缺失值
columns_to_fill <- c("cloud_cover", "global_radiation", "max_temp", "mean_temp", "min_temp", "precipitation", "pressure")
for (col in columns_to_fill) {
data[[col]] <- ifelse(is.na(data[[col]]), mean(data[[col]], na.rm = TRUE), data[[col]])
}
#可以看到填充是有效果的
print_missing_percentage(data)
## --------------------------------------------------------------------------------------------
# 对 snow_depth 进行样条插值
ts_data <- zoo(data$snow_depth, order.by = as.Date(data$date))
#na.spline() 函数进行样条插值，该函数可以自动进行缺失值插值，不需要手动删除缺失值。其中， na.spline() 函数的参数 xout 用于指定插值结果的时间序列，这里我们与原数据保持一致。
interpolated_snow_depth <- na.spline(ts_data, xout = time(ts_data))
data$snow_depth <- as.integer(interpolated_snow_depth)
print_missing_percentage(data)#查看数据缺失情况
## --------------------------------------------------------------------------------------------
head(data)
tail(data)
## --------------------------------------------------------------------------------------------
library(GGally)
ggpairs(data)
## ----相关性分析-----------------------------------------------------------------------------------
cor_matrix <- cor(data)
corrplot(cor_matrix, method = "circle")
## ----计算相关性-----------------------------------------------------------------------------------
correlations <- cor(data)[,'mean_temp']
print(correlations)
## ----删除无用的列----------------------------------------------------------------------------------
tr <- data['mean_temp']
ft <- data[, !(names(data) %in% c("date", "mean_temp", "pressure", "precipitation"))]
## ----划分训练集和测试集-------------------------------------------------------------------------------
# 计算切分比例
split_ratio <- 0.8
# 计算切分的索引位置
split_index <- round(split_ratio * nrow(data))

# 切分数据
train_data_feature <- ft[1:split_index, ]
train_data_target <- as.data.frame(tr[1:split_index, ])
names(train_data_target) <- c("mean_temp")
test_data_feature <- ft[(split_index+1):nrow(data), ]
test_data_target <- as.data.frame(tr[(split_index+1):nrow(data), ])
names(test_data_target) <- c("mean_temp")
typeof(train_data_feature)
typeof(train_data_target)
typeof(test_data_feature)
typeof(test_data_target)
## ----xgboost---------------------------------------------------------------------------------
# 将训练数据框转换为矩阵
ft_train_matrix <- as.matrix(train_data_feature)
# 将标签转换为矩阵
tr_train_matrix <- as.matrix(train_data_target)
# 创建XGBoost回归模型
xg_reg <- xgboost(data = ft_train_matrix,
label = tr_train_matrix,
objective = "reg:squarederror",
colsample_bytree = 0.3,
learning_rate = 0.1,
max_depth = 5,
alpha = 10,
nrounds = 10)
# 将测试数据框转换为矩阵
ft_test_matrix <- as.matrix(test_data_feature)
# 进行预测
pred_xgboost <- predict(xg_reg, newdata = ft_test_matrix)
# 输出fit完成
print("finished the predict")
typeof(pred_xgboost)
typeof(ft_test_matrix)
typeof(tr_train_matrix)
typeof(tr_train_matrix)

## --------------------------------------------------------------------------------------------
unlist_test_tr <- unlist(test_data_target)
double_test_tr <- as.numeric(unlist_test_tr)

# 计算平均绝对误差
mae <- mean(abs(double_test_tr - pred_xgboost))
# 计算均方误差
mse <- mean((double_test_tr - pred_xgboost))
# 计算R-squared（决定系数）
ss_total <- sum((double_test_tr - mean(double_test_tr))^2)
ss_residual <- sum((double_test_tr - pred_xgboost)^2)
r2 <- 1 - ss_residual/ss_total
xgboost_result <- function(){
  print(paste("Mean absolute error =", round(mae, 2)))
  print(paste("Mean squared error =", round(mse, 2)))
  print(paste("R-squared =", round(r2, 2)))
}

xgboost_result()
# 创建数据框
df <- data.frame(tr_test = double_test_tr, yc_pred = pred_xgboost)
# 创建散点图
ggplot(df, aes(x = tr_test, y = yc_pred)) 
geom_point(color = "blue") +
geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
labs(x = "Actual mean_temp", y = "Predicted mean_temp", title = "XgBoost Regression Model") +
theme_minimal()
## --------------------------------------------------------------------------------------------
# 创建随机森林回归器
regressor <- randomForest(ft_train_matrix, tr_train_matrix, ntree = 100, importance = TRUE)
# 进行预测
predictions2 <- predict(regressor, test_data_feature)
print("fininshed fit!")
# 计算均方误差
mse <- mean((double_test_tr - predictions2)^2)
# 计算中位数绝对误差
mae <- median(abs(double_test_tr - predictions2))
# 计算平均绝对误差
mAe <- mean(abs(double_test_tr - predictions2))
# 计算R-squared
r2 <- 1 - sum((double_test_tr - predictions2)^2) / sum((double_test_tr - mean(double_test_tr))^2)

randomforest_result <- function(){
  print(paste("Mean Squared Error:", mse))
  print(paste("Median absolute error =", round(mae, 2)))
  print(paste("Mean absolute error =", round(mAe, 2)))
  print(paste("R-squared:", round(r2, 3)))
}
randomforest_result()
regressor$importance
varImpPlot(regressor, main = "variable importance")
## --------------------------------------------------------------------------------------------
# 创建数据框
df <- data.frame(tr_test = double_test_tr, yc_pred = predictions2)
# 创建散点图
ggplot(df, aes(x = tr_test, y = yc_pred)) +
geom_point(color = "blue") +
geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
labs(x = "Actual mean_temp", y = "Predicted mean_temp", title = "RandomForest Regression Model") +
theme_minimal()