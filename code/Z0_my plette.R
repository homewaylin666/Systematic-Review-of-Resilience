library(viridis)        # 提供 viridis、magma、inferno.. 等色盤
library(ggplot2)        # 繪圖
library(scales)         # show_col() 方便顯示色盤

# 設定要幾個顏色，例如 10 色
n <- 7

# 用 viridis 套件
viridis   <- viridis(n)        # 默認調色盤是 "viridis"
magma     <- magma(n)          # 其他調色盤示例
inferno   <- inferno(n)
turbo <- turbo(n)

# 顯示 viridis 
show_col(viridis, border = "white")
show_col(turbo, border = "white")
