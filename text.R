library(writexl)
library(ggplot2)
library(dplyr)
library(plotly)
library(openxlsx)
library(readxl)
library(nortest)
library(car)


  # Đặt đường dẫn của file csv vào biến path
  path <- "C:/Users/Admin/source/repos/XSTK/CPU_new.xlsx"

df <- read.xlsx(path)

  #* Xây dựng mô hình hồi quy tuyến tính
  model <- lm(Processor_Base_Frequency ~ ., data = df) # biến y theo các biến còn lại 
  
  #! In ra kết quả
  
  #! Xây dựng lại mô hình hồi quy tuyến tính
  model <- lm(Processor_Base_Frequency ~ Vertical_Segment + Lithography + nb_of_Cores + TDP + Max_Memory_Size, data = df)
  
  #! Vẽ đồ thị kiểm định
  plot (model)
 