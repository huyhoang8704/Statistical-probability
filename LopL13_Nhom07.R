library(writexl)
library(ggplot2)
library(dplyr)
library(plotly)
library(nortest)
library(car)

  # Đặt đường dẫn của file csv vào biến path
  path <- "C:/Users/Admin/source/repos/XSTK/Intel_CPUs.csv"
  
  # Sử dụng hàm read.csv để đọc file
  data <- read.csv(file = path, header = TRUE, sep = ",")
  # In ra 6 dòng đầu tiên của dữ liệu
  head(data)
  

  # Tạo dataframe mới chỉ bao gồm các cột cần quan tâm
  df <- data[, c("Vertical_Segment", "Lithography", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "TDP", "Max_Memory_Size", "Max_Memory_Bandwidth", "Max_nb_of_PCI_Express_Lanes")]
  # In ra 6 dòng đầu tiên của dữ liệu mới
  head(df)
  

  # Chuyển đổi cột 'Vertical_Segment' thành biến phân loại
  df$Vertical_Segment <- as.factor(df$Vertical_Segment)
  
  # Định dạng dữ liệu
  df$Lithography <- as.numeric(gsub(" nm", "", df$Lithography))
  # Hàm để chuyển đổi GHz và MHz thành MHz
  convert_frequency <- function(frequency) {
    if (grepl("GHz", frequency)) {
      return(as.numeric(gsub(" GHz", "", frequency)) * 1000)
    } else if (grepl("MHz", frequency)) {
      return(as.numeric(gsub(" MHz", "", frequency)))
    } else {
      return(NA)
    }
  }
  
  # Áp dụng hàm cho cột Processor_Base_Frequency
  df$Processor_Base_Frequency <- sapply(df$Processor_Base_Frequency, convert_frequency)
  df$TDP <- as.numeric(gsub(" W", "", df$TDP))
  # Hàm để chuyển đổi GB và TB thành GB
  convert_memory_size <- function(memory_size) {
    if (grepl("TB", memory_size)) {
      return(as.numeric(gsub(" TB", "", memory_size)) * 1024)
    } else if (grepl("GB", memory_size)) {
      return(as.numeric(gsub(" GB", "", memory_size)))
    } else {
      return(NA)
    }
  }
  
  # Áp dụng hàm cho cột Max_Memory_Size
  df$Max_Memory_Size <- sapply(df$Max_Memory_Size, convert_memory_size)
  df$Max_Memory_Bandwidth <- as.numeric(gsub(" GB/s", "", df$Max_Memory_Bandwidth))
  
  # In ra 6 dòng đầu tiên của dữ liệu mới
  head(df)
  
  # Thống kê số lượng dữ liệu khuyết thiếu trong các biến
  apply(is.na(df),2,sum)
  
  # Thống kê tỷ lệ dữ liệu khuyết thiếu trong các biến
  apply(is.na(df),2,mean)
  
  # Hàm để thay thế NA bằng trung vị
  replace_na_with_median <- function(x) {
    median_value <- median(x, na.rm = TRUE)
    x[is.na(x)] <- median_value
    return(x)
  }
  
  # Chỉ áp dụng hàm cho các cột số
  numeric_columns <- sapply(df, is.numeric)
  df[numeric_columns] <- sapply(df[numeric_columns], replace_na_with_median)
  # Chuyển lại kết quả thành khung dữ liệu
  df <- as.data.frame(df)
  
  # Kiểm tra lại dữ liệu khuyết thiếu trong các biến
  apply(is.na(df),2,sum)
  
  # In ra 20 dòng đầu tiên của dữ liệu mới
  head(df,20)
  
  # In ra số lượng xuất hiện của mỗi hạng mục
  table(df$Vertical_Segment)
  
  # Tính IQR cho mỗi cột số
  IQR_values <- sapply(df[sapply(df, is.numeric)], IQR)
  
  # Tính giới hạn dưới và trên
  lower_bounds <- sapply(df[sapply(df, is.numeric)], quantile, probs = 0.25) - 1.5 * IQR_values
  upper_bounds <- sapply(df[sapply(df, is.numeric)], quantile, probs = 0.75) + 1.5 * IQR_values
  
  # Tìm giá trị ngoại lai
  outliers <- lapply(names(df), function(i) {
    if (is.numeric(df[[i]])) {
      df[[i]] < lower_bounds[i] | df[[i]] > upper_bounds[i]
    } else {
      rep(FALSE, length(df[[i]]))
    }
  })
  # Xem cấu trúc của danh sách 'outliers'
  str(outliers)
  
  # Chuyển đổi 'df' trở lại thành khung dữ liệu
  df <- as.data.frame(df)
  
  #* Thống kê tả các dữ liệu số
  summary(df)
  by(df, df$Vertical_Segment, summary)
  
  #* Vẽ biểu đồ Histogram
  hist(df$Processor_Base_Frequency, main = "Histogram of Processor Base Frequency", xlab = "Processor Base Frequency (MHz)", col = "blue", border = "black")
  
  #* Vẽ biểu đồ Boxplot , chia theo biến Vertical_Segment
  boxplot(df$Processor_Base_Frequency ~ df$Vertical_Segment, main = "Boxplot of Processor Base Frequency by Vertical Segment", xlab = "Vertical Segment", ylab = "Processor Base Frequency (MHz)", col = "blue")
  


  #* Vẽ biểu đồ phân tán của Processor Base Frequency với các biến
  numeric_columns <- df[sapply(df, is.numeric)]  # chọn các cột chỉ có số 
  
  # Loại bỏ cột Processor_Base_Frequency khỏi danh sách
  numeric_columns <- numeric_columns[!names(numeric_columns) %in% "Processor_Base_Frequency"] 
  
  library(ggplot2)  # vẽ biểu đồ phân tán
  for (col_name in names(numeric_columns)) {
    p <- ggplot(df, aes(x = !!sym(col_name), y = Processor_Base_Frequency)) +
      geom_point() +
      labs(x = col_name, y = "Processor Base Frequency (MHz)", title = paste("Processor Base Frequency vs", col_name)) +
      theme_minimal()
    print(p)
  }
  


  #* Xây dựng mô hình hồi quy tuyến tính
  model <- lm(Processor_Base_Frequency ~ ., data = df) # biến y theo các biến còn lại 
  
  #! In ra kết quả
  summary(model)
  
  #! Xây dựng lại mô hình hồi quy tuyến tính
  model <- lm(Processor_Base_Frequency ~ Vertical_Segment + Lithography + nb_of_Cores + TDP + Max_Memory_Size, data = df)
  summary(model) 
  
  #! Vẽ đồ thị kiểm định
  plot (model)
  
  #! Dự báo
  plot (df$Processor_Base_Frequency , predict ( model , df),xlab =" Processor_Base_Frequency ",ylab =" Predicted Processor_Base_Frequency ",main =" Processor_Base_Frequency and Predicted Processor_Base_Frequency ")
  compair <-lm( predict ( model , df)~Processor_Base_Frequency , data = df)
  abline ( compair ,col =" red ")
  
  #! plot(fitted(model)) vẽ mô hình dự báo 



#* ANOVA

# Điều kiện thứ nhất : Các quan sát được lấy độc lập , mình thực hiện nghiên cứu nên biết được các mẫu là độc lập
# Điều kiện thứ hai: Kiểm định các nhóm có pp chuẩn tổng thể
av_residual <- rstandard(aov(Processor_Base_Frequency ~ Lithography*TDP,data = df))
qqnorm(av_residual)
qqline(av_residual) # hình 1

# Điều kiện thứ ba: Kiểm định tính đồng nhất phương sai của tổng thể
leveneTest(Processor_Base_Frequency ~ Lithography*TDP,data = df) # hình 2

# ANOVA
model <- aov(Processor_Base_Frequency ~ Lithography*TDP,data = df) # hình 3
summary(model)
