library(readxl)
 HW3_Dataset_Kopya <- read_excel("C:/Users/AHMET MET??N/Desktop/HW3_Dataset - Kopya.xlsx")
 View(HW3_Dataset_Kopya)
 data <- read_excel("C:/Users/AHMET MET??N/Desktop/HW3_Dataset - Kopya.xlsx")
 library(ggplot2)
ggplot(data, aes(x = Policy_Rate, y = Inflation_Gap)) + geom_point() + labs(title = "Policy Rate vs Inflation Gap", x = "Policy Rate" , y = "Inflation Gap")
 ggplot(data, aes(x = Policy_Rate, y = Output_Gap)) + geom_point() + labs(title = "Policy Rate vs Output Gap", x = "Policy Rate" , y = "Output Gap")
 ggplot(data, aes(x = Inflation_Gap, y = Output_Gap)) + geom_point() + labs(title = "Inflation Gap vs Output Gap", x = "Inflation Gap" , y = "Output Gap")
 library(ggcorrplot)
cor_matrix <- cor(data[, c("Policy_Rate", "Inflation_Gap", "Output_Gap")])
 ggcorrplot(cor_matrix, lab = TRUE, method = "circle")
 library(psych)

 describe(data[, c("Policy_Rate", "Inflation_Gap", "Output_Gap")])


ggplot(data, aes(x = 1:nrow(data), y = Policy_Rate)) + geom_line() + labs(title = "Policy Rate Over Time", x = "Time", y = "Policy_Rate")
ggplot(data, aes(x = 1:nrow(data), y = Inflation_Gap)) + geom_line() + labs(title = "Inflation Gap Over Time", x = "Time", y = "Infalation_Gap")
ggplot(data, aes(x = 1:nrow(data), y = Output_Gap)) + geom_line() + labs(title = "Output Gap Over Time", x = "Time", y = "Output_Gap")
library(dynlm)


library(ARDL)

taylor_model <- dynlm(Policy_Rate ~ Inflation_Gap + Output_Gap, data = data)
summary(taylor_model)



bounds_test <-auto_ardl(Policy_Rate ~ Inflation_Gap + Output_Gap, data = data, max_order = c(4,4,4))
summary(bounds_test)

bounds_f_result <- bounds_f_test(best_model, case = 3)
summary(bounds_f_result)
str(bounds_f_result)
 bounds_f_result$statistic
bounds_f_result$tab





