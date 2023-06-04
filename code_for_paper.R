#数据准备
#清空工作空间
rm(list = ls())
setwd("C:\\code")

#install.packages("boot")
library(boot)  # 载入boot包

source("C:\\code\\CI.R")
mean_for_boot <- function(dat, ind){
  c(mean(dat[ind]), var(dat[ind])/length(dat))#计算均值和方差除数据量
} 


sample_num = 10
#sample_num = 20
#sample_num = 50
#sample_num = 100
df = c(1,2,5,10)
boot_eval = array(0,c(4,5,4))#建立数组，规则如此所示
for (i in 1:4){
  for (k in 1:400){
    sample_chisq = rchisq(sample_num, df[i])/df[i]#为了方差单位化
    ci_t <- CI_normal(sample_chisq)#t分布的置信区间
    mu.boot <- boot(sample_chisq, mean_for_boot, R = 1000)
    boot_ci <- boot.ci(mu.boot)
    abc_ci <- abc.ci(sample_chisq, function(x, w) sum(x*w))
    boot_ci_matrix <- rbind(ci_t, boot_ci$student[,4:5],boot_ci$percent[,4:5], boot_ci$bca[,4:5], abc_ci[2:3])
    rownames(boot_ci_matrix) <- c("student-t", "bootstrap-t", "percentile", "BCa", "Approximate BCa")
    for (j in 1:5){
      boot_eval[1,j,i] <- boot_eval[1,j,i] + as.numeric((1 >= boot_ci_matrix[j,1]) & (1 <= boot_ci_matrix[j,2]))
      boot_eval[2,j,i] <- boot_eval[2,j,i] + boot_ci_matrix[j,2] - boot_ci_matrix[j,1]
      boot_eval[3,j,i] <- boot_eval[3,j,i] + as.numeric(1 < boot_ci_matrix[j,1])
      boot_eval[4,j,i] <- boot_eval[4,j,i] + as.numeric(1 > boot_ci_matrix[j,2])
    }
  }
}
boot_eval <- boot_eval/4
boot_eval[2,,] <- boot_eval[2,,]/100
round(boot_eval,2)


#t分布
sample_num = 10
#sample_num = 20
#sample_num = 50
#sample_num_1 = 100
df_1 = c(3,5,10,20)
boot_eval_1 = array(0,c(4,5,4))#建立数组，规则如此所示
for (i in 1:4){
  for (k in 1:400){
    sample_t = rt(sample_num, df_1[i])#为了方差单位化
    ci_t1 <- CI_normal(sample_t)#t分布的置信区间
    mu.boot_1 <- boot(sample_t, mean_for_boot, R = 1000)
    boot_ci_1 <- boot.ci(mu.boot_1)
    abc_ci_1 <- abc.ci(sample_t, function(x, w) sum(x*w))
    boot_ci_matrix_1 <- rbind(ci_t1, boot_ci_1$student[,4:5],
                            boot_ci_1$percent[,4:5], boot_ci_1$bca[,4:5], abc_ci_1[2:3])
    rownames(boot_ci_matrix_1) <- c("student-t", "bootstrap-t", "percentile", "BCa", "Approximate BCa")
    for (j in 1:5){
      boot_eval_1[1,j,i] <- boot_eval_1[1,j,i] + as.numeric((0 >= boot_ci_matrix_1[j,1]) & (0 <= boot_ci_matrix_1[j,2]))
      boot_eval_1[2,j,i] <- boot_eval_1[2,j,i] + boot_ci_matrix_1[j,2] - boot_ci_matrix_1[j,1]
      boot_eval_1[3,j,i] <- boot_eval_1[3,j,i] + as.numeric(0 < boot_ci_matrix_1[j,1])
      boot_eval_1[4,j,i] <- boot_eval_1[4,j,i] + as.numeric(0 > boot_ci_matrix_1[j,2])
    }
  }
}
boot_eval_1 <- boot_eval_1/4
boot_eval_1[2,,] <- boot_eval_1[2,,]/100
round(boot_eval_1,2) 

#install.packages("readxl")

# 加载readxl包
library(readxl)

# 读取Excel文件,这个Excel文件存储着模拟的数据
Data_simulation_Chi <- data.frame(read_excel("C:\\code\\data.xlsx",sheet = "Chi"))
Data_simulation_t <- data.frame(read_excel("C:\\code\\data.xlsx",sheet = "t"))

Data_simulation_Chi_re <- replace(Data_simulation_Chi, is.na(Data_simulation_Chi), 0)
Data_simulation_t_re <- replace(Data_simulation_t, is.na(Data_simulation_t), 0)

#install.packages("ggplot2")

# 加载ggplot2包
library(ggplot2)

x_all <- Data_simulation_Chi_re[1:4,3]

y_1 <- Data_simulation_Chi_re[1:4,4:8]

y_2 <- Data_simulation_Chi_re[5:8,4:8]

y_3 <- Data_simulation_Chi_re[9:12,4:8]

y_4 <- Data_simulation_Chi_re[13:16,4:8]

y_5 <- Data_simulation_Chi_re[17:20,4:8]

y_6 <- Data_simulation_Chi_re[21:24,4:8]

y_7 <- Data_simulation_Chi_re[25:28,4:8]

y_8 <- Data_simulation_Chi_re[29:32,4:8]
 
y_9 <- Data_simulation_Chi_re[33:36,4:8]

y_10 <- Data_simulation_Chi_re[37:40,4:8]

y_11 <- Data_simulation_Chi_re[41:44,4:8]

y_12 <- Data_simulation_Chi_re[45:48,4:8]
 
y_13 <- Data_simulation_Chi_re[49:52,4:8]

y_14 <- Data_simulation_Chi_re[53:56,4:8]

y_15 <- Data_simulation_Chi_re[57:60,4:8]

y_16 <- Data_simulation_Chi_re[61:64,4:8]

#构建数据表
data_1_1 <- data.frame(x = x_all, y = y_1)

# 绘制折线图
plot_1_1 <- ggplot(data_1_1, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "coverage  (n=10)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", 
                                "Percentile" = "green","BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_1_1

#构建数据表
data_1_2 <- data.frame(x = x_all, y = y_2)

# 绘制折线图
plot_1_2 <- ggplot(data_1_2, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "coverage  (n=20)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),                      
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_1_2

#构建数据表
data_1_3 <- data.frame(x = x_all, y = y_3)

# 绘制折线图
plot_1_3 <- ggplot(data_1_3, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "coverage  (n=50)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),                    
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_1_3

#构建数据表
data_1_4 <- data.frame(x = x_all, y = y_4)

# 绘制折线图
plot_1_4 <- ggplot(data_1_4, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "coverage  (n=100)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_1_4

#构建数据表
data_2_1 <- data.frame(x = x_all, y = y_5)

# 绘制折线图
plot_2_1 <- ggplot(data_2_1, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "width  (n=10)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_2_1

#构建数据表
data_2_2 <- data.frame(x = x_all, y = y_6)

# 绘制折线图
plot_2_2 <- ggplot(data_2_2, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "width  (n=20)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_2_2

#构建数据表
data_2_3 <- data.frame(x = x_all, y = y_7)

# 绘制折线图
plot_2_3 <- ggplot(data_2_3, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "width  (n=50)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_2_3

#构建数据表
data_2_4 <- data.frame(x = x_all, y = y_8)

# 绘制折线图
plot_2_4 <- ggplot(data_2_4, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "width  (n=100)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_2_4

#构建数据表
data_3_1 <- data.frame(x = x_all, y = y_9)

# 绘制折线图
plot_3_1 <- ggplot(data_3_1, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss left(%)  (n=10)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_3_1

#构建数据表
data_3_2 <- data.frame(x = x_all, y = y_10)

# 绘制折线图
plot_3_2 <- ggplot(data_3_2, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss left(%)  (n=20)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_3_2

#构建数据表
data_3_3 <- data.frame(x = x_all, y = y_11)

# 绘制折线图
plot_3_3 <- ggplot(data_3_3, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss left(%)  (n=50)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_3_3

#构建数据表
data_3_4 <- data.frame(x = x_all, y = y_12)

# 绘制折线图
plot_3_4 <- ggplot(data_3_4, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss left(%)  (n=100)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_3_4

#构建数据表
data_4_1 <- data.frame(x = x_all, y = y_13)

# 绘制折线图
plot_4_1 <- ggplot(data_4_1, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss right(%)  (n=10)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_4_1

#构建数据表
data_4_2 <- data.frame(x = x_all, y = y_14)

# 绘制折线图
plot_4_2 <- ggplot(data_4_2, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss right(%)  (n=20)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_4_2

#构建数据表
data_4_3 <- data.frame(x = x_all, y = y_15)

# 绘制折线图
plot_4_3 <- ggplot(data_4_3, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss right(%)  (n=50)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_4_3

#构建数据表
data_4_4 <- data.frame(x = x_all, y = y_16)

# 绘制折线图
plot_4_4 <- ggplot(data_4_4, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss right(%)  (n=100)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_4_4

#install.packages("gridExtra")
library(gridExtra)

#这行代码请单独运行，不然容易报错
plots <- list(plot_1_1, plot_1_2, plot_1_3, plot_1_4,
              plot_2_1, plot_2_2, plot_2_3, plot_2_4,
              plot_3_1, plot_3_2, plot_3_3, plot_3_4,
              plot_4_1, plot_4_2, plot_4_3, plot_4_4)

plot_all_chi <- grid.arrange(grobs = plots, nrow = 4, ncol = 4)

#基于t分布数据模拟绘制折线图

x_all_new <- Data_simulation_t_re[1:4,3]

y_17 <- Data_simulation_t_re[1:4,4:8]

y_18 <- Data_simulation_t_re[5:8,4:8]

y_19 <- Data_simulation_t_re[9:12,4:8]

y_20 <- Data_simulation_t_re[13:16,4:8]

y_21 <- Data_simulation_t_re[17:20,4:8]

y_22 <- Data_simulation_t_re[21:24,4:8]

y_23 <- Data_simulation_t_re[25:28,4:8]

y_24 <- Data_simulation_t_re[29:32,4:8]

y_25 <- Data_simulation_t_re[33:36,4:8]

y_26 <- Data_simulation_t_re[37:40,4:8]

y_27 <- Data_simulation_t_re[41:44,4:8]

y_28 <- Data_simulation_t_re[45:48,4:8]

y_29 <- Data_simulation_t_re[49:52,4:8]

y_30 <- Data_simulation_t_re[53:56,4:8]

y_31 <- Data_simulation_t_re[57:60,4:8]

y_32 <- Data_simulation_t_re[61:64,4:8]

#构建数据表
data_5_1 <- data.frame(x = x_all_new, y = y_17)

# 绘制折线图
plot_5_1 <- ggplot(data_5_1, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "coverage  (n=10)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", 
                                "Percentile" = "green","BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_5_1

#构建数据表
data_5_2 <- data.frame(x = x_all_new, y = y_18)

# 绘制折线图
plot_5_2 <- ggplot(data_5_2, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "coverage  (n=20)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),                      
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_5_2

#构建数据表
data_5_3 <- data.frame(x = x_all_new, y = y_19)

# 绘制折线图
plot_5_3 <- ggplot(data_5_3, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "coverage  (n=50)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),                    
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_5_3

#构建数据表
data_5_4 <- data.frame(x = x_all_new, y = y_20)

# 绘制折线图
plot_5_4 <- ggplot(data_5_4, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "coverage  (n=100)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_5_4

#构建数据表
data_6_1 <- data.frame(x = x_all_new, y = y_21)

# 绘制折线图
plot_6_1 <- ggplot(data_6_1, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "width  (n=10)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_6_1

#构建数据表
data_6_2 <- data.frame(x = x_all_new, y = y_22)

# 绘制折线图
plot_6_2 <- ggplot(data_6_2, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "width  (n=20)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_6_2

#构建数据表
data_6_3 <- data.frame(x = x_all_new, y = y_23)

# 绘制折线图
plot_6_3 <- ggplot(data_6_3, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "width  (n=50)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_6_3

#构建数据表
data_6_4 <- data.frame(x = x_all_new, y = y_24)

# 绘制折线图
plot_6_4 <- ggplot(data_6_4, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "width  (n=100)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_6_4

#构建数据表
data_7_1 <- data.frame(x = x_all_new, y = y_25)

# 绘制折线图
plot_7_1 <- ggplot(data_7_1, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss left(%)  (n=10)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_7_1

#构建数据表
data_7_2 <- data.frame(x = x_all_new, y = y_26)

# 绘制折线图
plot_7_2 <- ggplot(data_7_2, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss left(%)  (n=20)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_7_2

#构建数据表
data_7_3 <- data.frame(x = x_all_new, y = y_27)

# 绘制折线图
plot_7_3 <- ggplot(data_7_3, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss left(%)  (n=50)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(), # 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_7_3

#构建数据表
data_7_4 <- data.frame(x = x_all_new, y = y_28)

# 绘制折线图
plot_7_4 <- ggplot(data_7_4, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss left(%)  (n=100)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_7_4

#构建数据表
data_8_1 <- data.frame(x = x_all_new, y = y_29)

# 绘制折线图
plot_8_1 <- ggplot(data_8_1, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss right(%)  (n=10)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_8_1

#构建数据表
data_8_2 <- data.frame(x = x_all_new, y = y_30)

# 绘制折线图
plot_8_2 <- ggplot(data_8_2, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss right(%)  (n=20)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_8_2

#构建数据表
data_8_3 <- data.frame(x = x_all_new, y = y_31)

# 绘制折线图
plot_8_3 <- ggplot(data_8_3, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss right(%)  (n=50)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2))  

plot_8_3

#构建数据表
data_8_4 <- data.frame(x = x_all_new, y = y_32)

# 绘制折线图
plot_8_4 <- ggplot(data_8_4, aes(x = x)) +
  geom_line(aes(y = y.student.t, color = "student_t"), size = 1) +
  geom_line(aes(y = y.bootstrap.t, color = "bootstrap_t"), size = 1) +
  geom_line(aes(y = y.Percentile, color = "Percentile"), size = 1) +
  geom_line(aes(y = y.BCa, color = "BCa"), size = 1) +
  geom_line(aes(y = y.ABC, color = "ABC"), size = 1) +
  geom_point(aes(y = y.student.t), color = "blue", size = 1) +
  geom_point(aes(y = y.bootstrap.t), color = "red", size = 1) +
  geom_point(aes(y = y.Percentile), color = "green", size = 1) +
  geom_point(aes(y = y.BCa), color = "purple", size = 1) +
  geom_point(aes(y = y.ABC), color = "orange", size = 1) +
  labs(x = "degree of freedom", y = "miss right(%)  (n=100)") +
  scale_color_manual(values = c("student_t" = "blue", "bootstrap_t" = "red", "Percentile" = "green",
                                "BCa" = "purple", "ABC" = "orange"),
                     labels = c( "ABC","BCa", "bootstrap_t","Percentile","student_t")) +
  scale_x_continuous(breaks = c(4, 8, 12, 16, 20)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank(),# 将图例的标题设置为空
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow = 2)) 

plot_8_4

library(gridExtra)

#这行代码请单独运行，不然容易报错
plots <- list(plot_5_1, plot_5_2, plot_5_3, plot_5_4,
              plot_6_1, plot_6_2, plot_6_3, plot_6_4,
              plot_7_1, plot_7_2, plot_7_3, plot_7_4,
              plot_8_1, plot_8_2, plot_8_3, plot_8_4)

plot_all_t <- grid.arrange(grobs = plots, nrow = 4, ncol = 4)

#蚯蚓数据实操
earthworms = read.csv("C:\\code\\earthworms.csv",  head = T, fileEncoding = "UTF-8")
head(earthworms)

## 直方图 ##
ew <- rep(earthworms$number.of.somites, earthworms$frequency)
n <- length(ew)
mu <- mean(ew)
std <- sd(ew)
df_ew <- data.frame(number = 1:n, nos = ew)
p = ggplot(data = df_ew, mapping = aes(x = ew)) +
  geom_histogram(fill="red", color="black", alpha=0.3) +
  labs(x="Number of Somites", y="") +
  theme(text = element_text(size = 30))   
p

#install.packages("boot")
library(boot)  # 载入boot包

#计算均值的置信区间
#定义一个函数，可计算函数的均值与方差的平均值
mean_for_boot <- function(dat, ind){
  c(mean(dat[ind]), var(dat[ind])/length(dat))
} 

#boot函数
ew.boot <- boot(ew, mean_for_boot, R = 1000)

#置信区间计算
boot_ci <- boot.ci(ew.boot)

#默认给出了95%的置信区间
abc_ci <- abc.ci(ew, function(x, w) sum(x*w))
abc_ci

#将这些置信区间放在一起看
boot_ci_matrix <- rbind(boot_ci$student[,4:5],boot_ci$percent[,4:5], boot_ci$bca[,4:5], abc_ci[2:3])
rownames(boot_ci_matrix) <- c( "bootstrap-t", "percentile", "BCa", "Approximate BCa")

boot_ci_matrix

#计算蚯蚓数据偏度的95%置信区间

#install.packages("boot")

library(boot)
library(moments)

skewness(ew)

kurtosis(ew) - 3

skewness_2 <- function(dat, ind){
  skewness(dat[ind])
}

skewness_new <- function(dat, ind) {
  c(skewness_2(dat[ind]), var(boot(dat[ind], skewness_2, R=50)$t))
}

set.seed(123) # 设置随机数种子，使结果可重复
boot_results <- boot(ew, skewness_new, R = 1000) # R表示bootstrap的次数
boot_results

boot_ci_new <- boot.ci(boot_results, conf = 0.95)
boot_ci_new

abc_ci <- abc.ci(ew, function(x, w) {
  weighted_mean <- sum(x * w)
  weighted_variance <- sum((x - weighted_mean)^2 * w)
  skew <- sum((x - weighted_mean)^3 * w) / (sqrt(weighted_variance)^3)
  return(skew)
})

abc_ci

#计算蚯蚓数据峰度的95%置信区间
kurtosis_2 <- function(dat, ind){
  kurtosis(dat[ind])
}

kurtosis_new <- function(dat, ind) {
  c(kurtosis_2(dat[ind]), var(boot(dat[ind], kurtosis_2, R=50)$t))
}

set.seed(123) # 设置随机数种子，使结果可重复
boot_results_other <- boot(ew, kurtosis_new, R = 1000) # R表示bootstrap的次数
boot_results_other

boot_ci_new_other <- boot.ci(boot_results_other, conf = 0.95)
boot_ci_new_other
#kurtosis函数没有减去3，以上产生的置信区间上下限需要手动减去3，下面的那个ABC区间不用

abc_ci_other <- abc.ci(ew,function(x, w) {
  weighted_mean <- sum(x * w)
  weighted_variance <- sum((x - weighted_mean)^2 * w)
  kurt <- sum((x - weighted_mean)^4 * w) / (sqrt(weighted_variance)^4) - 3
  return(kurt)
})

abc_ci_other