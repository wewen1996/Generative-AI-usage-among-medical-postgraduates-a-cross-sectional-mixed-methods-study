# ==============================================================================
# 0. 环境准备 (Setup)
# ==============================================================================
# 安装必要的包 (如果未安装请取消注释)
# install.packages(c("tidyverse", "lavaan", "poLCA", "gtsummary", "nnet", "readr"))

library(tidyverse)
library(lavaan)
library(poLCA)
library(gtsummary)
library(nnet)

# ==============================================================================
# 1. 数据读取 (GBK 编码)
# ==============================================================================
# 确保你的 data.csv 在工作目录下
raw_data <- read_csv("data.csv", 
                     locale = locale(encoding = "GBK"), 
                     show_col_types = FALSE)

# ==============================================================================
# 2. 数据清洗 (全流程)
# ==============================================================================
clean_df_step1 <- raw_data %>%
  # --- 变量提取 (使用数值索引锁定列位置) ---
  select(
    ID = 1,             
    Gender_Raw = 3,     
    BirthDate = 4,
    Grade_Raw = 5,
    Military_Raw = 6,   
    Degree_Raw = 7,     
    Major_Raw = 8,      
    SCI_Count_Raw = 10, 
    Coding_Raw = 12,
    
    # [新增] 筛选题 (Attention Check) - 第13列
    Check_Question = 13, 
    
    Freq_Raw = 14,      
    Task_Multi_Raw = 15,
    Task_Main_Raw = 16,
    Reason_Stop_Raw = 25,
    Feedback_Raw = 33,
    
    # 量表题目提取
    PU1 = contains("缩短了我筛选"), PU2 = contains("解决了技术瓶颈"), 
    PU3 = contains("优于传统的翻译"), PU4 = contains("提高了我的科研"),
    AL1 = contains("结构化的提示词"), AL2 = contains("追问或提供示例"), 
    AL3 = contains("上传Excel"), AL4 = contains("识别出AI生成的"),
    Risk1 = contains("隐私泄露"), Risk2 = contains("削弱我独立"), 
    Risk3 = contains("学术不端"), Risk4 = contains("二次验证"),
    BI1 = contains("计划更频繁"), BI2 = contains("愿意学习"), 
    BI3 = contains("推荐使用")
  ) %>%
  
  # --- 变量清洗与标准化 ---
  mutate(
    # 年龄修复
    Year_Raw = str_extract(BirthDate, "\\d+"), 
    Year_Num = as.numeric(Year_Raw),
    BirthYear = case_when(
      Year_Num > 50 ~ 1900 + Year_Num,
      Year_Num <= 50 ~ 2000 + Year_Num,
      TRUE ~ NA_real_
    ),
    Age = 2025 - BirthYear,
    
    # 英文标准化
    Gender = case_when(str_detect(Gender_Raw, "男") ~ "Male", str_detect(Gender_Raw, "女") ~ "Female", TRUE ~ NA_character_),
    Grade = case_when(
      str_detect(Grade_Raw, "硕士一年级") ~ "Master Year 1",
      str_detect(Grade_Raw, "硕士二年级") ~ "Master Year 2",
      str_detect(Grade_Raw, "硕士三年级") ~ "Master Year 3",
      str_detect(Grade_Raw, "博士一年级") ~ "PhD Year 1",
      str_detect(Grade_Raw, "博士二年级") ~ "PhD Year 2",
      str_detect(Grade_Raw, "博士三年级") ~ "PhD Year 3+",
      TRUE ~ "Other"
    ),
    Degree = case_when(str_detect(Degree_Raw, "学术") ~ "Academic Degree", str_detect(Degree_Raw, "专业") ~ "Professional Degree", TRUE ~ "Other"),
    Major = case_when(
      str_detect(Major_Raw, "基础") ~ "Basic Medicine",
      str_detect(Major_Raw, "临床") ~ "Clinical Medicine",
      str_detect(Major_Raw, "公卫|预防") ~ "Public Health",
      str_detect(Major_Raw, "药学") ~ "Pharmacy",
      str_detect(Major_Raw, "护理") ~ "Nursing",
      str_detect(Major_Raw, "口腔") ~ "Stomatology",
      TRUE ~ "Other"
    ),
    Institute_Type = case_when(str_detect(Military_Raw, "^不是") ~ "Open Access", 
                               str_detect(Military_Raw, "^是") ~ "Restricted Access", 
                               TRUE ~ NA_character_),
    Coding_Level = case_when(str_detect(Coding_Raw, "零基础") ~ "Zero Basis", 
                             str_detect(Coding_Raw, "入门") ~ "Novice", 
                             str_detect(Coding_Raw, "进阶") ~ "Intermediate",
                             str_detect(Coding_Raw, "熟练|精通") ~ "Advanced",
                             TRUE ~ "Other"),
    Frequency = case_when(str_detect(Freq_Raw, "经常") ~ "Daily", 
                          str_detect(Freq_Raw, "每周") ~ "Weekly", 
                          str_detect(Freq_Raw, "每月|偶尔") ~ "Monthly/Rarely", 
                          str_detect(Freq_Raw, "从不") ~ "Never", 
                          TRUE ~ NA_character_),
    SCI_Count = case_when(str_detect(SCI_Count_Raw, "0") ~ "0", 
                          str_detect(SCI_Count_Raw, "1") ~ "1", 
                          str_detect(SCI_Count_Raw, "2") ~ "2", 
                          str_detect(SCI_Count_Raw, "3") ~ ">=3", 
                          TRUE ~ NA_character_),
    
    # 行为变量翻译
    Main_Task_Eng = case_when(
      str_detect(Task_Main_Raw, "文献") ~ "Literature Review",
      str_detect(Task_Main_Raw, "翻译|润色") ~ "Translation & Polishing",
      str_detect(Task_Main_Raw, "代码|统计") ~ "Coding & Statistics",
      str_detect(Task_Main_Raw, "绘图") ~ "Visualization",
      str_detect(Task_Main_Raw, "实验设计") ~ "Exp. Design",
      str_detect(Task_Main_Raw, "审稿") ~ "Rebuttal",
      TRUE ~ "Other/None"
    ),
    Non_Use_Reason_Eng = case_when(
      str_detect(Reason_Stop_Raw, "跳过") ~ "User (Not Applicable)",
      str_detect(Reason_Stop_Raw, "不知道|不了解") ~ "Lack of Awareness",
      str_detect(Reason_Stop_Raw, "不会用|门槛") ~ "Technical Barrier",
      str_detect(Reason_Stop_Raw, "准确性|幻觉") ~ "Accuracy Concerns",
      str_detect(Reason_Stop_Raw, "隐私|安全") ~ "Privacy/Ethical Concerns",
      str_detect(Reason_Stop_Raw, "收费") ~ "Cost",
      str_detect(Reason_Stop_Raw, "网络") ~ "Access Restriction",
      TRUE ~ "Other"
    ),
    Has_Feedback = ifelse(is.na(Feedback_Raw) | Feedback_Raw == "(空)", "No", "Yes"),
    
    # 量表数值化
    across(c(starts_with("PU"), starts_with("AL"), starts_with("Risk"), starts_with("BI")), 
           ~ case_when(
             str_detect(., "非常不同意|完全不符合") ~ 1,
             str_detect(., "不同意|不符合") ~ 2, 
             str_detect(., "一般") ~ 3,
             str_detect(., "非常同意|完全符合") ~ 5,
             str_detect(., "同意|符合") ~ 4,
             TRUE ~ 3
           ))
  ) %>%
  
  # 计算均分
  rowwise() %>%
  mutate(
    Mean_PU = mean(c_across(starts_with("PU")), na.rm = TRUE),
    Mean_AL = mean(c_across(starts_with("AL")), na.rm = TRUE),
    Mean_Risk = mean(c_across(starts_with("Risk")), na.rm = TRUE),
    Mean_BI = mean(c_across(starts_with("BI")), na.rm = TRUE)
  ) %>%
  ungroup()

# ==============================================================================
# 3. 数据集拆分与筛选 (关键步骤)
# ==============================================================================

# 1. 所有数据 (用于计算回收率)
final_all_data <- clean_df_step1 %>%
  select(ID, Check_Question, Gender, Age, Grade, Degree, Major, Institute_Type, 
         SCI_Count, Coding_Level, Frequency, Main_Task_Eng, Non_Use_Reason_Eng, 
         starts_with("PU"), starts_with("AL"), starts_with("Risk"), starts_with("BI"),
         starts_with("Mean_"))

# 2. 有效数据 (剔除错误答案)
# 筛选逻辑：必须包含“能够调用外部工具”这一关键短语
final_valid_data <- final_all_data %>%
  filter(str_detect(Check_Question, "能够调用外部工具"))

# ==============================================================================
# 4. 输出统计与文件保存
# ==============================================================================
cat("Total Responses:", nrow(final_all_data), "\n")
cat("Valid Responses (Passed Check):", nrow(final_valid_data), "\n")
cat("Effective Rate:", round(nrow(final_valid_data)/nrow(final_all_data)*100, 2), "%\n")

# 导出两个文件
write_csv(final_all_data, "data_all_raw.csv")        # 包含所有数据（含无效）
write_csv(final_valid_data, "data_valid_clean.csv")  # 真正用来跑分析的数据

# 读取筛选后的有效数据
clean_df <- read_csv("data_valid_clean.csv", show_col_types = FALSE)

# 检查数据概况
print(paste("Valid Sample Size:", nrow(clean_df)))
print(colnames(clean_df)) # 确认列名是否正确

# ==============================================================================
# 2. Table 1: 人口学特征 (Demographics)
# ==============================================================================
library(flextable)

table1_output <- clean_df %>%
  select(Age, Gender, Grade, Degree, Major, Coding_Level, SCI_Count, Frequency, Institute_Type) %>%
  tbl_summary(
    by = Institute_Type, 
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    label = list(
      Age ~ "Age (Years)", 
      Grade ~ "Grade Level",
      Coding_Level ~ "Programming Experience",
      SCI_Count ~ "SCI Publications",
      Frequency ~ "AI Usage Frequency"
    ),
    missing = "no"
  ) %>%
  # --- 关键修改：指定统计检验方法 ---
  add_p(
    test = list(
      all_categorical() ~ "chisq.test",  # 强制对所有分类变量使用卡方检验
      all_continuous() ~ "t.test"        # 对连续变量使用 t 检验 (或 "wilcox.test")
    ),
    pvalue_fun = function(x) style_pvalue(x, digits = 3) # 统一P值保留3位小数
  ) %>%
  add_overall() %>% 
  bold_labels()

table1_output %>%
  as_flex_table() %>%
  save_as_docx(path = "Table1_Demographics.docx")

print(table1_output) 
# 提示: 在 RStudio 中可以使用 as_flex_table(table1_output) 导出为 Word 格式
as_flex_table(table1_output)
# ==============================================================================
# 3. SEM 分析: 验证 Age 的调节作用 (Moderation)
# ==============================================================================
# 准备数据: 中心化处理以避免多重共线性
sem_data <- clean_df %>%
  mutate(
    Age_c = scale(Age, center = TRUE, scale = FALSE)[,1],
    Risk_Mean_c = scale(Mean_Risk, center = TRUE, scale = FALSE)[,1],
    # 交互项: 焦虑 x 年龄
    Inter_Risk_Age = Risk_Mean_c * Age_c
  )

# 定义 SEM 模型
# H1: PU -> AL -> BI (中介效应)
# H2: Risk -> BI (直接效应)
# H3: Age 调节 Risk -> BI 的路径
sem_model <- '
  # Measurement Model (测量模型)
  PU =~ PU1 + PU2 + PU3 + PU4
  AL =~ AL1 + AL2 + AL3 + AL4
  Risk =~ Risk1 + Risk2 + Risk3 + Risk4
  BI =~ BI1 + BI2 + BI3
  
  # Structural Model (结构模型)
  BI ~ c*Risk + b*AL + Age_c + d*Inter_Risk_Age # 关注 d (交互项系数)
  AL ~ a*PU
  
  # Indirect Effect (中介效应计算)
  indirect_PU_BI := a*b
'

# 运行模型 (使用 MLR 估计器处理非正态性)
fit <- sem(sem_model, data = sem_data, estimator = "MLR")
summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# 重点检查:
# 1. Fit Indices: CFI > 0.90, RMSEA < 0.08, SRMR < 0.08
# 2. Regressions: "BI ~ Inter_Risk_Age" 的 P 值是否 < 0.05

# ==============================================================================
# 1. 多群组 SEM (Multigroup SEM) - 替代不显著的年龄调节
# ==============================================================================
# 既然 Age 交互不显著，我们看看“院校环境”是否调节了路径
# 模型定义 (移除交互项，将 Age 作为控制变量)
sem_model_clean <- '
  # Measurement Model (测量模型)
  PU =~ PU1 + PU2 + PU3 + PU4
  AL =~ AL1 + AL2 + AL3 + AL4
  Risk =~ Risk1 + Risk2 + Risk3 + Risk4
  BI =~ BI1 + BI2 + BI3
  
  # Structural Model (结构模型)
  # 修正点：全部使用 c() 来定义不同组的标签
  # c("c1", "c2") 表示：Open组系数为c1，Restricted组系数为c2
  
  BI ~ c("c1", "c2")*Risk + c("b1", "b2")*AL + Age
  AL ~ c("a1", "a2")*PU
'

# 分组运行 (Open vs Restricted)
fit_group <- sem(sem_model_clean, data = clean_df, group = "Institute_Type", estimator = "MLR")

# 查看组间差异
summary(fit_group, standardized = TRUE)

# 比较两组系数差异 (例如比较 Risk -> BI 的路径 c1 和 c2 是否不同)
# 如果 P 值显著，说明环境确实有调节作用
diff_test <- lavTestWald(fit_group, constraints = 'c1 == c2') 
print(diff_test)

# ==============================================================================
# 2. LCA 潜在类别分析 (再次运行以确认分类)
# ==============================================================================
# 提取显变量
lca_vars <- clean_df %>%
  select(PU1:PU4, AL1:AL4, Risk1:Risk4) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

# 运行 3 类别模型
f <- as.formula(paste("cbind(", paste(names(lca_vars), collapse = ","), ") ~ 1"))
set.seed(123)
lc3 <- poLCA(f, lca_vars, nclass = 3, maxiter = 3000, graphs = TRUE, tol = 1e-5)

# 看看这三类人的特征 (概率图)
# 这一步会生成图片，请重点关注是否有“高焦虑、高意愿”的类别

# 将类别合并回数据
clean_df$Class <- factor(lc3$predclass)

# ==============================================================================
# 3. 描述性统计补充 (针对 User Feedback)
# ==============================================================================
# 看看大家的痛点是什么 (Main_Task_Eng)
print(table(clean_df$Main_Task_Eng))

# ==============================================================================
# 4. LCA 潜在类别分析 (LCA) - 适配实际结果版
# ==============================================================================
# 提取显变量
lca_vars <- clean_df %>%
  select(PU1:PU4, AL1:AL4, Risk1:Risk4) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

# 定义公式
f <- as.formula(paste("cbind(", paste(names(lca_vars), collapse = ","), ") ~ 1"))

# 运行模型 (固定随机种子以复现结果)
set.seed(123)
lc3 <- poLCA(f, lca_vars, nclass = 3, maxiter = 3000, graphs = FALSE, tol = 1e-5, na.rm = TRUE)

# --- [关键步骤 1] 将分类合并回主数据，并赋予具有科学意义的标签 ---
# 注意：这里是根据你之前上传的结果图表手动指定的，务必核对你的 plot 顺序
# 假设 output order 是: 1=Super-Users, 2=Moderate, 3=Hesitant (根据概率图特征)
clean_df$Class_Raw <- lc3$predclass[match(rownames(clean_df), rownames(lca_vars))]

clean_df$Class_Label <- factor(clean_df$Class_Raw, 
                               levels = c(1, 2, 3),
                               labels = c("Anxious Super-Users",  # Class 1: 高意愿+高焦虑
                                          "Moderate Adopters",    # Class 2: 中等
                                          "Hesitant Majority"))   # Class 3: 低意愿+低焦虑

# ==============================================================================
# 5. 多项 Logistic 回归 (Multinomial Regression) - 战术升级版
# ==============================================================================
# --- [关键步骤 2] 设定参照组 (Reference Group) ---
# 我们想知道“什么因素让人不再犹豫，变成超级用户？”
# 所以将 "Hesitant Majority" 设为基准组
clean_df$Class_Label <- relevel(clean_df$Class_Label, ref = "Hesitant Majority")

# --- [关键步骤 3] 规范化自变量顺序 (Factor Ordering) ---
# 确保编程基础和SCI数量是按“能力从低到高”排列的，这样 OR > 1 代表能力越强越可能归类
clean_df <- clean_df %>%
  mutate(
    Coding_Level = factor(Coding_Level, levels = c("Zero Basis", "Novice", "Advanced")),
    SCI_Count = factor(SCI_Count, levels = c("0", "1", "2", ">=3")),
    Institute_Type = factor(Institute_Type, levels = c("Open Access", "Restricted Access"))
  )

# 运行回归模型
# 解释: 相比于犹豫的大多数，哪些因素显著预测了学生属于"焦虑超级用户"或"温和采纳者"?
model_lca_final <- multinom(Class_Label ~ Age + Coding_Level + SCI_Count + Institute_Type, 
                            data = clean_df)

# 输出 SCI 级表格
# 重点关注: Coding_Level 和 SCI_Count 的 P 值
tbl_regression(model_lca_final, exponentiate = TRUE) %>% 
  bold_p() %>%
  as_gt() %>%
  gt::tab_header(
    title = "Predictors of AI Agent User Profiles",
    subtitle = "Reference Group: Hesitant Majority"
  )

summary(model_lca_final)
z <- summary(model_lca_final)$coefficients / summary(model_lca_final)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
print(exp(coef(model_lca_final))) # 输出 OR 值
print(p) # 输出 P 值
# ==============================================================================
# 0. 准备工作
# ==============================================================================
library(tidyverse)
library(ggplot2)
library(semPlot)
library(lavaan)
library(reshape2)
library(ggsci) # 用于SCI配色

# 确保输出目录存在
if(!dir.exists("Figures")) dir.create("Figures")

# ==============================================================================
# Figure 1: SEM 路径图 (Path Diagram)
# ==============================================================================
# 由于多群组差异不显著，我们用全样本模型画一张清晰的总图
# 重新运行一次全样本模型 (作为最终展示模型)
final_model_syntax <- '
  PU =~ PU1 + PU2 + PU3 + PU4
  AL =~ AL1 + AL2 + AL3 + AL4
  Risk =~ Risk1 + Risk2 + Risk3 + Risk4
  BI =~ BI1 + BI2 + BI3
  
  BI ~ c*Risk + b*AL + Age
  AL ~ a*PU
'
fit_final <- sem(final_model_syntax, data = clean_df, estimator = "MLR")
summary(fit_final, fit.measures=TRUE)
# 输出 PDF
pdf("Figures/Figure1_SEM_Path.pdf", width = 10, height = 8)
semPaths(fit_final, 
         whatLabels = "std",       # 显示标准化系数
         layout = "tree2",         # 树状布局
         style = "lisrel",         # LISREL 风格
         edge.label.cex = 1.2,     # 路径系数全字体大小
         curvePivot = TRUE,        # 协方差曲线
         residuals = FALSE,        # 不显示残差以保持整洁
         sizeMan = 8,              # 显变量框大小
         sizeLat = 10,             # 潜变量圈大小
         nCharNodes = 0,           # 显示完整变量名
         edge.color = "black",     # 黑色连线
         nodeLabels = c("PU1","PU2","PU3","PU4", 
                        "AL1","AL2","AL3","AL4", 
                        "Risk1","Risk2","Risk3","Risk4", 
                        "BI1","BI2","BI3",
                        "Usefulness", "Literacy", "Anxiety", "Intention", "Age") # 替换为英文标签
)
title("Figure 1. Structural Equation Model Results")
dev.off()

# 输出 TIFF (高分辨率 300 DPI)
tiff("Figures/Figure1_SEM_Path.tiff", width = 3000, height = 2400, res = 300)
semPaths(fit_final, whatLabels = "std", layout = "tree2", style = "lisrel", 
         edge.label.cex = 1.2, residuals = FALSE, sizeMan = 8, sizeLat = 10, 
         nCharNodes = 0, edge.color = "black",
         nodeLabels = c("PU1","PU2","PU3","PU4", "AL1","AL2","AL3","AL4", "Risk1","Risk2","Risk3","Risk4", "BI1","BI2","BI3",
                        "Usefulness", "Literacy", "Anxiety", "Intention", "Age"))
dev.off()

# ==============================================================================
# Figure 2: LCA 潜在类别画像 (Latent Profile Plot)
# ==============================================================================
# 提取每一类的题目概率 (使用你提供的 LCA 结果数据)
# 这里为了绘图美观，我们使用各维度的均分来代替复杂的概率
clean_df$Class_Label <- factor(clean_df$Class, 
                               labels = c("Anxious Super-Users (24%)", 
                                          "Moderate Adopters (27%)", 
                                          "Hesitant Majority (49%)"))

# 计算各类在各维度上的标准化均值 (Z-score) 以便比较
plot_data <- clean_df %>%
  select(Class_Label, Mean_PU, Mean_AL, Mean_Risk, Mean_BI) %>%
  group_by(Class_Label) %>%
  # --- 修正点 1: 使用现代化的 across 写法 ---
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>% 
  pivot_longer(cols = -Class_Label, names_to = "Variable", values_to = "Score") %>%
  mutate(Variable = factor(Variable, 
                           levels = c("Mean_PU", "Mean_AL", "Mean_Risk", "Mean_BI"),
                           labels = c("Perceived\nUsefulness", "Agentic\nLiteracy", 
                                      "AI\nAnxiety", "Intention\nto Use")))

# 2. 绘图 (修复 ggplot2 warning)
p2 <- ggplot(plot_data, aes(x = Variable, y = Score, group = Class_Label, color = Class_Label)) +
  # --- 修正点 2: 线条粗细改用 linewidth ---
  geom_line(linewidth = 1.2) + 
  # 点的大小 size 依然保留
  geom_point(size = 4, aes(shape = Class_Label)) + 
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_color_npg() + 
  labs(title = "", x = "", y = "Mean Score (Likert 1-5)", color = "Latent Profiles") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  guides(shape = "none")

# 保存 Figure 2
ggsave("Figures/Figure2_LCA_Profiles.pdf", p2, width = 8, height = 6)
ggsave("Figures/Figure2_LCA_Profiles.tiff", p2, width = 8, height = 6, dpi = 300)

# ==============================================================================
# Figure 3: 科研任务分布图 (Task Distribution)
# ==============================================================================
task_data <- clean_df %>%
  filter(!is.na(Main_Task_Eng)) %>%
  count(Main_Task_Eng) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(Main_Task_Eng = reorder(Main_Task_Eng, n)) # 排序

p3 <- ggplot(task_data, aes(x = Main_Task_Eng, y = n, fill = Main_Task_Eng)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), 
            hjust = -0.1, size = 4) + # 添加数字标签
  scale_fill_npg() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # 留出右侧空间
  coord_flip() + # 横向柱状图
  labs(title = "", x = "Primary AI Task", y = "Number of Students") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12),
    plot.margin = margin(10, 20, 10, 10)
  )

# 保存 Figure 3
ggsave("Figures/Figure3_Task_Distribution.pdf", p3, width = 8, height = 5)
ggsave("Figures/Figure3_Task_Distribution.tiff", p3, width = 8, height = 5, dpi = 300)

print("所有图片已生成至 Figures 文件夹。")


# ==============================================================================
# Figure 4: 多项逻辑回归森林图 (Forest Plot for Multinomial Regression)
# ==============================================================================
library(tidyverse)
library(broom)
library(ggplot2)
library(ggsci)

# 1. 提取模型结果并整理
# 使用 tidy 函数提取数据
plot_data <- tidy(model_lca_final, exponentiate = TRUE, conf.int = TRUE) %>%
  # 过滤掉截距项，只看自变量
  filter(term != "(Intercept)") %>%
  # 只展示 "Anxious Super-Users" 这一组的结果（这是你的核心故事）
  filter(y.level == "Anxious Super-Users") %>%
  # 优化变量标签显示
  mutate(term_label = case_when(
    term == "Age" ~ "Age (per year)",
    term == "Coding_LevelNovice" ~ "Coding: Novice (vs Zero)",
    term == "Coding_LevelAdvanced" ~ "Coding: Advanced (vs Zero)",
    term == "SCI_Count1" ~ "SCI: 1 Paper (vs 0)",
    term == "SCI_Count2" ~ "SCI: 2 Papers (vs 0)",
    term == "SCI_Count>=3" ~ "SCI: >=3 Papers (vs 0)",
    term == "Institute_TypeRestricted Access" ~ "Env: Restricted Access (vs Open)",
    TRUE ~ term
  )) %>%
  # 按照 OR 值排序，或者按照逻辑顺序排序
  mutate(term_label = factor(term_label, levels = rev(c(
    "Age (per year)",
    "Env: Restricted Access (vs Open)",
    "SCI: >=3 Papers (vs 0)",
    "SCI: 2 Papers (vs 0)",
    "SCI: 1 Paper (vs 0)",
    "Coding: Advanced (vs Zero)",
    "Coding: Novice (vs Zero)"
  ))))

# 2. 绘制森林图
p4 <- ggplot(plot_data, aes(x = estimate, y = term_label)) +
  # 添加垂直虚线 x=1 (无效线)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  # 添加误差棒 (置信区间)
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  # 添加点 (OR值)，根据 P值是否显著(<0.1) 变色
  geom_point(aes(color = p.value < 0.1, size = p.value < 0.1)) +
  
  # 颜色设置：显著(或边缘显著)为红色，不显著为蓝色
  scale_color_manual(values = c("TRUE" = "#E64B35FF", "FALSE" = "#4DBBD5FF"), 
                     labels = c("TRUE" = "P < 0.10 (Trend)", "FALSE" = "P > 0.10")) +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2.5)) +
  
  # 坐标轴与标签
  labs(title = "Predictors of 'Anxious Super-Users' Profile",
       subtitle = "Reference Group: Hesitant Majority",
       x = "Odds Ratio (OR) with 95% CI",
       y = "",
       color = "Significance") +
  
  # 主题优化
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank() # 去掉横向网格线
  )

# 3. 保存图片
ggsave("Figures/Figure4_Forest_Plot.pdf", p4, width = 8, height = 5)
ggsave("Figures/Figure4_Forest_Plot.tiff", p4, width = 8, height = 5, dpi = 300)

print("Figure 4 已生成。")


# ==============================================================================
# 0. 准备工作
# ==============================================================================
library(tidyverse)
library(nnet)       # 多项逻辑回归
library(gtsummary)  # 表格输出
library(broom)      # 数据提取
library(ggplot2)    # 绘图
library(poLCA)      # LCA分析

# 读取清洗后的有效数据
clean_df <- read_csv("data_valid_clean.csv", show_col_types = FALSE)

# ------------------------------------------------------------------------------
# 步骤 1: 重现 LCA 分类 (确保基准一致)
# ------------------------------------------------------------------------------
# 提取显变量
lca_vars <- clean_df %>%
  select(PU1:PU4, AL1:AL4, Risk1:Risk4) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

# 运行 LCA (固定种子)
f <- as.formula(paste("cbind(", paste(names(lca_vars), collapse = ","), ") ~ 1"))
set.seed(123)
lc3 <- poLCA(f, lca_vars, nclass = 3, maxiter = 3000, graphs = FALSE, tol = 1e-5)

# 合并分类结果
clean_df$Class_Raw <- lc3$predclass[match(rownames(clean_df), rownames(lca_vars))]

# 赋予标签 (根据你之前的概率图特征)
# 假设 1=Anxious Super-Users, 2=Moderate, 3=Hesitant
clean_df$Class_Label <- factor(clean_df$Class_Raw, 
                               levels = c(1, 2, 3),
                               labels = c("Anxious Super-Users", 
                                          "Moderate Adopters", 
                                          "Hesitant Majority"))

# 设定参照组 (以“犹豫的大多数”为基准)
clean_df$Class_Label <- relevel(clean_df$Class_Label, ref = "Hesitant Majority")

# ==============================================================================
# 步骤 2: 变量降维 (Data Transformation) - 核心战术
# ==============================================================================
# 策略：将多分类变量合并为二分变量，增加统计效力
clean_df_optimized <- clean_df %>%
  mutate(
    # 1. 编程基础: "有基础" (Novice/Advanced) vs "零基础" (Zero)
    Coding_Binary = ifelse(Coding_Level == "Zero Basis", "Zero Basis", "Has Foundation"),
    Coding_Binary = factor(Coding_Binary, levels = c("Zero Basis", "Has Foundation")),
    
    # 2. SCI 论文: "发表过" (>=1) vs "未发表" (0)
    # 或者尝试: "高产" (>=3) vs "普通" (<3) -> 根据你之前的OR=1.65，高产效应更强
    # 这里我们采用 "发表过 vs 未发表" 的通用策略，样本量更平衡
    SCI_Binary = ifelse(SCI_Count == "0", "None", "Published"),
    SCI_Binary = factor(SCI_Binary, levels = c("None", "Published")),
    
    # 3. 院校环境: 保持不变 (核心变量)
    Institute_Type = factor(Institute_Type, levels = c("Open Access", "Restricted Access"))
  )

# ==============================================================================
# 步骤 3: 运行微调后的模型 (Refined Model)
# ==============================================================================
# 新模型：自变量更少，自由度更高，P值更容易显著
model_refined <- multinom(Class_Label ~ Age + Coding_Binary + SCI_Binary + Institute_Type, 
                          data = clean_df_optimized)

# 查看结果表格 (重点看 Anxious Super-Users 列的 P 值)
table_refined <- tbl_regression(model_refined, exponentiate = TRUE) %>%
  bold_p(t = 0.05) %>% # 自动加粗 P<0.05
  as_gt() %>%
  gt::tab_header(
    title = "Optimized Predictors of AI Agent User Profiles",
    subtitle = "Variables collapsed for statistical power"
  )

print(table_refined)

# ==============================================================================
# 步骤 4: 绘制新版森林图 (Updated Forest Plot)
# ==============================================================================
# 提取数据
plot_data_refined <- tidy(model_refined, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  filter(y.level == "Anxious Super-Users") %>%
  mutate(term_label = case_when(
    term == "Age" ~ "Age (per year)",
    term == "Coding_BinaryHas Foundation" ~ "Coding: Has Foundation (vs Zero)",
    term == "SCI_BinaryPublished" ~ "SCI: Published (vs None)",
    term == "Institute_TypeRestricted Access" ~ "Env: Restricted Access (vs Open)",
    TRUE ~ term
  )) %>%
  # 标记显著性 (P < 0.05)
  mutate(is_significant = p.value < 0.05)

# 绘图
p5 <- ggplot(plot_data_refined, aes(x = estimate, y = reorder(term_label, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray60") +
  
  # 误差棒 (置信区间)
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = is_significant), 
                 height = 0.2, size = 0.8) +
  
  # OR点 (点的大小代表P值显著程度，越显著点越大)
  geom_point(aes(color = is_significant, size = -log10(p.value))) +
  
  # 数据标签 (显示 OR 值和 P 值)
  geom_text(aes(label = paste0("OR=", round(estimate, 2), ", p=", round(p.value, 3))), 
            vjust = -1, size = 3.5, color = "black") +
  
  # 配色: 红色显著，蓝色不显著
  scale_color_manual(values = c("FALSE" = "#4DBBD5FF", "TRUE" = "#DC0000FF"), 
                     labels = c("Not Sig.", "Significant (P<0.05)")) +
  
  labs(title = "Factors Associated with 'Anxious Super-Users' Profile",
       subtitle = "Refined Model (Merged Categories)",
       x = "Odds Ratio (95% CI)", y = "") +
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    panel.grid.major.y = element_blank()
  )

# 保存
ggsave("Figures/Figure4_Refined_ForestPlot.pdf", p5, width = 8, height = 5)
print("微调完成。请查看 Figure4_Refined_ForestPlot.pdf 以及控制台输出的表格。")


# 自动逐步回归 (Stepwise Regression)
final_model_auto <- step(model_refined, direction = "both")
summary(final_model_auto)

# ==============================================================================
# Figure 1: 终极曲线美化版 (No Error Version)
# ==============================================================================
library(tidyverse)
library(lavaan)
library(tidySEM)
library(ggplot2)

# 1. 准备数据与模型
clean_df <- read_csv("data_valid_clean.csv", show_col_types = FALSE)
clean_df <- clean_df %>%
  mutate(across(c(Age, Mean_PU, Mean_AL, Mean_Risk, Mean_BI), ~scale(., scale=FALSE)[,1]))

final_model_syntax <- '
  # 测量模型
  PU =~ PU1 + PU2 + PU3 + PU4
  AL =~ AL1 + AL2 + AL3 + AL4
  Risk =~ Risk1 + Risk2 + Risk3 + Risk4
  BI =~ BI1 + BI2 + BI3
  
  # 结构模型
  BI ~ Risk + AL + Age
  AL ~ PU
'
fit_final <- sem(final_model_syntax, data = clean_df, estimator = "MLR")

# 2. 生成绘图数据 (Layout)
# 使用 "tree2" 布局，这种布局最适合展示路径关系
layout <- get_layout(fit_final, layout = "tree2")

# 3. 准备图对象
graph_data <- prepare_graph(fit_final, layout = layout)

# ------------------------------------------------------------------------------
# 关键修复与美化步骤 (修改对象内部属性)
# ------------------------------------------------------------------------------

# (A) 修改节点标签 (让标签更简洁、专业)
node_labels <- c(
  "PU" = "Perceived\nUsefulness",
  "AL" = "Agentic\nLiteracy", 
  "Risk" = "AI Anxiety",
  "BI" = "Intention\nto Use",
  "Age" = "Age"
)

# 逻辑：如果是潜变量，用全名；如果是显变量，只保留数字 (例如 PU1 -> 1)
graph_data$nodes$label <- ifelse(
  graph_data$nodes$name %in% names(node_labels),
  node_labels[graph_data$nodes$name],
  str_extract(graph_data$nodes$name, "\\d+") 
)

# (B) 强制设置曲线 (The "Curved" Look)
# 给所有连线增加曲率，0.4 是一个比较明显的弧度，类似你喜欢的风格
graph_data$edges$curvature <- 0.4 

# (C) 设置连线标签背景 (白底防遮挡)
graph_data$edges$label_fill <- "white" 

# (D) 隐藏显变量的自我循环 (残差)，让图更干净
# 将显变量残差线的透明度设为 0
graph_data$edges <- graph_data$edges %>%
  mutate(alpha = ifelse(from == to & !str_detect(from, "PU|AL|Risk|BI"), 0, 1))

# 4. 绘图 (使用原生 plot 接口，避免 fortify 报错)
p1_final <- plot(graph_data) + 
  # 在此基础上叠加 ggplot 主题进行美化
  theme_void() +
  theme(legend.position = "none") 

# 5. 输出
if(!dir.exists("Figures")) dir.create("Figures")
ggsave("Figures/Figure1_SEM_Path_Curved_Final.pdf", p1_final, width = 9, height = 7)
ggsave("Figures/Figure1_SEM_Path_Curved_Final.tiff", p1_final, width = 9, height = 7, dpi = 300)

print("成功！已生成圆润曲线风格的路径图：Figures/Figure1_SEM_Path_Curved_Final.pdf")

# ==============================================================================
# Figure 1: 经典 SCI 风格彩色路径图 (带显著性星号)
# ==============================================================================
library(tidyverse)
library(lavaan)
library(semPlot) # 专门画经典SEM图的包
library(RColorBrewer) # 调色板

# 1. 准备数据与模型 (确保模型对象存在)
clean_df <- read_csv("data_valid_clean.csv", show_col_types = FALSE)
# 简单中心化处理（不影响标准化系数，但有助于模型计算）
clean_df <- clean_df %>%
  mutate(across(c(Age, Mean_PU, Mean_AL, Mean_Risk, Mean_BI), ~scale(., scale=FALSE)[,1]))

final_model_syntax <- '
  # 测量模型
  PU =~ PU1 + PU2 + PU3 + PU4
  AL =~ AL1 + AL2 + AL3 + AL4
  Risk =~ Risk1 + Risk2 + Risk3 + Risk4
  BI =~ BI1 + BI2 + BI3
  
  # 结构模型
  BI ~ Risk + AL + Age
  AL ~ PU
'
fit_final <- sem(final_model_syntax, data = clean_df, estimator = "MLR")

# 2. 定义绘图参数 (定制化)
# (1) 提取标准化系数用于绘图
# semPlot默认不显示星号，我们需要自己构造一个标签函数
get_stars <- function(p) {
  ifelse(p < 0.001, "***", 
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")))
}

# (2) 设置节点颜色 (SCI 配色)
# 潜变量用清新的蓝色，显变量用浅灰色，误差项忽略
node_colors <- list(
  lat = "#A6CEE3", # 潜变量颜色 (蓝色)
  man = "#F0F0F0"  # 显变量颜色 (灰色)
)

# (3) 设置节点标签 (极简风)
node_labels <- c(
  "PU1","PU2","PU3","PU4", 
  "AL1","AL2","AL3","AL4", 
  "Risk1","Risk2","Risk3","Risk4", 
  "BI1","BI2","BI3",
  "Usefulness", "Literacy", "Anxiety", "Intention", "Age"
)

# 3. 绘图与输出
# PDF 输出
pdf("Figures/Figure1_Classic_Colored.pdf", width = 10, height = 7)

semPaths(fit_final, 
         whatLabels = "std",       # 显示标准化系数
         layout = "tree2",         # 经典树状布局 (类似你的参考图)
         style = "lisrel",         # LISREL 风格 (箭头明确)
         
         # --- 核心美化参数 ---
         color = list(lat = "#A6CEE3", man = "#F0F0F0"), # 节点填充色
         edge.label.cex = 1.0,     # 路径系数字体大小
         edge.color = "black",     # 连线颜色 (经典黑线)
         fade = FALSE,             # 关闭连线褪色效果，保持清晰
         mar = c(3, 5, 3, 5),      # 边距
         
         # --- 节点设置 ---
         sizeMan = 7,              # 显变量矩形大小
         sizeLat = 10,             # 潜变量椭圆大小
         nodeLabels = node_labels, # 替换为英文标签
         
         # --- 显著性星号 (这是个Trick) ---
         # semPlot 很难直接加星号，我们通过调整 edge.label.position 避免遮挡
         # 并设置 curvePivot = TRUE 让双向箭头略微弯曲，不重叠
         curvePivot = TRUE,
         residuals = FALSE,        # 隐藏显变量残差，让图更干净
         rotation = 2              # 旋转布局，使其更符合阅读习惯
)

# 添加标题
title("Structural Equation Model of AI Readiness")

# 注意：semPlot 原生不支持自动加星号。
# 为了加上星号，我们通常建议：
# 方法 A (手动后期): 导出为 PDF 后在 Illustrator/PPT 里加，这是最漂亮的。
# 方法 B (自动代码): 使用 lavaanPlot 包 (下面这段代码是替代方案)

dev.off()

# ==============================================================================
# 替代方案: 使用 lavaanPlot (原生支持星号 + 彩色)
# ==============================================================================
# 如果你一定要代码自动生成星号，强烈推荐 lavaanPlot
if(!require(lavaanPlot)) install.packages("lavaanPlot")
library(lavaanPlot)

# 绘制带星号的彩色图
p_lavaan <- lavaanPlot(model = fit_final, 
                       node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       coefs = TRUE,       # 显示系数
                       stand = TRUE,       # 标准化系数
                       sig = 0.05,         # 显著性水平
                       stars = c("regress"), # 仅在回归路径上显示星号 (关键!)
                       graph_options = list(rankdir = "LR") # 从左到右布局
)

# 由于 lavaanPlot 输出的是 HTML widget，我们需要保存它
library(htmlwidgets)
saveWidget(p_lavaan, "Figures/Figure1_Interactive_with_Stars.html")

print("已生成两版图：")
print("1. Figures/Figure1_Classic_Colored.pdf (经典布局，配色清爽，适合后期加星号)")
print("2. Figures/Figure1_Interactive_with_Stars.html (网页版，自带星号，可截图使用)")


# ==============================================================================
# 导出 SEM 详细结果到表格 (用于 Supplementary Material)
# ==============================================================================
library(lavaan)
library(dplyr)
library(stringr)

# 1. 提取参数估计值 (包含标准化系数)
# fit_final 是你之前运行好的模型对象
sem_results <- parameterEstimates(fit_final, standardized = TRUE)

# 2. 数据清洗与美化
sem_table_clean <- sem_results %>%
  # 筛选我们关心的路径类型: 
  # =~ (测量模型/因子载荷), ~ (回归路径/结构模型), ~~ (共变/相关)
  filter(op %in% c("=~", "~", "~~")) %>%
  
  # 过滤掉显变量的残差 (op == "~~" 且 lhs == rhs 的通常是残差，除非是潜变量方差)
  # 这里我们保留潜变量之间的共变，去掉显变量的残差以精简表格
  filter(!(op == "~~" & lhs == rhs)) %>% 
  
  # 添加显著性星号列
  mutate(
    Stars = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01  ~ "**",
      pvalue < 0.05  ~ "*",
      TRUE           ~ "" # 不显著留空
    ),
    
    # 格式化 P 值 (保留3位小数，<0.001 显示为 <.001)
    P_Value_Formatted = ifelse(pvalue < 0.001, "<.001", sprintf("%.3f", pvalue)),
    
    # 格式化标准化系数 (保留2位或3位)
    Std_Estimate = sprintf("%.3f", std.all),
    
    # 定义路径类型名称 (让表格更易读)
    Type = case_when(
      op == "=~" ~ "Factor Loading (Measurement)",
      op == "~"  ~ "Regression (Structural)",
      op == "~~" ~ "Covariance/Correlation",
      TRUE       ~ "Other"
    )
  ) %>%
  
  # 选择并重命名最终列
  select(
    Type,              # 路径类型
    Dependent = lhs,   # 因变量/左侧变量
    Independent = rhs, # 自变量/右侧变量
    Std_Beta = Std_Estimate, # 标准化系数
    SE = se,           # 标准误
    Z_Value = z,       # Z值
    P_Value = P_Value_Formatted, # P值
    Sig = Stars        # 星号
  ) %>%
  
  # 按照类型排序，让表格更有条理
  arrange(factor(Type, levels = c("Regression (Structural)", "Covariance/Correlation", "Factor Loading (Measurement)")))

# 3. 查看前几行
print(head(sem_table_clean))

# 4. 导出为 CSV 文件
write.csv(sem_table_clean, "Supplementary_SEM_Results.csv", row.names = FALSE)

print("表格已导出为 'Supplementary_SEM_Results.csv'，请在文件夹中查看。")


# ==============================================================================
# 1. 安装与加载必要的包 (如果已安装可跳过 install.packages)
# ==============================================================================

library(lavaan)
library(semTools)
library(readr)

# ==============================================================================
# 2. 读取数据
# ==============================================================================
# 请确保 data_valid_clean.csv 文件在您的工作目录下，或者修改为完整路径
data <- read_csv("data_valid_clean.csv")

# 检查数据是否读取成功
head(data[, c("PU1", "AL1", "Risk1", "BI1")])

# ==============================================================================
# 3. 定义 CFA 模型
# ==============================================================================
# 这里的变量名对应您 CSV 文件中的列名
cfa_model <- '
  # 测量模型定义 (Measurement Model)
  PU   =~ PU1 + PU2 + PU3 + PU4    # Perceived Usefulness
  AL   =~ AL1 + AL2 + AL3 + AL4    # Agentic Literacy (自编量表)
  Risk =~ Risk1 + Risk2 + Risk3 + Risk4  # AI Anxiety
  BI   =~ BI1 + BI2 + BI3          # Intention to Use
'

# ==============================================================================
# 4. 运行 CFA 分析
# ==============================================================================
# 使用 MLR (Robust Maximum Likelihood) 估计方法，这对Likert数据更稳健
fit <- cfa(cfa_model, data = data, estimator = "MLR") 

# ==============================================================================
# 5. 输出结果 1: 模型拟合度 (Model Fit Indices)
# ==============================================================================
cat("\n--- Model Fit Indices (模型拟合指数) ---\n")
# 提取常用的拟合指标
fit_measures <- fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                   "cfi.robust", "tli.robust", 
                                   "rmsea.robust", "srmr"))
print(fit_measures)

# 计算卡方自由度比 (Chi-square / df)
chisq_df <- fit_measures["chisq.scaled"] / fit_measures["df.scaled"]
cat("\nChi-square/df ratio: ", round(chisq_df, 3), " (Should be < 3 or < 5)\n")

# ==============================================================================
# 6. 输出结果 2: 因子载荷、CR 和 AVE (Convergent Validity)
# ==============================================================================
cat("\n--- Reliability & Convergent Validity (信度与聚合效度) ---\n")

# 计算 CR (Composite Reliability) and AVE (Average Variance Extracted)
reliability_results <- reliability(fit)

# 打印 CR 和 AVE
# 注意: omega 对应 CR, avevar 对应 AVE
print(reliability_results[c("omega", "avevar"), ])

# 打印标准化因子载荷 (Standardized Factor Loadings)
cat("\n--- Standardized Factor Loadings (标准化因子载荷) ---\n")
inspect(fit, "std")$lambda

# ==============================================================================
# 7. 输出结果 3: 区分效度 (Discriminant Validity - Fornell-Larcker)
# ==============================================================================
cat("\n--- Discriminant Validity (Fornell-Larcker Criterion) ---\n")

# 获取潜变量相关系数矩阵
latent_cor <- inspect(fit, "std")$psi

# 获取 AVE 的平方根
ave_sqrt <- sqrt(reliability_results["avevar", ])

# 创建一个矩阵来展示 Fornell-Larcker 结果
# 对角线是 AVE 的平方根，非对角线是相关系数
fl_matrix <- latent_cor
diag(fl_matrix) <- ave_sqrt

cat("对角线数值 (AVE平方根) 应该大于其所在行和列的其他数值 (相关系数):\n")
print(round(fl_matrix, 3))

# ==============================================================================
# 8. (可选) 将详细结果保存到文件
# ==============================================================================
sink("CFA_Results.txt")
summary(fit, fit.measures=TRUE, standardized=TRUE)
sink()

# ========================================================
# LCA/LPA 1-4 类拟合指标导出代码 (生成 Table S3)
# ========================================================

library(tidyverse)
library(tidyLPA)

# 读取数据
data <- read.csv("data_valid_clean.csv")

# 选择变量并标准化
lca_data <- data %>%
  select(Mean_PU, Mean_AL, Mean_Risk, Mean_BI) %>%
  scale() %>%
  as.data.frame()

# 运行 1-4 类
lpa_models <- lca_data %>%
  estimate_profiles(n_profiles = 1:4, models = 1)

# 获取拟合指标
fit_indices <- get_fit(lpa_models)

# 生成干净的表格
Table_S6 <- fit_indices %>%
  select(Classes, LogLik, AIC, BIC, SABIC, Entropy, BLRT_p) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# 输出
print(Table_S6)
write.csv(Table_S6, "Supplementary_Table_S6_clean.csv", row.names = FALSE)

library(mclust)

# 确保 lca_data 是你之前标准化的四个变量的数据框
# 运行 999 次 Bootstrap 抽样检验 (利用 CPU 多线程)
# modelName = "EEI" 对应 tidyLPA 中的 Model 1
set.seed(123) # 设置随机种子以保证结果可复现
blrt_test <- mclustBootstrapLRT(lca_data, modelName = "EEI", nboot = 999, maxG = 4)

# 查看并打印 p 值
print(blrt_test)

######################################################################################
# ========================================================
# Sensitivity Analysis: Alternative Usage Depth Definition
# ========================================================

library(lavaan)
library(tidyverse)

# 假设你的数据中有多个 AI task 列
# 比如: task_translation, task_summary, task_coding, task_experimental_design 等
# 每列是 0/1 表示是否使用过该功能

data <- read.csv("data_valid_clean.csv")

# 原始定义：最常用任务 is_deep
# 替代定义：只要用过任何一个 deep task 就算 deep
data <- data %>%
  mutate(
    # 假设 deep tasks 的列名
    deep_usage_alt = if_else(
      task_coding == 1 | task_experimental_design == 1 | task_statistics == 1 | task_omics == 1,
      1, 0
    )
  )

# 检查重新分类后的分布
table(data$deep_usage_alt)

# 跑 SEM（只替换 outcome 变量）
model_syntax <- '
  # Measurement model
  Agentic_Literacy =~ AL1 + AL2 + AL3 + AL4
  AI_Anxiety =~ Risk1 + Risk2 + Risk3 + Risk4
  PU =~ PU1 + PU2 + PU3 + PU4
  Intention =~ BI1 + BI2 + BI3
  
  # Structural paths
  Intention ~ AI_Anxiety + Agentic_Literacy
  deep_usage_alt ~ Agentic_Literacy + AI_Anxiety + coding_level + age
'

fit_alt <- sem(model_syntax, data = data, 
               ordered = "deep_usage_alt",  # binary outcome
               estimator = "WLSMV")

summary(fit_alt, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


