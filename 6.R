# ==============================================================================
# 0. 加载必要的包
# ==============================================================================
library(tidyverse)
library(gtsummary)

# ==============================================================================
# 1. 稳健的数据读取 (解决乱码的关键)
# ==============================================================================
# 方案 A: 如果文件是 UTF-8 (您上传的文件是这个格式)
# raw_data <- read_csv("data.csv", 
#                      locale = locale(encoding = "UTF-8"), # 显式指定 UTF-8
#                      show_col_types = FALSE)

# 方案 B: 如果您本地跑出来还是乱码，说明您本地文件是 GBK，请改用下面这行：
raw_data <- read_csv("data.csv", locale = locale(encoding = "GBK"), show_col_types = FALSE)

# ==============================================================================
# 2. 数据清洗与变量重构
# ==============================================================================
clean_df <- raw_data %>%
  # --- 2.1 变量提取与重命名 (根据原始列位置或名称) ---
  select(
    # 注意：请根据实际列名调整，这里使用了您的原始逻辑
    Degree_Raw = matches("学位类型"),
    Coding_Raw = matches("编程/统计软件基础"),
    Check_Question = matches("问卷导语"), # 筛选题
    Task_Main_Raw = matches("频率最高"),  # 最依赖的任务
    # 提取量表题 (示例：根据列名关键词提取)
    PU_Cols = contains("效率"), 
    Risk_Cols = contains("焦虑|风险"),
    BI_Cols = contains("意愿|推荐")
  ) %>%
  
  # --- 2.2 筛选有效问卷 (这是找回数据的关键步骤) ---
  # 之前可能因为乱码导致 str_detect 失败，现在编码正确后可以正常筛选
  filter(str_detect(Check_Question, "能够调用外部工具")) %>%
  
  mutate(
    # --- 2.3 核心修正：编程等级映射 (找回 Intermediate) ---
    Coding_Level = case_when(
      str_detect(Coding_Raw, "零基础") ~ "Zero Basis",
      str_detect(Coding_Raw, "入门") ~ "Novice",
      str_detect(Coding_Raw, "进阶") ~ "Intermediate", # 关键：编码正确后，这里能匹配到了！
      str_detect(Coding_Raw, "熟练|精通") ~ "Advanced",
      TRUE ~ "Other" # 兜底
    ),
    # 设置因子顺序，确保绘图和表格顺序正确
    Coding_Level = factor(Coding_Level, levels = c("Zero Basis", "Novice", "Intermediate", "Advanced")),
    
    # --- 2.4 任务类型归类 ---
    Main_Task_Eng = case_when(
      str_detect(Task_Main_Raw, "文献") ~ "Literature Review",
      str_detect(Task_Main_Raw, "翻译|润色") ~ "Translation & Polishing",
      str_detect(Task_Main_Raw, "代码|统计") ~ "Coding & Statistics",
      str_detect(Task_Main_Raw, "绘图") ~ "Visualization",
      str_detect(Task_Main_Raw, "实验设计") ~ "Exp. Design",
      str_detect(Task_Main_Raw, "审稿") ~ "Rebuttal",
      TRUE ~ "Other/None"
    ),
    
    # 定义任务深度
    Task_Category = case_when(
      Main_Task_Eng %in% c("Translation & Polishing", "Literature Review") ~ "Shallow Processing",
      Main_Task_Eng %in% c("Coding & Statistics", "Exp. Design", "Visualization") ~ "Deep Reasoning",
      TRUE ~ "Other"
    )
  )

# ==============================================================================
# 3. 结果验证
# ==============================================================================
# 打印各组人数，确认 Intermediate 组是否存在
print("样本分布情况：")
table(clean_df$Coding_Level)

# ==============================================================================
# 4. 生成 Table 1 (支持复杂表头)
# ==============================================================================
table1_final <- clean_df %>%
  select(Task_Category, Main_Task_Eng, Coding_Level) %>%
  tbl_summary(
    by = Coding_Level,
    label = list(Task_Category ~ "Task Depth", Main_Task_Eng ~ "Specific Task"),
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_overall() %>% 
  add_p(
    # 使用模拟 P 值解决内存报错问题
    test = all_categorical() ~ "fisher.test",
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  bold_labels()
print(table1_final)

# 保存为 Word (需要 huxtable 或 flextable 包，可选)
library(flextable)
table1_final %>% as_flex_table() %>% save_as_docx(path = "Table1_Depth_Deficit.docx")


#########################################################################################
library(tidyverse)
library(gtsummary)

# 1. 读取数据 (确保使用 UTF-8)
raw_data <- read_csv("data.csv", locale = locale(encoding = "GBK"), show_col_types = FALSE)

# 2. 数据清洗 (使用列索引号，绕过中文匹配问题)
clean_df <- raw_data %>%
  # --- 变量提取 (基于列的位置，R索引从1开始) ---
  select(
    Degree_Raw = 7,          # 第7列：学位类型
    Coding_Raw = 12,         # 第12列：编程基础
    Check_Question = 13,     # 第13列：筛选题 (关键列)
    Task_Main_Raw = 16,      # 第16列：主要任务
    
    # 提取量表题 (根据之前的分析锁定位置)
    # PU (效率): 第17-20列
    PU1 = 17, PU2 = 18, PU3 = 19, PU4 = 20,
    # AL (素养): 第21-24列
    AL1 = 21, AL2 = 22, AL3 = 23, AL4 = 24,
    # Risk (焦虑): 第26-29列 (跳过第25列"不使用的原因")
    Risk1 = 26, Risk2 = 27, Risk3 = 28, Risk4 = 29,
    # BI (意愿): 第30-32列
    BI1 = 30, BI2 = 31, BI3 = 32
  ) %>%
  
  # --- 筛选有效问卷 ---
  # 现在 Check_Question 肯定存在了
  filter(str_detect(Check_Question, "能够调用外部工具")) %>%
  
  # --- 变量转换 ---
  mutate(
    # 1. 学位类型
    Degree = case_when(
      str_detect(Degree_Raw, "学术") ~ "Academic Degree",
      str_detect(Degree_Raw, "专业") ~ "Professional Degree",
      TRUE ~ "Other"
    ),
    
    # 2. 编程水平 (含 Intermediate)
    Coding_Level = case_when(
      str_detect(Coding_Raw, "零基础") ~ "Zero Basis",
      str_detect(Coding_Raw, "入门") ~ "Novice",
      str_detect(Coding_Raw, "进阶") ~ "Intermediate", 
      str_detect(Coding_Raw, "熟练|精通") ~ "Advanced",
      TRUE ~ "Other"
    ),
    Coding_Level = factor(Coding_Level, levels = c("Zero Basis", "Novice", "Intermediate", "Advanced")),
    
    # 3. 任务类型
    Main_Task_Eng = case_when(
      str_detect(Task_Main_Raw, "文献") ~ "Literature Review",
      str_detect(Task_Main_Raw, "翻译|润色") ~ "Translation & Polishing",
      str_detect(Task_Main_Raw, "代码|统计") ~ "Coding & Statistics",
      str_detect(Task_Main_Raw, "绘图") ~ "Visualization",
      str_detect(Task_Main_Raw, "实验设计") ~ "Exp. Design",
      str_detect(Task_Main_Raw, "审稿") ~ "Rebuttal",
      TRUE ~ "Other/None"
    ),
    Task_Category = case_when(
      Main_Task_Eng %in% c("Translation & Polishing", "Literature Review") ~ "Shallow Processing",
      Main_Task_Eng %in% c("Coding & Statistics", "Exp. Design", "Visualization") ~ "Deep Reasoning",
      TRUE ~ "Other"
    ),
    
    # 4. 量表转数字
    across(starts_with(c("PU", "AL", "Risk", "BI")), ~ case_when(
      str_detect(., "非常不同意|完全不符合") ~ 1,
      str_detect(., "不同意|不符合") ~ 2,
      str_detect(., "一般") ~ 3,
      str_detect(., "非常同意|完全符合") ~ 5,
      str_detect(., "同意|符合") ~ 4,
      TRUE ~ 3
    ))
  ) %>%
  
  # 5. 计算均分
  rowwise() %>%
  mutate(
    Mean_PU = mean(c(PU1, PU2, PU3, PU4), na.rm = TRUE),
    Mean_AL = mean(c(AL1, AL2, AL3, AL4), na.rm = TRUE),
    Mean_Risk = mean(c(Risk1, Risk2, Risk3, Risk4), na.rm = TRUE),
    Mean_BI = mean(c(BI1, BI2, BI3), na.rm = TRUE)
  ) %>%
  ungroup()

# 检查数据是否正常
print(table(clean_df$Coding_Level))


# ==============================================================================
# 0. 加载必要的包 & 设置
# ==============================================================================
library(ggplot2)
library(ggpubr)
library(patchwork)
library(tidyr)
library(ggsci) # 引入顶级期刊配色包

# 确保使用之前清洗好的数据
df <- clean_df 

# ==============================================================================
# 1. Figure 1: 焦虑驱动意愿 (散点 + 密度图润色版)
# 亮点：使用 Nature 风格配色，调整点的大小和透明度
# ==============================================================================

p1 <- ggplot(df, aes(x = Mean_Risk, y = Mean_BI)) +
  # 1.1 散点层：稍微调大一点点，增加透明度防止重叠
  geom_jitter(aes(color = Coding_Level, shape = Coding_Level), 
              alpha = 0.7, width = 0.15, height = 0.15, size = 2.5) + 
  
  # 1.2 拟合线：使用深灰色，虚线置信区间
  geom_smooth(method = "lm", color = "#3C5488B2", fill = "#3C5488B2", 
              alpha = 0.15, linewidth = 1) +
  
  # 1.3 统计指标：放在显眼但不挡数据的位置
  stat_cor(method = "pearson", label.x = 1, label.y = 4.9, 
           size = 5, family = "sans", label.sep = "\n") +
  
  # 1.4 配色与主题
  scale_color_npg(name = "Coding Proficiency") + # Nature Publishing Group 配色
  scale_shape_manual(values = c(16, 17, 15, 18), name = "Coding Proficiency") + # 不同形状区分
  
  labs(
    title = "A. The Paradox of Defensive Adoption",
    subtitle = "Higher AI anxiety correlates with stronger intention to use",
    x = "AI Anxiety (Mean Score)",
    y = "Intention to Use (Mean Score)"
  ) +
  theme_pubr(base_size = 12) +
  theme(
    legend.position = "top", # 图例放上面，节省横向空间
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# ==============================================================================
# 2. Figure 2: 学位差异 (小提琴图 + 箱线图)
# 亮点：增加小提琴图层，展示数据分布密度
# ==============================================================================

# 数据准备
df_long <- df %>%
  filter(Degree %in% c("Academic Degree", "Professional Degree")) %>%
  select(Degree, Mean_AL, Mean_Risk) %>%
  pivot_longer(cols = c(Mean_AL, Mean_Risk), names_to = "Metric", values_to = "Score") %>%
  mutate(
    Metric_Label = ifelse(Metric == "Mean_AL", "AI Literacy (Skill)", "AI Anxiety (Risk)"),
    # 简化横坐标标签
    Degree_Label = ifelse(Degree == "Academic Degree", "Academic", "Professional")
  )

p2 <- ggplot(df_long, aes(x = Degree_Label, y = Score, fill = Degree)) +
  # 2.1 小提琴图：展示分布密度
  geom_violin(alpha = 0.3, color = NA, trim = FALSE) +
  
  # 2.2 箱线图：展示中位数和四分位
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA, color = "black") +
  
  # 2.3 抖动散点：展示原始数据分布
  geom_jitter(width = 0.1, alpha = 0.2, size = 1, color = "black") +
  
  # 2.4 分面显示
  facet_wrap(~Metric_Label, scales = "free_y") + 
  
  # 2.5 统计检验
  stat_compare_means(method = "t.test", label = "p.signif", 
                     label.y = 5.2, size = 6, vjust = 0.5) + 
  
  # 2.6 配色与标签
  scale_fill_npg() + # 保持配色一致
  labs(
    title = "B. Disparities by Degree Type",
    subtitle = "Professional students show comparable anxiety but varying literacy profiles",
    x = NULL,
    y = "Likert Score (1-5)"
  ) +
  theme_pubr(base_size = 12) +
  theme(
    legend.position = "none", # 不需要图例，横坐标已说明
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(size = 12, face = "bold"), # 分面标题加粗
    strip.background = element_rect(fill = "#E8E8E8", color = NA) # 分面背景灰底
  )

# ==============================================================================
# 3. 组合与导出 (PDF)
# ==============================================================================

combined_plot <- p1 / p2 + 
  plot_layout(heights = c(1.3, 1)) # 上图稍微高一点，留给图例

# 导出 PDF (矢量图，适合投稿)
ggsave("Figure_1_Final.pdf", plot = combined_plot, width = 8, height = 10, device = cairo_pdf)
# 同时也导出一个高清 PNG 方便插入 Word 预览
ggsave("Figure_1_Final.png", plot = combined_plot, width = 8, height = 10, dpi = 600)

print("Figure_1_Final.pdf 已生成。")
