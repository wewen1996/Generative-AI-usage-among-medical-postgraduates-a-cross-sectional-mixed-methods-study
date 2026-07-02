library(lavaan)
library(dplyr)

# 读取数据
data <- read.csv("data_valid_clean.csv")

# 1. 基础CFA模型 (你的原模型)
model_baseline <- '
  PU   =~ PU1 + PU2 + PU3 + PU4
  AL   =~ AL1 + AL2 + AL3 + AL4
  Risk =~ Risk1 + Risk2 + Risk3 + Risk4
  BI   =~ BI1 + BI2 + BI3
'

# 2. ULMC模型 (加入未测方法因子M)
model_ulmc <- '
  PU   =~ PU1 + PU2 + PU3 + PU4
  AL   =~ AL1 + AL2 + AL3 + AL4
  Risk =~ Risk1 + Risk2 + Risk3 + Risk4
  BI   =~ BI1 + BI2 + BI3
  
  # 所有观测变量都在方法因子M上有载荷
  M =~ PU1 + PU2 + PU3 + PU4 + AL1 + AL2 + AL3 + AL4 + Risk1 + Risk2 + Risk3 + Risk4 + BI1 + BI2 + BI3
  
  # 设定方法因子与实质因子正交（无相关性）
  M ~~ 0*PU
  M ~~ 0*AL
  M ~~ 0*Risk
  M ~~ 0*BI
'

# 运行模型
fit_base <- cfa(model_baseline, data = data, std.lv = TRUE, estimator = "MLR")
fit_ulmc <- cfa(model_ulmc, data = data, std.lv = TRUE, estimator = "MLR")

# 比较结果
summary(fit_base, fit.measures = TRUE)
summary(fit_ulmc, fit.measures = TRUE)

# 查看拟合差异 (SCI汇报重点)
anova(fit_base, fit_ulmc)

library(lavaan)
library(interactions)

# 数据二值化处理：定义“深层使用”
# 包含：Exp. Design (39人), Coding & Statistics (33人), Visualization (9人)
data$Usage_Depth <- ifelse(data$Main_Task_Eng %in% c("Exp. Design", "Coding & Statistics", "Visualization"), 1, 0)

# 变量中心化 (消除多重共线性)
data$PU_c <- scale(data$Mean_PU, center = TRUE, scale = FALSE)
data$AL_c <- scale(data$Mean_AL, center = TRUE, scale = FALSE)
data$Risk_c <- scale(data$Mean_Risk, center = TRUE, scale = FALSE)

# 构建交互项
data$PU_AL_int <- data$PU_c * data$AL_c

# 广义结构方程模型 (GSEM)，Y为二分类变量
model_mod_med <- '
  # 中介路径: Risk -> PU
  PU_c ~ a * Risk_c
  
  # 因变量路径: PU + AL + PU*AL -> Depth (使用逻辑回归链接)
  Usage_Depth ~ b1 * PU_c + b2 * AL_c + b3 * PU_AL_int + cp * Risk_c
  
  # 定义条件间接效应 (Condition Indirect Effects)
  # AL低水平 (-1 SD, 设 SD=0.8 根据你的数据)
  indirect_low  := a * (b1 + b3 * (-0.8))
  # AL均值水平
  indirect_mean := a * (b1 + b3 * 0)
  # AL高水平 (+1 SD)
  indirect_high := a * (b1 + b3 * (0.8))
'

# 运行模型 (使用 Bootstrapping 5000 次，利用双3090算力)
fit_mod_med <- sem(model_mod_med, 
                   data = data, 
                   ordered = "Usage_Depth", 
                   estimator = "DWLS",      # <--- 【修正点】显式指定 DWLS 估计器
                   se = "bootstrap", 
                   bootstrap = 5000, 
                   parallel = "snow",       # Windows 系统使用 snow 模式
                   ncpus = 16)

summary(fit_mod_med, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

# 加载必要的包
library(ggplot2)
library(dplyr)

# 1. 读取数据
data <- read.csv("data_valid_clean.csv")

# 2. 数据处理与转化
data_plot <- data %>%
  # 将 Main_Task_Eng 转化为二分类变量：1=深层使用，0=浅层使用
  mutate(Usage_Depth = ifelse(Main_Task_Eng %in% c("Exp. Design", "Coding & Statistics", "Visualization"), 1, 0)) %>%
  # 根据 Mean_AL 的中位数（Median Split），将人群分为“高素养”和“低素养”
  mutate(AL_Group = ifelse(Mean_AL >= median(Mean_AL), "High Agentic Literacy", "Low Agentic Literacy")) %>%
  # 转换为因子并固定水平顺序，确保“低”在左，“高”在右
  mutate(AL_Group = factor(AL_Group, levels = c("Low Agentic Literacy", "High Agentic Literacy"))) %>%
  # 分组计算深层使用的比例（*100 转化为百分比）
  group_by(AL_Group) %>%
  summarise(Deep_Usage_Percentage = mean(Usage_Depth) * 100)

# 3. 绘制 SCI 级学术条形图
p <- ggplot(data_plot, aes(x = AL_Group, y = Deep_Usage_Percentage, fill = AL_Group)) +
  # 绘制柱状图：调整宽度，添加黑色边框增加质感
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE, color = "black", size = 0.5) +
  # 在柱子顶端添加百分比数值标签
  geom_text(aes(label = sprintf("%.1f%%", Deep_Usage_Percentage)), vjust = -0.5, size = 5, fontface = "bold") +
  # 定义 BMC 期刊偏好的学术配色（低素养用浅灰警示，高素养用深蓝强调）
  scale_fill_manual(values = c("Low Agentic Literacy" = "#B0BEC5", "High Agentic Literacy" = "#1E88E5")) +
  # 动态设置 Y 轴上限，防止标签溢出
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(data_plot$Deep_Usage_Percentage) + 10)) +
  # 坐标轴与标题设置
  labs(
    x = "Agentic Literacy Level",
    y = "Proportion of Deep Usage (%)",
    title = "The Intermediate Coding Threshold in GenAI Adoption"
  ) +
  # 经典极简学术主题
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.ticks.length = unit(0.2, "cm")
  )

# 4. 保存为高分辨率 TIFF 格式 (直接用于投稿)
ggsave("Figure_Agentic_Literacy_Threshold.tiff", plot = p, width = 6, height = 5, dpi = 300, compression = "lzw")

# 同时在 RStudio 中显示
print(p)

# ==========================================
# SCI 高水平补充分析: 文本挖掘与结构主题模型 (STM)
# ==========================================

# 1. 加载必要的R包
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, jiebaR, tidytext, stm, quanteda, ggpubr, readr)

# 2. 读取数据并进行变量重命名 (适配你的表头)
raw_data <- read_csv("data.xlsx - Sheet1.csv")

# 提取最后一道开放性问题文本及焦虑量表得分
text_data <- raw_data %>%
  rename(
    Text_Response = `如果您可以定制一个专属的“医学科研AI助手”，或者学校开设AI相关课程，您最希望它能帮您解决哪一个具体的“痛点”问题？`,
    Anx1 = `关于“技术焦虑与伦理感知”，您的看法是: (1-非常不同意，2-不同意，3-一般，4-同意，5-非常同意)—C1. 我担心上传未发表的数据给AI会导致隐私泄露或被模型训练`,
    Anx2 = `C2. 我担心过度依赖AI会削弱我独立思考和撰写论文的能力`,
    Anx3 = `C3. 我不确定在论文中使用AI辅助是否会被期刊认定为学术不端`,
    Anx4 = `C4. 面对AI生成的统计结果，我往往持怀疑态度，会进行二次验证`
  ) %>%
  # 计算AI焦虑均值
  mutate(AI_Anxiety = (Anx1 + Anx2 + Anx3 + Anx4) / 4) %>%
  # 剔除未填写的空文本
  filter(!is.na(Text_Response) & Text_Response != "(空)") %>%
  mutate(Doc_ID = row_number())

# 3. 中文分词与词汇复杂度计算 (Lexical Complexity)
cutter <- worker(stop_word = "stop_words.txt") # 如果有停用词表请配置，否则去掉stop_word参数

text_tokens <- text_data %>%
  mutate(Tokens = map(Text_Response, ~segment(.x, cutter))) %>%
  unnest(Tokens) %>%
  group_by(Doc_ID) %>%
  # 计算每条回复的长度(Token Count)和词汇丰富度(TTR: Type-Token Ratio)
  summarise(
    Word_Count = n(),
    Unique_Words = n_distinct(Tokens),
    TTR = Unique_Words / Word_Count, # 词汇多样性指标
    AI_Anxiety = mean(AI_Anxiety)
  )

# 4. 相关性检验：高焦虑是否导致表述匮乏？
cor_test <- cor.test(text_tokens$AI_Anxiety, text_tokens$Word_Count, method = "spearman")
print(cor_test)

# 5. 可视化：焦虑与Prompt复杂度的负相关关系
p_corr <- ggscatter(text_tokens, x = "AI_Anxiety", y = "Word_Count", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "AI Anxiety Score", ylab = "Prompt Complexity (Word Count)",
                    title = "Relationship between AI Anxiety and Prompt Elaboration") +
  theme_classic()

ggsave("Figure_Text_Mining_Correlation.tiff", plot = p_corr, dpi = 300, width = 6, height = 5)

# ==========================================
# 进阶分析：使用结构主题模型 (Structural Topic Model)
# 寻找高焦虑学生的关注点
# ==========================================

# 创建文本语料库
dfm_counts <- text_data %>%
  mutate(Tokens = map(Text_Response, ~segment(.x, cutter))) %>%
  unnest(Tokens) %>%
  count(Doc_ID, Tokens) %>%
  cast_dfm(Doc_ID, Tokens, n)

# 拟合 STM 模型 (利用 3090 GPU 并行，寻找最佳K个主题)
# 公式 ~ AI_Anxiety 意味着将焦虑作为主题生成的前提变量
stm_model <- stm(dfm_counts, K = 4, prevalence = ~ AI_Anxiety, 
                 max.em.its = 100, data = text_data, init.type = "Spectral")

# 评估焦虑对不同主题的影响
prep <- estimateEffect(1:4 ~ AI_Anxiety, stm_model, meta = text_data)
summary(prep)

# 绘制主题图
plot(prep, covariate = "AI_Anxiety", topics = c(1, 2, 3, 4),
     model = stm_model, method = "difference",
     cov.value1 = 5, cov.value2 = 1, # 比较高焦虑(5) vs 低焦虑(1)
     xlab = "More likely in Low Anxiety <--------> More likely in High Anxiety",
     main = "Effect of AI Anxiety on Expressed Research Pain-points")


# ==========================================
# SCI 高水平补充分析: 文本挖掘与结构主题模型 (STM)
# ==========================================

# 1. 加载必要的R包
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, jiebaR, tidytext, stm, quanteda, ggpubr, readr)

# 2. 读取数据并进行变量重命名 (适配你的表头)
raw_data <- read_csv("data.csv",locale = locale(encoding = "GBK"))

# 3. 数据清洗、变量重命名与【文本转数值】
text_data <- raw_data %>%
  rename(
    Text_Response = `如果您可以定制一个专属的“医学科研AI助手”，或者学校开设AI相关课程，您最希望它能帮您解决哪一个具体的“痛点”问题？`,
    Anx1 = `关于“技术焦虑与伦理感知”，您的看法是: (1-非常不同意，2-不同意，3-一般，4-同意，5-非常同意)—C1. 我担心上传未发表的数据给AI会导致隐私泄露或被模型训练`,
    Anx2 = `C2. 我担心过度依赖AI会削弱我独立思考和撰写论文的能力`,
    Anx3 = `C3. 我不确定在论文中使用AI辅助是否会被期刊认定为学术不端`,
    Anx4 = `C4. 面对AI生成的统计结果，我往往持怀疑态度，会进行二次验证`
  ) %>%
  # 【新增核心修正】：将中文字符映射为数字
  mutate(across(c(Anx1, Anx2, Anx3, Anx4), ~ case_match(.,
                                                        "非常不同意" ~ 1,
                                                        "不同意"   ~ 2,
                                                        "一般"     ~ 3,
                                                        "同意"     ~ 4,
                                                        "非常同意"   ~ 5,
                                                        .default   = NA # 将其他异常值（如"跳过"）设为缺失值
  ))) %>%
  # 计算AI焦虑均值 (na.rm = TRUE 保证有缺失值时也能计算)
  mutate(AI_Anxiety = (Anx1 + Anx2 + Anx3 + Anx4) / 4) %>%
  # 剔除未填写的空文本
  filter(!is.na(Text_Response) & Text_Response != "(空)") %>%
  mutate(Doc_ID = row_number())

# 检查转换后的数据类型（此时 AI_Anxiety 应该是 numeric/dbl）
str(text_data$AI_Anxiety)

# 1. 强制激活分词包
library(jiebaR)

# 2. 安全初始化分词引擎 (Scientific Initialization)
# 策略：不调用外部文件，直接使用 jiebaR 内置的学术标准停用词
cutter <- worker() 

# 3. 继续运行后续的分词与统计代码
text_tokens <- text_data %>%
  mutate(Tokens = map(Text_Response, ~segment(.x, cutter))) %>%
  unnest(Tokens) %>%
  group_by(Doc_ID) %>%
  summarise(
    Word_Count = n(),
    AI_Anxiety = mean(AI_Anxiety, na.rm = TRUE) # 确保有缺失值时均值也能计算
  )

# 4. 再次检查是否成功
head(text_tokens)

text_tokens <- text_data %>%
  mutate(Tokens = map(Text_Response, ~segment(.x, cutter))) %>%
  unnest(Tokens) %>%
  group_by(Doc_ID) %>%
  # 计算每条回复的长度(Token Count)和词汇丰富度(TTR: Type-Token Ratio)
  summarise(
    Word_Count = n(),
    Unique_Words = n_distinct(Tokens),
    TTR = Unique_Words / Word_Count, # 词汇多样性指标
    AI_Anxiety = mean(AI_Anxiety)
  )

# 4. 相关性检验：高焦虑是否导致表述匮乏？
cor_test <- cor.test(text_tokens$AI_Anxiety, text_tokens$Word_Count, method = "spearman")
print(cor_test)

# 5. 可视化：焦虑与Prompt复杂度的负相关关系
p_corr <- ggscatter(text_tokens, x = "AI_Anxiety", y = "Word_Count", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "AI Anxiety Score", ylab = "Prompt Complexity (Word Count)",
                    title = "Relationship between AI Anxiety and Prompt Elaboration") +
  theme_classic()

ggsave("Figure_Text_Mining_Correlation.tiff", plot = p_corr, dpi = 300, width = 6, height = 5)
ggsave("Figure_Text_Mining_Correlation.pdf", plot = p_corr, dpi = 300, width = 6, height = 5)
# ==========================================
# 进阶分析：使用结构主题模型 (Structural Topic Model)
# 寻找高焦虑学生的关注点
# ==========================================

# 创建文本语料库
dfm_counts <- text_data %>%
  mutate(Tokens = map(Text_Response, ~segment(.x, cutter))) %>%
  unnest(Tokens) %>%
  count(Doc_ID, Tokens) %>%
  cast_dfm(Doc_ID, Tokens, n)

# ---------------------------------------------------------
# 修正后的结构主题模型 (STM) 代码：数据严格对齐
# ---------------------------------------------------------

# 1. 重新构建 DFM 矩阵
dfm_counts <- text_data %>%
  mutate(Tokens = map(Text_Response, ~segment(.x, cutter))) %>%
  unnest(Tokens) %>%
  count(Doc_ID, Tokens) %>%
  filter(n > 1) %>% # 过滤低频词
  cast_dfm(Doc_ID, Tokens, n)

# 2. 【核心修正】：获取 DFM 中成功保留的 Doc_ID
valid_docs <- as.numeric(docnames(dfm_counts))

# 3. 对齐元数据：剔除空文本对应的样本
text_data_aligned <- text_data %>%
  filter(Doc_ID %in% valid_docs)

# 检查对齐结果（必须为 TRUE 才能继续）
print(paste("维度是否对齐:", ndoc(dfm_counts) == nrow(text_data_aligned)))

# 4. 重新拟合 STM 模型 (注意 data 参数已改为 text_data_aligned)
stm_model <- stm(dfm_counts, K = 4, prevalence = ~ AI_Anxiety, 
                 max.em.its = 100, data = text_data_aligned, init.type = "Spectral")

# 5. 后续评估与绘图 (同样使用对齐后的数据)
prep <- estimateEffect(1:4 ~ AI_Anxiety, stm_model, meta = text_data_aligned)
summary(prep)
pdf("Effect of AI Anxiety on Expressed Research Pain-points.pdf", width = 12, height = 8)

# 【核心修复】：强行撑开绘图边界
# 默认是 c(5, 4, 4, 2) + 0.1。我们把 左(第2个) 和 右(第4个) 参数加大到 15
par(mar = c(6, 15, 4, 15)) 

# 绘图
plot(prep, covariate = "AI_Anxiety", topics = c(1, 2, 3, 4),
     model = stm_model, method = "difference",
     cov.value1 = 5, cov.value2 = 1,
     xlab = "More likely in Low Anxiety <--------> More likely in High Anxiety",
     main = "Effect of AI Anxiety on Expressed Research Pain-points",
     labeltype = "custom", # 推荐：如果有自定义标签可以用这个
     cex = 1.2) # 稍微放大一下字体

dev.off() # 关闭图形设备


tiff("Figure_STM_Difference_HighRes.tiff", width = 12, height = 8, units = "in", res = 300, compression = "lzw")

# 同样的，必须在这里再次设置边界，因为 dev.off() 会重置参数
par(mar = c(6, 15, 4, 15)) 

plot(prep, covariate = "AI_Anxiety", topics = c(1, 2, 3, 4),
     model = stm_model, method = "difference",
     cov.value1 = 5, cov.value2 = 1,
     xlab = "More likely in Low Anxiety <--------> More likely in High Anxiety",
     main = "Effect of AI Anxiety on Expressed Research Pain-points",
     cex = 1.2)

dev.off() # 关闭图形设备

# 恢复R的默认边界设置（以防影响你画后续的图）
par(mar = c(5, 4, 4, 2) + 0.1)


# 比较高焦虑(>=4分)和低焦虑(<=2分)两类极端人群的字数差异
test_extreme <- text_tokens %>%
  filter(AI_Anxiety >= 4 | AI_Anxiety <= 2) %>%
  mutate(Anxiety_Group = ifelse(AI_Anxiety >= 4, "High", "Low"))

wilcox.test(Word_Count ~ Anxiety_Group, data = test_extreme)
