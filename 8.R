library(ggplot2)
library(dplyr)

# 1. 读取数据
data <- read.csv("data_valid_clean.csv")

# 2. 数据处理与转化
data_plot <- data %>%
  mutate(Usage_Depth = ifelse(Main_Task_Eng %in% c("Exp. Design", "Coding & Statistics", "Visualization"), 1, 0)) %>%
  mutate(AL_Group = ifelse(Mean_AL >= median(Mean_AL), "High Agentic Literacy", "Low Agentic Literacy")) %>%
  mutate(AL_Group = factor(AL_Group, levels = c("Low Agentic Literacy", "High Agentic Literacy"))) %>%
  group_by(AL_Group) %>%
  summarise(Deep_Usage_Percentage = mean(Usage_Depth) * 100)

# 3. 绘制 SCI 级学术条形图
p <- ggplot(data_plot, aes(x = AL_Group, y = Deep_Usage_Percentage, fill = AL_Group)) +
  # [修正点]：使用 linewidth 替代 size，避免底层 ffi_list2 调用冲突
  geom_bar(stat = "identity", width = 0.5, show.legend = FALSE, color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", Deep_Usage_Percentage)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Low Agentic Literacy" = "#B0BEC5", "High Agentic Literacy" = "#1E88E5")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(data_plot$Deep_Usage_Percentage) + 10)) +
  labs(
    x = "Agentic Literacy Level",
    y = "Proportion of Deep Usage (%)",
    title = "The Intermediate Coding Threshold in GenAI Adoption"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.ticks.length = unit(0.2, "cm")
  )

print(p)

# 4. 保存为高分辨率 TIFF 格式 (直接用于投稿)
ggsave("Figure_Agentic_Literacy_Threshold.tiff", plot = p, width = 6, height = 5, dpi = 300, compression = "lzw")
ggsave("Figure_Agentic_Literacy_Threshold.pdf", plot = p, width = 6, height = 5, dpi = 300)
