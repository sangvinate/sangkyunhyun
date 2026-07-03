# 한국 휴머노이드 로봇 생산 예측 그래프
# South Korea's estimated footprint in humanoid robot production
# Source: Goldman Sachs Research

# 필요한 라이브러리 로드
library(ggplot2)
library(scales)

# 데이터 생성 (이미지에서 추출한 값)
robot_data <- data.frame(
  year = 2025:2035,
  units = c(
    0,      # 2025
    0,      # 2026
    20000,  # 2027
    30000,  # 2028
    50000,  # 2029
    75000,  # 2030
    110000, # 2031
    160000, # 2032
    210000, # 2033
    300000, # 2034
    410000  # 2035
  )
)

# 데이터 확인
print(robot_data)

# 그래프 생성
p <- ggplot(robot_data, aes(x = year, y = units)) +
  geom_bar(stat = "identity", fill = "#003366", width = 0.7) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 400000, 100000),
    limits = c(0, 450000),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = 2025:2035,
    labels = 2025:2035
  ) +
  labs(
    title = "South Korea's estimated footprint in humanoid robot production",
    subtitle = "Humanoid robots forecast to be produced using components from South Korean companies",
    x = "",
    y = "",
    caption = "Source: Goldman Sachs Research"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, hjust = 0, color = "gray30", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, hjust = 0, color = "gray50", margin = margin(t = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", size = 0.3),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.ticks.x = element_line(color = "gray50"),
    axis.ticks.y = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# 그래프 출력
print(p)

# 그래프 저장
ggsave("humanoid_robot_forecast.png", p, width = 10, height = 6, dpi = 300, bg = "white")

# 추가 통계
cat("\n=== 휴머노이드 로봇 생산 예측 통계 ===\n\n")
cat("2025년 예상:", format(robot_data$units[1], big.mark = ","), "대\n")
cat("2035년 예상:", format(robot_data$units[11], big.mark = ","), "대\n")
cat("총 증가량:", format(robot_data$units[11] - robot_data$units[1], big.mark = ","), "대\n")
cat("평균 연간 성장률:", round(((robot_data$units[11] / max(robot_data$units[1], 1))^(1/10) - 1) * 100, 1), "%\n")
cat("\n2025-2035 총 생산 예상량:", format(sum(robot_data$units), big.mark = ","), "대\n")

cat("\n그래프가 'humanoid_robot_forecast.png' 파일로 저장되었습니다.\n")
