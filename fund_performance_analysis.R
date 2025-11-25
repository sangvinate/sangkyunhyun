# 펀드 성과 분석 프로그램
# 종목별 매매손익 및 평가손익 계산 및 시각화

# 필요한 라이브러리 로드
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# 작업 디렉토리 설정 (사용자 환경에 맞게 수정)
setwd("C:/Users/infomax/OneDrive/문서/fund")

# 1. 데이터 파일 읽기
# 파일명은 실제 파일명에 맞게 수정하세요
cat("데이터 파일을 읽는 중...\n")

# 기준가 추이 데이터
nav_data <- read.csv("기준가추이.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 보유내역 데이터
holdings_1031 <- read.csv("펀드보유내역_20251031.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
holdings_1124 <- read.csv("펀드보유내역_20251124.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 매매내역 데이터
trading_data <- read.csv("주식매매내역_20251101_20251124.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)

cat("데이터 로딩 완료!\n\n")

# 2. 매매손익 계산
cat("매매손익 계산 중...\n")

# 매매내역에서 매수/매도 분리
# 컬럼명은 실제 데이터에 맞게 조정 필요
# 가정: 종목명, 매매구분(매수/매도), 수량, 단가, 금액 컬럼 존재
trading_summary <- trading_data %>%
  group_by(종목명, 매매구분) %>%
  summarise(
    총수량 = sum(수량, na.rm = TRUE),
    총금액 = sum(금액, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = 매매구분,
    values_from = c(총수량, 총금액),
    values_fill = 0
  )

# 매매손익 = 매도금액 - 매수금액
trading_summary <- trading_summary %>%
  mutate(
    매매손익 = ifelse(exists("총금액_매도", where = .), 총금액_매도, 0) -
               ifelse(exists("총금액_매수", where = .), 총금액_매수, 0)
  )

# 3. 평가손익 계산
cat("평가손익 계산 중...\n")

# 10/31과 11/24 보유내역 비교
# 컬럼명은 실제 데이터에 맞게 조정 필요
# 가정: 종목명, 보유수량, 평가금액 컬럼 존재

holdings_comparison <- holdings_1031 %>%
  select(종목명, 평가금액) %>%
  rename(평가금액_1031 = 평가금액) %>%
  full_join(
    holdings_1124 %>% select(종목명, 평가금액) %>% rename(평가금액_1124 = 평가금액),
    by = "종목명"
  ) %>%
  replace_na(list(평가금액_1031 = 0, 평가금액_1124 = 0))

# 매매로 인한 순증감 계산
net_trading <- trading_summary %>%
  select(종목명, 매매구분_순증감 = 총금액_매수) %>%
  mutate(매매구분_순증감 = ifelse(exists("총금액_매도", where = trading_summary),
                                    총금액_매수 - 총금액_매도, 총금액_매수))

# 평가손익 = (11/24 평가금액 - 10/31 평가금액) - 순매수금액
# 더 정확한 계산: 11/24 평가금액 - (10/31 평가금액 + 순매수금액 - 순매도금액)
holdings_comparison <- holdings_comparison %>%
  left_join(
    trading_summary %>%
      mutate(
        순매수금액 = ifelse(exists("총금액_매수", where = .), 총금액_매수, 0) -
                      ifelse(exists("총금액_매도", where = .), 총금액_매도, 0)
      ) %>%
      select(종목명, 순매수금액),
    by = "종목명"
  ) %>%
  replace_na(list(순매수금액 = 0)) %>%
  mutate(
    평가손익 = 평가금액_1124 - 평가금액_1031 - 순매수금액
  )

# 4. 종목별 손익 통합
cat("종목별 손익 통합 중...\n")

# 매매손익과 평가손익 통합
total_profit_loss <- holdings_comparison %>%
  select(종목명, 평가손익) %>%
  full_join(
    trading_summary %>% select(종목명, 매매손익),
    by = "종목명"
  ) %>%
  replace_na(list(평가손익 = 0, 매매손익 = 0)) %>%
  mutate(
    총손익 = 평가손익 + 매매손익
  ) %>%
  arrange(desc(총손익))

# 결과 출력
cat("\n=== 종목별 손익 분석 결과 ===\n\n")
print(total_profit_loss, n = 20)

cat("\n총 매매손익:", sum(total_profit_loss$매매손익, na.rm = TRUE), "\n")
cat("총 평가손익:", sum(total_profit_loss$평가손익, na.rm = TRUE), "\n")
cat("총 손익:", sum(total_profit_loss$총손익, na.rm = TRUE), "\n\n")

# 5. 막대그래프 시각화
cat("막대그래프 생성 중...\n")

# 데이터를 long format으로 변환
plot_data <- total_profit_loss %>%
  select(종목명, 매매손익, 평가손익) %>%
  pivot_longer(
    cols = c(매매손익, 평가손익),
    names_to = "손익구분",
    values_to = "금액"
  ) %>%
  filter(abs(금액) > 0)  # 0이 아닌 값만 표시

# 상위/하위 종목만 표시 (너무 많을 경우)
top_stocks <- total_profit_loss %>%
  filter(abs(총손익) > 0) %>%
  arrange(desc(abs(총손익))) %>%
  head(20) %>%
  pull(종목명)

plot_data_filtered <- plot_data %>%
  filter(종목명 %in% top_stocks)

# 그래프 1: 종목별 매매손익 및 평가손익 (누적 막대그래프)
p1 <- ggplot(plot_data_filtered, aes(x = reorder(종목명, 금액), y = 금액, fill = 손익구분)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("매매손익" = "#2E86AB", "평가손익" = "#A23B72")) +
  labs(
    title = "종목별 매매손익 및 평가손익 분석",
    subtitle = paste0("기간: 2025.11.01 ~ 2025.11.24 (상위 ", min(20, length(top_stocks)), "개 종목)"),
    x = "종목명",
    y = "손익 (원)",
    fill = "손익 구분"
  ) +
  theme_minimal(base_family = "") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::comma)

print(p1)

# 그래프 저장
ggsave("종목별_손익_분석_누적.png", p1, width = 12, height = 8, dpi = 300)

# 그래프 2: 종목별 매매손익 및 평가손익 (병렬 막대그래프)
p2 <- ggplot(plot_data_filtered, aes(x = reorder(종목명, 금액), y = 금액, fill = 손익구분)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("매매손익" = "#2E86AB", "평가손익" = "#A23B72")) +
  labs(
    title = "종목별 매매손익 및 평가손익 비교",
    subtitle = paste0("기간: 2025.11.01 ~ 2025.11.24 (상위 ", min(20, length(top_stocks)), "개 종목)"),
    x = "종목명",
    y = "손익 (원)",
    fill = "손익 구분"
  ) +
  theme_minimal(base_family = "") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::comma)

print(p2)

# 그래프 저장
ggsave("종목별_손익_분석_병렬.png", p2, width = 12, height = 8, dpi = 300)

# 그래프 3: 총손익 막대그래프 (손익 크기순)
p3 <- ggplot(total_profit_loss %>% filter(종목명 %in% top_stocks),
             aes(x = reorder(종목명, 총손익), y = 총손익, fill = 총손익 > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#06D6A0", "FALSE" = "#EF476F"),
                    labels = c("손실", "이익")) +
  labs(
    title = "종목별 총손익 (매매손익 + 평가손익)",
    subtitle = paste0("기간: 2025.11.01 ~ 2025.11.24 (상위 ", min(20, length(top_stocks)), "개 종목)"),
    x = "종목명",
    y = "총손익 (원)",
    fill = "손익"
  ) +
  theme_minimal(base_family = "") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::comma)

print(p3)

# 그래프 저장
ggsave("종목별_총손익.png", p3, width = 12, height = 8, dpi = 300)

cat("\n분석 완료! 그래프가 저장되었습니다.\n")
cat("- 종목별_손익_분석_누적.png\n")
cat("- 종목별_손익_분석_병렬.png\n")
cat("- 종목별_총손익.png\n")

# 결과를 CSV로 저장
write.csv(total_profit_loss, "종목별_손익_분석_결과.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("- 종목별_손익_분석_결과.csv\n")
