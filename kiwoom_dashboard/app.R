# ============================================================
# 키움증권 보유내역 대시보드 (조회 전용)
# ============================================================
# - 키움 REST API(https://openapi.kiwoom.com)를 사용합니다.
# - 접근토큰 발급(/oauth2/token)과 계좌평가잔고내역요청(kt00018)만
#   호출하며, 주문/매매 관련 API는 일절 호출하지 않습니다.
#
# 실행 방법:
#   shiny::runApp("kiwoom_dashboard")
#
# 앱키/시크릿키는 .Renviron 파일에 저장해 두면 자동으로 불러옵니다:
#   KIWOOM_APP_KEY=발급받은앱키
#   KIWOOM_SECRET_KEY=발급받은시크릿키
# ============================================================

library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(scales)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ------------------------------------------------------------
# 1. API 호출 함수
# ------------------------------------------------------------

KIWOOM_HOSTS <- c(
  "실전투자" = "https://api.kiwoom.com",
  "모의투자" = "https://mockapi.kiwoom.com"
)

# 키움 응답의 숫자 문자열("000012345", "-1234", "+70300")을 숫자로 변환
knum <- function(x) {
  if (is.null(x)) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

# 접근토큰 발급 (au10001)
kiwoom_get_token <- function(host, appkey, secretkey) {
  res <- POST(
    url = paste0(host, "/oauth2/token"),
    add_headers(`Content-Type` = "application/json;charset=UTF-8"),
    body = toJSON(list(
      grant_type = "client_credentials",
      appkey     = appkey,
      secretkey  = secretkey
    ), auto_unbox = TRUE)
  )
  out <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  if (!is.null(out$return_code) && out$return_code != 0) {
    stop("토큰 발급 실패: ", out$return_msg %||% "알 수 없는 오류")
  }
  if (is.null(out$token)) stop("토큰 발급 실패: 응답에 token이 없습니다.")
  out  # token, expires_dt(yyyymmddHHMMSS), token_type
}

# 계좌평가잔고내역요청 (kt00018) - 연속조회 포함 전체 보유종목 수집
kiwoom_get_balance <- function(host, token) {
  cont_yn  <- "N"
  next_key <- ""
  summary  <- NULL
  pages    <- list()

  repeat {
    res <- POST(
      url = paste0(host, "/api/dostk/acnt"),
      add_headers(
        `Content-Type` = "application/json;charset=UTF-8",
        authorization  = paste("Bearer", token),
        `api-id`       = "kt00018",
        `cont-yn`      = cont_yn,
        `next-key`     = next_key
      ),
      body = toJSON(list(
        qry_tp       = "1",    # 1: 합산, 2: 개별
        dmst_stex_tp = "KRX"   # 국내거래소구분
      ), auto_unbox = TRUE)
    )
    out <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
    if (!is.null(out$return_code) && out$return_code != 0) {
      stop("잔고 조회 실패: ", out$return_msg %||% "알 수 없는 오류")
    }
    if (is.null(summary)) summary <- out

    lst <- out$acnt_evlt_remn_indv_tot
    if (!is.null(lst) && NROW(lst) > 0) {
      pages[[length(pages) + 1]] <- as.data.frame(lst, stringsAsFactors = FALSE)
    }

    hdr <- headers(res)
    if (identical(hdr[["cont-yn"]], "Y") && nzchar(hdr[["next-key"]] %||% "")) {
      cont_yn  <- "Y"
      next_key <- hdr[["next-key"]]
    } else {
      break
    }
  }

  raw <- if (length(pages) > 0) do.call(rbind, pages) else data.frame()

  col <- function(nm) if (nm %in% names(raw)) raw[[nm]] else rep(NA, NROW(raw))

  holdings <- if (NROW(raw) > 0) {
    data.frame(
      종목코드   = sub("^A", "", as.character(col("stk_cd"))),
      종목명     = as.character(col("stk_nm")),
      보유수량   = knum(col("rmnd_qty")),
      매입가     = knum(col("pur_pric")),
      현재가     = knum(col("cur_prc")),
      매입금액   = knum(col("pur_amt")),
      평가금액   = knum(col("evlt_amt")),
      평가손익   = knum(col("evltv_prft")),
      수익률     = knum(col("prft_rt")),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame()
  }

  # 보유비중(%)은 평가금액 합계 기준으로 직접 계산
  if (NROW(holdings) > 0) {
    tot <- sum(holdings$평가금액, na.rm = TRUE)
    holdings$비중 <- if (tot > 0) round(holdings$평가금액 / tot * 100, 2) else NA
    holdings <- holdings[order(-holdings$평가금액), ]
  }

  list(
    총매입금액   = knum(summary$tot_pur_amt),
    총평가금액   = knum(summary$tot_evlt_amt),
    총평가손익   = knum(summary$tot_evlt_pl),
    총수익률     = knum(summary$tot_prft_rt),
    추정예탁자산 = knum(summary$prsm_dpst_aset_amt),
    보유종목     = holdings,
    조회시각     = Sys.time()
  )
}

# ------------------------------------------------------------
# 2. 색상 (한국 관례: 이익 = 빨강, 손실 = 파랑)
# ------------------------------------------------------------
COL_GAIN    <- "#d64550"
COL_LOSS    <- "#3b6fd4"
COL_NEUTRAL <- "#868e96"
COL_BAR     <- "#4e79a7"

pl_color <- function(x) {
  ifelse(is.na(x), COL_NEUTRAL, ifelse(x > 0, COL_GAIN, ifelse(x < 0, COL_LOSS, COL_NEUTRAL)))
}

won <- function(x) ifelse(is.na(x), "-", paste0(format(round(x), big.mark = ","), "원"))
pct <- function(x) ifelse(is.na(x), "-", paste0(format(round(x, 2), nsmall = 2), "%"))

value_box <- function(title, value, color = "#212529") {
  div(
    style = paste0(
      "flex:1; min-width:170px; background:#ffffff; border:1px solid #e9ecef;",
      "border-radius:10px; padding:14px 18px; margin:6px;"
    ),
    div(style = "font-size:13px; color:#868e96;", title),
    div(style = paste0("font-size:21px; font-weight:700; margin-top:4px; color:", color, ";"), value)
  )
}

# ------------------------------------------------------------
# 3. UI
# ------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f4f6f9; }
    .well { background:#ffffff; border:1px solid #e9ecef; border-radius:10px; }
    h3 { font-weight:700; }
    .chart-card { background:#ffffff; border:1px solid #e9ecef; border-radius:10px;
                  padding:16px; margin-bottom:16px; }
  "))),

  titlePanel("키움증권 보유내역 대시보드"),
  div(style = "color:#868e96; margin-bottom:12px;",
      "조회 전용 대시보드입니다. 주문/매매 API는 호출하지 않습니다."),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("env", "접속 환경",
                   choices = names(KIWOOM_HOSTS), selected = "실전투자"),
      textInput("appkey", "App Key",
                value = Sys.getenv("KIWOOM_APP_KEY")),
      passwordInput("secretkey", "Secret Key",
                    value = Sys.getenv("KIWOOM_SECRET_KEY")),
      actionButton("refresh", "보유내역 조회", class = "btn-primary",
                   style = "width:100%; margin-top:8px;"),
      checkboxInput("auto", "자동 새로고침 (60초)", value = FALSE),
      hr(),
      div(style = "font-size:12px; color:#868e96;",
          "앱키/시크릿키를 .Renviron에 저장해 두면 자동으로 입력됩니다.",
          tags$br(), tags$br(),
          textOutput("last_updated"))
    ),

    mainPanel(
      width = 9,
      div(style = "display:flex; flex-wrap:wrap; margin:-6px;",
          uiOutput("boxes")),
      br(),
      div(class = "chart-card",
          h4("보유종목 상세"),
          DTOutput("holdings_table")),
      fluidRow(
        column(6, div(class = "chart-card",
                      h4("보유비중 (평가금액 기준)"),
                      plotOutput("weight_plot", height = "360px"))),
        column(6, div(class = "chart-card",
                      h4("종목별 평가손익"),
                      plotOutput("pl_plot", height = "360px")))
      )
    )
  )
)

# ------------------------------------------------------------
# 4. Server
# ------------------------------------------------------------
server <- function(input, output, session) {

  token_cache <- reactiveVal(NULL)   # list(host, appkey, token, expires)
  balance     <- reactiveVal(NULL)

  get_valid_token <- function(host, appkey, secretkey) {
    tc <- isolate(token_cache())
    now <- Sys.time()
    if (!is.null(tc) && tc$host == host && tc$appkey == appkey &&
        !is.na(tc$expires) && tc$expires > now + 60) {
      return(tc$token)
    }
    tok <- kiwoom_get_token(host, appkey, secretkey)
    expires <- suppressWarnings(as.POSIXct(as.character(tok$expires_dt),
                                           format = "%Y%m%d%H%M%S",
                                           tz = "Asia/Seoul"))
    if (length(expires) == 0 || is.na(expires)) expires <- now + 3600
    token_cache(list(host = host, appkey = appkey,
                     token = tok$token,
                     expires = expires))
    tok$token
  }

  do_refresh <- function() {
    appkey    <- trimws(input$appkey)
    secretkey <- trimws(input$secretkey)
    if (!nzchar(appkey) || !nzchar(secretkey)) {
      showNotification("App Key와 Secret Key를 입력하세요.", type = "warning")
      return(invisible(NULL))
    }
    host <- KIWOOM_HOSTS[[input$env]]
    withProgress(message = "보유내역 조회 중...", value = 0.5, {
      result <- tryCatch({
        token <- get_valid_token(host, appkey, secretkey)
        kiwoom_get_balance(host, token)
      }, error = function(e) {
        token_cache(NULL)  # 인증 오류일 수 있으니 토큰 캐시 초기화
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      })
      if (!is.null(result)) balance(result)
    })
  }

  observeEvent(input$refresh, do_refresh())

  # 자동 새로고침 (60초)
  observe({
    req(isTRUE(input$auto))
    invalidateLater(60000, session)
    if (!is.null(isolate(balance()))) do_refresh()
  })

  output$last_updated <- renderText({
    b <- balance()
    if (is.null(b)) "아직 조회하지 않았습니다."
    else paste0("최근 조회: ", format(b$조회시각, "%Y-%m-%d %H:%M:%S"))
  })

  output$boxes <- renderUI({
    b <- balance()
    if (is.null(b)) {
      return(div(style = "color:#868e96; padding:20px;",
                 "좌측에서 키를 입력하고 [보유내역 조회] 버튼을 누르세요."))
    }
    tagList(
      value_box("추정예탁자산", won(b$추정예탁자산)),
      value_box("총매입금액",   won(b$총매입금액)),
      value_box("총평가금액",   won(b$총평가금액)),
      value_box("총평가손익",   won(b$총평가손익), color = pl_color(b$총평가손익)),
      value_box("총수익률",     pct(b$총수익률),   color = pl_color(b$총수익률))
    )
  })

  output$holdings_table <- renderDT({
    b <- balance()
    req(b)
    h <- b$보유종목
    if (NROW(h) == 0) {
      return(datatable(data.frame(안내 = "보유종목이 없습니다."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    datatable(
      h,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        dom = "Bfrtip",
        buttons = list(list(extend = "csv", text = "CSV 저장",
                            filename = paste0("보유내역_", format(Sys.Date(), "%Y%m%d")))),
        language = list(
          search = "검색:", lengthMenu = "_MENU_ 개씩 보기",
          info = "총 _TOTAL_개 중 _START_-_END_",
          infoEmpty = "데이터 없음", zeroRecords = "검색 결과가 없습니다",
          paginate = list(first = "처음", last = "마지막",
                          `next` = "다음", previous = "이전")
        )
      )
    ) |>
      formatCurrency(c("매입가", "현재가", "매입금액", "평가금액", "평가손익"),
                     currency = "", interval = 3, mark = ",", digits = 0) |>
      formatCurrency("보유수량", currency = "", interval = 3, mark = ",", digits = 0) |>
      formatString("수익률", suffix = "%") |>
      formatString("비중", suffix = "%") |>
      formatStyle("평가손익",
                  color = styleInterval(c(-1e-9, 1e-9),
                                        c(COL_LOSS, COL_NEUTRAL, COL_GAIN)),
                  fontWeight = "bold") |>
      formatStyle("수익률",
                  color = styleInterval(c(-1e-9, 1e-9),
                                        c(COL_LOSS, COL_NEUTRAL, COL_GAIN)),
                  fontWeight = "bold")
  })

  # 보유비중: 단일 색상 가로 막대 (상위 15개 + 기타)
  output$weight_plot <- renderPlot({
    b <- balance()
    req(b, NROW(b$보유종목) > 0)
    h <- b$보유종목[!is.na(b$보유종목$평가금액), ]
    req(NROW(h) > 0)
    if (NROW(h) > 15) {
      top   <- h[1:15, ]
      other <- data.frame(종목명 = "기타",
                          평가금액 = sum(h$평가금액[-(1:15)], na.rm = TRUE))
      h <- rbind(top[, c("종목명", "평가금액")], other)
    }
    h$종목명 <- factor(h$종목명, levels = rev(h$종목명))
    ggplot(h, aes(x = 평가금액, y = 종목명)) +
      geom_col(fill = COL_BAR, width = 0.62) +
      geom_text(aes(label = paste0(" ", comma(round(평가금액)))),
                hjust = 0, size = 3.4, color = "#495057") +
      scale_x_continuous(labels = comma,
                         expand = expansion(mult = c(0, 0.22))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank())
  }, res = 96)

  # 종목별 평가손익: 다이버징 (이익 빨강 / 손실 파랑)
  output$pl_plot <- renderPlot({
    b <- balance()
    req(b, NROW(b$보유종목) > 0)
    h <- b$보유종목[!is.na(b$보유종목$평가손익), ]
    req(NROW(h) > 0)
    h <- h[order(-h$평가손익), ]
    if (NROW(h) > 20) h <- rbind(head(h, 10), tail(h, 10))
    h$종목명 <- factor(h$종목명, levels = rev(h$종목명))
    h$구분 <- ifelse(h$평가손익 >= 0, "이익", "손실")
    ggplot(h, aes(x = 평가손익, y = 종목명, fill = 구분)) +
      geom_col(width = 0.62, show.legend = FALSE) +
      geom_vline(xintercept = 0, color = "#adb5bd") +
      scale_fill_manual(values = c("이익" = COL_GAIN, "손실" = COL_LOSS)) +
      scale_x_continuous(labels = comma) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank())
  }, res = 96)
}

shinyApp(ui, server)
