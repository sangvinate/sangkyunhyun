# Word 문서 생성 스크립트
# R Markdown 파일을 Word 문서로 변환

# 필요한 패키지 설치 및 로드
if (!require("rmarkdown")) {
  install.packages("rmarkdown")
}
library(rmarkdown)

# 현재 작업 디렉토리 확인
cat("현재 작업 디렉토리:", getwd(), "\n\n")

# R Markdown 파일을 Word 문서로 변환
cat("그래프해설.Rmd 파일을 Word 문서로 변환 중...\n")

tryCatch({
  # Word 문서로 렌더링
  rmarkdown::render(
    input = "그래프해설.Rmd",
    output_format = "word_document",
    output_file = "그래프해설.docx",
    encoding = "UTF-8"
  )

  cat("\n성공! Word 문서가 생성되었습니다.\n")
  cat("파일명: 그래프해설.docx\n")
  cat("위치:", file.path(getwd(), "그래프해설.docx"), "\n")

}, error = function(e) {
  cat("\n오류 발생:", conditionMessage(e), "\n")
  cat("\n해결 방법:\n")
  cat("1. rmarkdown 패키지 설치: install.packages('rmarkdown')\n")
  cat("2. Pandoc 설치 확인 (R Markdown 변환에 필요)\n")
  cat("   - RStudio를 사용하면 자동으로 포함됨\n")
  cat("   - 또는 https://pandoc.org/installing.html 에서 다운로드\n")
})

# HTML 버전도 생성 (선택사항)
cat("\n\nHTML 버전도 생성하시겠습니까? (선택사항)\n")
cat("HTML 버전 생성 코드:\n")
cat("rmarkdown::render('그래프해설.Rmd', output_format = 'html_document', output_file = '그래프해설.html')\n")
