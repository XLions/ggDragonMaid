#' 检测文本的主要语言（中文、日文或英文）
#'
#' @param text 字符向量，待检测的文本
#' @return 与输入等长的字符向量，取值为 "Chinese", "Japanese", "Romaji", "Other" 或 NA
#' @examples
#' detect_language(c("你好", "こんにちは", "Hello", "123", NA))
detect_language <- function(text) {
  if (!is.character(text)) stop("输入必须是字符向量。")

  sapply(text, function(t) {
    if (is.na(t) || nchar(t) == 0) return(NA_character_)

    # 1. 检测日文：存在平假名或片假名
    has_kana <- grepl("[\\p{Hiragana}\\p{Katakana}]", t, perl = TRUE)
    if (has_kana) return("Japanese")

    # 2. 检测中文：存在汉字
    has_han <- grepl("\\p{Han}", t, perl = TRUE)
    if (has_han) return("Chinese")

    # 3. 检测英文：拉丁字母在非空白字符中占比 > 0.5
    letters_only <- gsub("[^A-Za-z]", "", t)
    non_space <- gsub("\\s", "", t)
    if (nchar(non_space) > 0 && nchar(letters_only) / nchar(non_space) > 0.5) {
      return("Romaji")
    }

    # 4. 其他情况
    return("Other")
  }, USE.NAMES = FALSE)
}
