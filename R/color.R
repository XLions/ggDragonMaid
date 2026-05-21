#' 获取《小林家的龙女仆》经典配色数据
#'
#' 返回一个包含所有登场角色经典服装配色信息的数据框。
#' 每位角色对应 5 种颜色，来源于萌娘百科的角色形象设定。
#'
#' @return 一个 \code{data.frame}，包含两列：
#'   \describe{
#'     \item{\code{name}}{角色罗马音名称，因子类型，共 12 个水平，每个水平重复 5 次。}
#'     \item{\code{color}}{对应的十六进制颜色码，字符型。}
#'   }
#' @export
#'
#' @examples
#' head(ColorDataSets_Classic())
ColorDataSets_Classic<-function(){
  return(
    data.frame(
      name=rep(
        c(
          "Kobayashi", "Tohru", "Kanna Kamui", "Elma", "Lucoa",
          "Fafnir", "Saikawa Riko", "Magatsuchi Shouta", "Takiya Makoto", "Ilulu",
          "Saikawa Georgie", "Aida Taketo"
        ),
        each=5
      ),
      # 颜色参考图片来源：小林家的龙女仆 - 萌娘百科 万物皆可萌的百科全书
      # https://mzh.moegirl.org.cn/%E5%B0%8F%E6%9E%97%E5%AE%B6%E7%9A%84%E9%BE%99%E5%A5%B3%E4%BB%86
      color=c(
        "#E07A7A", "#D6B033", "#5A656B", "#FFFFFF", "#8B5938", # 小林经典社畜装
        "#414763", "#FFD761", "#4FA478", "#EE6166", "#FFFFFF", # 托尔经典女仆装
        "#E5DEEC", "#F2A3BC", "#34385E", "#00A5DF", "#FFF2E8", # 康娜经典哥特萝莉
        "#2E303D", "#C8BAE5", "#416FD5", "#9D8464", "#FFE1CD", # 艾露玛战斗龙
        "#E5EA6D", "#FBA4B0", "#494A5C", "#688BCA", "#B98561", # 尔科亚经典痴女装
        "#292A2C", "#585864", "#FFFFFF", "#C12B2B", "#8D7A55", # 法夫纳经典执事管家装
        "#9E6746", "#76D296", "#FB8E69", "#FCEEE3", "#49A9A3", # 才川理子经典小学生装
        "#9F73CE", "#64C6D1", "#5D8951", "#F9E0D3", "#276EAD", # 真土翔太经典小学生装
        "#222325", "#FFFFFF", "#417BAF", "#6D7C91", "#68482E", # 泷谷经典社畜装
        "#EE6D8B", "#4F4C62", "#F8CE70", "#FFF2E7", "#E7465F", # 依露露经典皮肤
        "#D48344", "#9C5E45", "#F4D787", "#F7F7F7", "#5A9F4D", # 才川乔吉经典女仆装
        "#373C41", "#574231", "#F3DAC5", "#537492", "#CDA94F"  # 会田竹人经典DK装
      )
    )
  )
}

#' 提取角色配色向量
#'
#' 根据角色名称（罗马音）从经典配色数据中提取对应的颜色向量。
#' 支持精确匹配、不区分大小写的匹配，以及唯一部分匹配。
#'
#' @param name 角色名，字符串，接受罗马音（如 \code{"Tohru"}）。
#' @param theme 配色主题，目前仅支持 \code{"classic"}（经典配色）。
#' @return 一个长度为 5 的字符向量，包含十六进制颜色码。
#' @export
#'
#' @examples
#' # 获取托尔的经典配色
#' Maid_colors("Tohru")
#'
#' # 不区分大小写
#' Maid_colors("kobayashi")
#'
#' # 唯一的部分匹配
#' Maid_colors("Kanna")
#'
#' \dontrun{
#' # 会引发错误：未找到角色
#' Maid_colors("Hikari")
#' }
Maid_colors_Romaji <- function(name, theme = "classic") {
  # 标准化输入
  name <- trimws(name)         # 去除首尾空格

  # 根据主题选择数据源（可扩展）
  theme <- match.arg(theme, choices = c("classic"))
  color_df <- switch(
    theme,
    classic = ColorDataSets_Classic()
  )

  # 尝试精确匹配
  idx <- which(color_df$name == name)
  if (length(idx) > 0) {
    return(color_df$color[idx])
  }

  # 若未精确匹配，尝试不区分大小写匹配
  idx <- which(tolower(color_df$name) == tolower(name))
  if (length(idx) > 0) {
    return(color_df$color[idx])
  }

  # 若仍无结果，尝试部分匹配（仅当唯一时返回）
  matches <- grep(name, color_df$name, ignore.case = TRUE, fixed = TRUE)
  if (length(matches) == 1) {
    return(color_df$color[matches])
  } else if (length(matches) > 1) {
    stop("名称 '", name, "' 匹配到多个角色: ",
         paste(color_df$name[matches], collapse = ", "),
         "。请提供更精确的名称。")
  }

  # 完全找不到
  stop("未找到对应角色: ", name)
}


#' 根据角色名与语言获取配色向量
#'
#' 输入任意语言（中文、日文或罗马音）的角色名，先将其转换为罗马音，
#' 再提取对应角色的经典配色（5种颜色）。
#'
#' @param name 角色名，字符串。语言种类由 \code{lang} 指定。
#' @param lang 输入名称的语言，可选 \code{"Chinese"}（中文）、
#'   \code{"Japanese"}（日文）或 \code{"Romaji"}（罗马音）。
#' @param theme 配色主题，目前仅支持 \code{"classic"}（经典配色），
#'   默认 \code{"classic"}。
#'
#' @return 长度为 5 的字符向量，包含十六进制颜色码。
#' @export
#'
#' @examples
#' # 通过中文名获取托尔的配色
#' Maid_color("托尔", lang = "Chinese")
#'
#' # 通过日文名获取康娜的配色
#' Maid_color("カンナ·カムイ", lang = "Japanese")
#'
#' # 通过罗马音获取艾露玛的配色
#' Maid_color("Elma", lang = "Romaji")
#'
#' \dontrun{
#' # 未找到角色时抛出错误
#' Maid_color("小美", lang = "Chinese")
#' }
Maid_color <- function(name, theme = "classic") {

  # 获取对应罗马音名称
  name_clean<-SearchCharactersNameInMultipleLanguge(name)$Romaji

  if (length(name_clean) == 0) {
    stop("未找到对应角色: ", name)
  }
  if (length(name_clean) > 1) {
    # 理论上不应发生（角色名唯一），但保留安全检查
    stop("名称 '", name, "' 匹配到多个角色，请联系数据维护者。")
  }

  # 利用罗马音获取配色向量
  return(Maid_colors_Romaji(name = name_clean, theme = theme))
}
