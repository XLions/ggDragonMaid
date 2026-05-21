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




#' 绘制《小林家的龙女仆》角色主题色热图
#'
#' @description
#' 根据输入指定的语言（中文、日文或罗马音），生成一个展示各个角色经典配色（Hex色值）的热图。
#' 颜色将按照调色板顺序从左到右排列，左侧 Y 轴为对应语言的角色名称。
#'
#' @param lang 字符串，指定显示的语言。可选值为 \code{"Chinese"} (中文),
#' \code{"Japanese"} (日文), 或 \code{"Romaji"} (罗马音)。默认为 \code{"Chinese"}。
#'
#' @return 返回一个 \code{ggplot} 对象，显示角色主题色的热图。
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_identity scale_x_continuous theme_minimal labs theme element_blank element_text
#' @export
#'
#' @examples
#' # 绘制中文角色名热图
#' Show_Maid_colors(lang = "Chinese")
#'
#' # 绘制日文角色名热图
#' Show_Maid_colors(lang = "Japanese")
Show_Maid_colors <-
  function(lang = c("Chinese", "Japanese", "Romaji")) {
  # 验证输入参数，默认取第一个有效值
  lang <- match.arg(lang)

  # 1. 载入内置的两个数据集
  char_data <- WideDatasetCharacterNameInMultipleLanguge()
  color_data <- ColorDataSets_Classic()

  # 2. 为颜色数据添加序列号，用于热图的 X 轴 (1到5)
  # 使用 ave 函数按 name 分组生成序号，不依赖 dplyr
  color_data$color_index <- ave(integer(nrow(color_data)), color_data$name, FUN = seq_along)

  # 3. 合并数据
  # color_data 的 'name' 列对应 char_data 的 'Romaji' 列
  merged_data <- merge(color_data, char_data, by.x = "name", by.y = "Romaji", all.x = TRUE)

  # 4. 根据输入的语言选择要在 Y 轴上显示的列名，并提取对应的列数据
  y_col <- switch(lang,
                  "Chinese" = "ChineseName",
                  "Japanese" = "JapaneseName",
                  "Romaji" = "name")

  # 为了让 Y 轴按照原始数据集的顺序排列（第一个角色在热图最上方），需要将其转换为因子并反转 levels
  original_col_name <- switch(lang,
                              "Chinese" = "ChineseName",
                              "Japanese" = "JapaneseName",
                              "Romaji" = "Romaji")
  name_levels <- rev(char_data[[original_col_name]])
  merged_data[[y_col]] <- factor(merged_data[[y_col]], levels = name_levels)

  # 5. 确保 ggplot2 可用 (在包开发中通过 Imports 声明即可)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install 'ggplot2' to use this function.")
  }

  # 6. 设置对应语言的绘图
  plot_labels <- switch(lang,
                        "Chinese" = list(
                          # 使用 expression(italic("text") * "text") 来拼接斜体和正体
                          title = expression(italic("《小林家的龙女仆》") * " 角色经典配色"),
                          subtitle = "当前显示语言: 中文",
                          x = "颜色顺位",
                          y = "角色名称"
                        ),
                        "Japanese" = list(
                          title = expression(italic("小林さんちのメイドラゴン") * " キャラクター定番配色"),
                          subtitle = "現在の表示言語: 日本語",
                          x = "色の順位",
                          y = "キャラクター名"
                        ),
                        "Romaji" = list(
                          title = expression(italic("Miss Kobayashi's Dragon Maid") * " Character Classic Colors"),
                          subtitle = "Current Language: Romaji",
                          x = "Color Order",
                          y = "Character Name"
                        )
  )

  # 7. 绘制热图
  # 使用 .data[[]] 语法来动态调用列名，这可以避免 R CMD check 出现 "no visible binding for global variable" 的警告
  p <- ggplot2::ggplot(merged_data, ggplot2::aes(x = color_index, y = .data[[y_col]], fill = color)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +  # 添加白色边框分割色块
    ggplot2::scale_fill_identity() +                        # 核心：让 ggplot2 直接解析 Hex 颜色字符串而不是将其当作分类变量
    ggplot2::scale_x_continuous(breaks = 1:5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = plot_labels$x,
      y = plot_labels$y,
      title = plot_labels$title,
      subtitle = plot_labels$subtitle
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),              # 移除网格线使热图更干净
      axis.text.y = ggplot2::element_text(size = 11, face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10))
    )+
    showtext::showtext_auto()

  return(p)
}




#' 获取角色CP组合的代表颜色
#'
#' 根据输入的角色名，在预设的CP配对表中查找该角色所属的CP组，
#' 并返回该CP组对应的两种代表颜色。颜色顺序为：人类角色的颜色在前，
#' 龙族角色的颜色在后。
#'
#' @param name 长度为1的字符向量，角色名称。支持中文名（如 \code{"小林"}）、
#'   日文名（如 \code{"小林"}）或罗马音（如 \code{"Kobayashi"}）。
#'
#' @return 长度为2的字符向量，包含两个十六进制颜色码。第一个颜色对应人类角色，
#'   第二个颜色对应龙族角色。
#' @export
#'
#' @examples
#' # 获取小林和托尔这对CP的代表颜色
#' getColor_CP("小林")
#'
#' # 使用罗马音获取
#' getColor_CP("Kanna Kamui")
getColor_CP<-function(name){
  CP_color_df<-
    data.frame(
      Character=c(
        '小林','才川理子','泷谷真','真土翔太','会田竹人',
        '托尔','康娜卡姆依','法夫纳','尔科亚','依露露'
      ),
      CP=paste0('CP',rep(as.character(1:5),2)),
      Color=c(
        "#E07A7A","#76D296","#FFFFFF","#9F73CE","#373C41",
        "#FFD761","#F2A3BC","#585864","#E5EA6D","#E7465F"
      )
    )
  name_Chinese<-SearchCharactersNameInMultipleLanguge(name)$ChineseName
  CPNo_Name<-CP_color_df$CP[which(CP_color_df$Character==name_Chinese)]
  return(
    CP_color_df$Color[which(CP_color_df$CP==CPNo_Name)] #输出的颜色人在前龙在后
  )
}
