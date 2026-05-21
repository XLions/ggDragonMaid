#' 宽表：多语言角色基本信息
#'
#' 返回《小林家的龙女仆》中 12 位出场人物的基本信息表，
#' 包含中文名、日文名、罗马音、性别与种族。
#'
#' @return 一个包含 12 行 5 列的 data.frame，列名依次为：
#'   \code{ChineseName}, \code{JapaneseName}, \code{Romaji},
#'   \code{Gender}, \code{Species}。
#' @export
#'
#' @examples
#' WideDatasetCharacterNameInMultipleLanguge()
WideDatasetCharacterNameInMultipleLanguge<-function(){
  return(
    # 《小林家的龙女仆》出场人物统计表
    data.frame(
      ChineseName = c(
        "小林", "托尔", "康娜卡姆依", "艾露玛", "尔科亚",
        "法夫纳", "才川理子", "真土翔太", "泷谷真", "伊露露",
        "才川乔吉", "会田竹人"
      ),
      JapaneseName = c(
        "小林", "トール", "カンナ·カムイ", "エルマ", "ルコア",
        "ファフニール", "才川 リコ", "真土 翔太", "滝谷 真", "イルル",
        "才川 ジョージー", "会田 竹人"
      ),
      Romaji = c(
        "Kobayashi", "Tohru", "Kanna Kamui", "Elma", "Lucoa",
        "Fafnir", "Saikawa Riko", "Magatsuchi Shouta", "Takiya Makoto", "Ilulu",
        "Saikawa Georgie", "Aida Taketo"
      ),
      Gender = c(
        "Female", "Female", "Female", "Female", "Female",
        "Male", "Female", "Male", "Male", "Female",
        "Female", "Male"
      ),
      Species = c(
        "Human", "Dragon", "Dragon", "Dragon", "Dragon",
        "Dragon", "Human", "Human", "Human", "Dragon",
        "Human", "Human"
      )
    )
  )
}




#' 按性别与种族搜索角色多语言名称
#'
#' 从角色信息表中按性别和种族筛选角色，并以指定语言返回角色名列表。
#'
#' @param Gender 性别筛选条件，允许 \code{"Male"}（男）或 \code{"Female"}（女）。
#'   默认同时包含两者。
#' @param Species 种族筛选条件，允许 \code{"Dragon"}（龙）或 \code{"Human"}（人类）。
#'   默认同时包含两者。
#' @param Language 返回结果的语言，可选 \code{"Chinese"}（中文名）、
#'   \code{"Japanese"}（日文名）或 \code{"Romaji"}（罗马音）。
#'
#' @return 字符向量，包含符合条件的角色名称。若无匹配则返回零长度向量并给出警告。
#' @export
#'
#' @examples
#' # 查找所有龙族女性角色的中文名
#' SearchCharactersNameInDetails(
#'   Gender = "Female", Species = "Dragon", Language = "Chinese"
#' )
#'
#' # 查找所有人类男性角色的罗马音
#' SearchCharactersNameInDetails(
#'   Gender = "Male", Species = "Human", Language = "Romaji"
#' )
SearchCharactersNameInDetails <-
  function(Gender = c('Male','Female'),
           Species = c('Dragon','Human'),
           Language = c("Chinese", "Japanese", "Romaji")) {
    # 载入数据
    characters_df <- data.frame(WideDatasetCharacterNameInMultipleLanguge())
    # 匹配参数，忽略大小写
    Language <- match.arg(Language)
    # 转换为小写以便比较
    gender_lower <- tolower(Gender)
    species_lower <- tolower(Species)

    # 筛选
    idx <- tolower(characters_df$Gender) == gender_lower &
      tolower(characters_df$Species) == species_lower

    # 根据Language选择输出列
    if (Language == "Chinese") {
      result <- characters_df$ChineseName[idx]
    } else if (Language == "Japanese") {
      result <- characters_df$JapaneseName[idx]
    } else if (Language == "Romaji") {
      result <- characters_df$Romaji[idx]
    }

    if (length(result) == 0) {
      warning("No matching characters found.")
      return(character(0))
    } else {
      return(result)
    }
  }




#' 按名称搜索角色多语言信息
#'
#' 输入一个角色名（中文、日文或罗马音），函数自动检测其语言，
#' 并在《小林家的龙女仆》角色宽表中查找匹配的记录，返回完整的行信息。
#'
#' @param name 长度为1的字符向量，待搜索的角色名称。
#'   支持中文名（如 \code{"托尔"}）、日文名（如 \code{"トール"}）或罗马音（如 \code{"Tohru"}）。
#'
#' @return 一个包含1行5列的 \code{data.frame}（若找到匹配），列名同
#'   \code{WideDatasetCharacterNameInMultipleLanguge()} 的输出。
#'   若无匹配或语言无法识别，则返回空数据框（0行5列）并给出警告。
#' @export
#'
#' @examples
#' # 用中文名搜索
#' SearchCharactersNameInMultipleLanguge("康娜卡姆依")
#'
#' # 用日文名搜索
#' SearchCharactersNameInMultipleLanguge("エルマ")
#'
#' # 用罗马音搜索（不区分大小写）
#' SearchCharactersNameInMultipleLanguge("tohru")
SearchCharactersNameInMultipleLanguge <- function(name) {
  # 输入检查
  if (!is.character(name) || length(name) != 1 || is.na(name) || nchar(name) == 0) {
    warning("请输入一个非空且长度为1的字符型名字。")
    return(WideDatasetCharacterNameInMultipleLanguge()[0, ])
  }

  # 获取完整数据
  df <- WideDatasetCharacterNameInMultipleLanguge()

  # 检测语言
  lang <- detect_language(name)

  # 根据语言选择对应的列名
  col_map <- c(
    Chinese  = "ChineseName",
    Japanese = "JapaneseName",
    Romaji   = "Romaji"
  )

  if (!lang %in% names(col_map)) {
    warning(sprintf("无法识别的语言类型：%s。仅接受中文、日文或罗马音输入。", lang))
    return(df[0, ])
  }

  target_col <- col_map[[lang]]

  # 构建筛选逻辑（罗马音忽略大小写）
  if (lang == "Romaji") {
    matched <- tolower(df[[target_col]]) == tolower(name)
  } else {
    matched <- df[[target_col]] == name
  }

  # 返回结果
  result <- df[matched, , drop = FALSE]
  if (nrow(result) == 0) {
    warning(sprintf("未找到名称为 \"%s\" 的角色。", name))
  }

  return(result)
}




#' 搜索角色CP的罗马音名称
#'
#' 根据输入的任意语言（中文、日文或罗马音）角色名，在预设的CP配对表中查找
#' 其对应的配对角色，并返回该角色的罗马音。CP配对基于《小林家的龙女仆》中的
#' 主要官配关系：托尔 ↔ 小林、康娜卡姆依 ↔ 才川理子、法夫纳 ↔ 泷谷真、
#' 尔科亚 ↔ 真土翔太、依露露 ↔ 会田竹人。
#'
#' @param name 长度为1的字符向量，待搜索的角色名称。支持中文名（如 \code{"托尔"}）、
#' 日文名（如 \code{"トール"}）或罗马音（如 \code{"Tohru"}）。要求输入的
#' 角色必须存在于角色信息表中，否则将引发错误。
#'
#' @return 一个字符串，表示配对角色的罗马音。例如输入 \code{"托尔"} 返回 \code{"Kobayashi"}，
#' 输入 \code{"小林"} 返回 \code{"Tohru"}。
#' @export
#'
#' @examples
#' # 通过中文名查找CP
#' SearchCP("托尔")
#'
#' # 通过罗马音查找CP
#' SearchCP("Kobayashi")
#'
#' # 日文名也可以
#' SearchCP("カンナ·カムイ")
SearchCP<-function(name){
  CP_df<-
    data.frame(
      #CP中的龙
      Dragon=c('托尔','康娜卡姆依','法夫纳','尔科亚','依露露'),
      #CP中的人
      Human=c('小林','才川理子','泷谷真','真土翔太','会田竹人')
    )
  inputCharacter<-
    SearchCharactersNameInMultipleLanguge(name)
  if(inputCharacter$Species=='Human'){
    return(
      SearchCharactersNameInMultipleLanguge(
      CP_df$Dragon[which(CP_df$Human==inputCharacter$ChineseName)])$Romaji
    )
  }else if(inputCharacter$Species=='Dragon'){
    return(
      SearchCharactersNameInMultipleLanguge(
      CP_df$Human[which(CP_df$Dragon==inputCharacter$ChineseName)])$Romaji
    )
  }
}


