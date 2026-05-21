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
#' SearchCharactersNameInMultipleLanguge(
#'   Gender = "Female", Species = "Dragon", Language = "Chinese"
#' )
#'
#' # 查找所有人类男性角色的罗马音
#' SearchCharactersNameInMultipleLanguge(
#'   Gender = "Male", Species = "Human", Language = "Romaji"
#' )
SearchCharactersNameInMultipleLanguge <-
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

