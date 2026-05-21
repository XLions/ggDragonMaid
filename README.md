# ggDragonMaid
 
ggDragonMaid 是一个 R 语言扩展包，提供动画《小林家的龙女仆》中 12 位主要角色的经典配色方案，以及多语言（中文、日文、罗马音）的角色基本信息表。你可以将这些配色用于 ggplot2 图表，或为任何可视化作品增添一抹龙娘色彩。  
  
ggDragonMaid is an R package that delivers classic color palettes inspired by the 12 main characters from the anime Miss Kobayashi's Dragon Maid, along with a multilingual character dataset (Chinese, Japanese, Romaji). Use the palettes in ggplot2 or any R graphic to bring a touch of dragon-maid charm to your visualizations.  

## 📦 安装 / Installation
目前可以从 GitHub 安装开发版：  
You can install the development version from GitHub:  
```R
# install.packages("remotes")
remotes::install_github("XLions/ggDragonMaid")
```

## 🎨 快速开始 / Quick Start
### 1. 展示所有角色配色 / Show all Character Colors
使用 `Show_Maid_colors()` 展示所有角色配色的热图，并调整不同语言的坐标轴和标题标签。  
Use `Show_Maid_colors()` to display a heatmap of all character color schemes, and adjust the axis labels and title for different languages.
```R
ggDragonMaid::Show_Maid_colors(lang = 'Romaji')
```
### 2. 获取角色配色 / Get Character Colors
使用 `Maid_color()` 可以通过任意语言的角色名获取其经典 5 色调色板。  
Use `Maid_color()` to get the classic 5-color palette of any character by their name in any language.
```R
# 用中文名获取托尔的颜色 Get Tohru's colors using the Chinese name
# 【所有中文名均参考B站中配版翻译】[All Chinese names refer to the Bilibili Chinese-dubbed translation.]
ggDragonMaid::Maid_color("托尔", lang = "Chinese")
#> [1] "#414763" "#FFD761" "#4FA478" "#EE6166" "#FFFFFF"

# 用罗马音获取康娜的颜色 Get Kanna's colors using the Chinese name
ggDragonMaid::Maid_color("Kanna Kamui", lang = "Romaji")
#> [1] "#E5DEEC" "#F2A3BC" "#34385E" "#00A5DF" "#FFF2E8"
```

### 3. 角色信息检索 / Search Characters
通过性别和种族筛选角色，返回指定语言的名字列表：  
Filter characters by gender and race, returning a list of names in a specified language:
```R
# 查找所有女性龙族角色的日文名 Find the Japanese names of all female dragon characters
ggDragonMaid::SearchCharactersNameInMultipleLanguge(
  Gender = "Female",
  Species = "Dragon",
  Language = "Japanese"
)
#> [1] "トール"        "カンナ·カムイ" "エルマ"        "ルコア"        "イルル"
```
  
### 4. 完整数据表 / Full Dataset
直接获取 12 位角色的基本信息（中文名、日文名、罗马音、性别、种族）：  
Get the basic information of 12 characters directly (Chinese name, Japanese name, Romaji, gender, race):
```R
ggDragonMaid::WideDatasetCharacterNameInMultipleLanguge()
```
  
### 5. 原始配色数据 / Raw Color Data
返回所有角色的 5 色调色板数据框：  
Return a data frame of the 5-color palettes for all characters:
```R
head(ggDragonMaid::ColorDataSets_Classic())
```

### 🧑‍🎨 配色参考 / Color Reference
所有颜色均参考自萌娘百科《小林家的龙女仆》角色形象设定的图片。  
All colors are referenced from pictures of the character designs on [Moegirl Wiki](https://mzh.moegirl.org.cn/%E5%B0%8F%E6%9E%97%E5%AE%B6%E7%9A%84%E9%BE%99%E5%A5%B3%E4%BB%86).

---
Enjoy your dragon-maid palette! 🐉✨
享受你的龙娘调色板吧！



