# ggDragonMaid
 
ggDragonMaid 是一个 R 语言扩展包，提供动画《小林家的龙女仆》中 12 位主要角色的经典配色方案，以及多语言（中文、日文、罗马音）的角色基本信息表。你可以将这些配色用于 ggplot2 图表，或为任何可视化作品增添一抹龙女仆色彩。  
  
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

### 3. 获取CP组合代表色 / Get CP Pair Colors  
根据任意语言的角色名，返回其所属CP组合的两种代表颜色（人类角色在前，龙族角色在后）。  
Get the two representative colors of the CP pair for any given character (human character's color first, dragon's color second).  
```R
# 获取小林&托尔CP的代表色
ggDragonMaid::getColor_CP("小林")
#> [1] "#E07A7A" "#FFD761"

# 使用日文名获取康娜CP的代表色
ggDragonMaid::getColor_CP("カンナ·カムイ")
#> [1] "#76D296" "#F2A3BC"
```

### 4. 兔子图标 / Rabiit Icon
兔子图标多次出现在动画中。小林和托尔的杯子上的兔子图标是出现频率最高的。  
The rabbit icon appears multiple times throughout the anime. The rabbit icons on Kobayashi's and Tohru's cups are the most frequently seen ones.  
```R
icon_rabbit_Tohru() # 绘制托尔的兔子图标：红底金色
icon_rabbit_Kobayashi() # 绘制小林的兔子图标：金底红色

# 自定义的兔子图标（完整ggplot对象）
icon_rabbit_detail(
    fill,
    linecolor=NA,
    background,
    icon_size=1,
    x0=0,
    y0=0,
    angle
)
# 自定义的兔子图标（仅图层）
icon_rabbit_layer(
    fill,
    linecolor=NA,
    background,
    icon_size=1,
    x0=0,
    y0=0,
    angle
)
```

## 💻 信息检索 / Information Search
### 1. 角色信息检索 / Search Characters
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
  
### 2. 原始配色数据 / Raw Color Data
返回所有角色的 5 色调色板数据框：  
Return a data frame of the 5-color palettes for all characters:
```R
head(ggDragonMaid::ColorDataSets_Classic())
```

### 3. 内置示例图表 / Built‑in Example Plots
包内提供了两个示例函数，直观展示如何将角色配色应用到`ggplot2`图表中：  
The package includes two example functions to demonstrate how to apply character colors in`ggplot2`plots:  
```R
# 柱状图示例：托尔主题颜色 Bar plot Example: Tohru theme
ggDragonMaid::example_col()

# 韦恩图示例：小林&托尔CP颜色 Venn Example: Kobayashi & Tohru CP colors
ggDragonMaid::example_venn()
```

### 🧑‍🎨 配色参考 / Color Reference
所有颜色均参考自萌娘百科《小林家的龙女仆》角色形象设定的图片。  
All colors are referenced from pictures of the character designs on [Moegirl Wiki](https://mzh.moegirl.org.cn/%E5%B0%8F%E6%9E%97%E5%AE%B6%E7%9A%84%E9%BE%99%E5%A5%B3%E4%BB%86).

---
Enjoy your dragon-maid palette! 🐉✨
享受你的龙女仆调色板吧！



