#' 创建角色像素风图层（仅ggplot图层）
#'
#' @description
#' 根据输入的角色名称（支持中文、日文或罗马音），提取其对应的像素风格数据集，
#' 并返回一个包含 `ggplot2` 图层、坐标系和主题设置的列表。
#' 该列表可直接叠加到现有 `ggplot` 对象上，或用于 `Q_pixel_plot()` 快速生成完整图像。
#'
#' @param name 角色名称，长度为1的字符向量。支持中文名（如 `"托尔"`）、
#'   日文名（如 `"トール"`）或罗马音（如 `"Tohru"`）。函数内部会自动检测语言并匹配角色。
#' @param bakcgroud 背景颜色，默认为 `NA`（透明背景）。可以传入颜色名或十六进制颜色码。
#' @param border 像素方块的边框颜色，默认为 `NA`（无边框）。可以传入颜色名或十六进制颜色码。
#'
#' @return 返回一个 `list`，包含以下元素：
#'   \itemize{
#'     \item `geom_tile()` 图层：使用角色像素数据的中心坐标、宽高和填充色绘制。
#'     \item `scale_fill_identity()`：保持原始颜色映射。
#'     \item `coord_fixed()`：固定坐标比例，防止图像变形。
#'     \item `labs()`、`theme_void()` 和自定义主题：去除轴标签、网格和图例，设置背景色。
#'   }
#' @export
#'
#' @examples
#' # 在空白画布上叠加托尔的像素图层
#' ggplot2::ggplot() +
#'   Q_pixel_layer("托尔", bakcgroud = "indianred", border = NA)
#'
#' # 叠加多个角色图层（需注意坐标可能重叠）
#' ggplot2::ggplot() +
#'   Q_pixel_layer("小林", bakcgroud = "gold") +
#'   Q_pixel_layer("托尔", bakcgroud = "indianred")
Q_pixel_layer <- function(name, bakcgroud = NA, border = NA) {
  # 1. 将输入名称转换为罗马音（去除空格以保证数据集命名一致）
  name_Romaji <- SearchCharactersNameInMultipleLanguge(name)$Romaji
  name_Romaji <- stringr::str_remove_all(name_Romaji,' ')

  # 2. 根据罗马音获取预定义的像素数据集（例如 Q_PixelData_Tohru）
  data <- get(paste0('Q_PixelData_', name_Romaji))

  # 3. 构建图层列表：geom_tile + 坐标/主题设置
  output <- list(
    # 核心：根据数据中的 x_center, y_center, width, height, fill 绘制色块
    ggplot2::geom_tile(
      data = data$data,
      ggplot2::aes(x = x_center, y = y_center,
                   width = width, height = height, fill = fill),
      color = border
    ),
    # 直接使用数据中的颜色值，不转换为因子
    ggplot2::scale_fill_identity(),
    # 固定 x/y 比例，使像素保持正方形
    ggplot2::coord_fixed(),
    # 移除轴标题
    ggplot2::labs(x = NULL, y = NULL),
    # 清空主题元素，仅保留背景颜色设置
    ggplot2::theme_void(),
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = bakcgroud, color = NA),
      legend.position = "none"
    )
  )
  return(output)
}




#' 绘制角色像素风头像（完整ggplot对象）
#'
#' @description
#' 快速生成指定角色的像素风格头像。该函数内部调用 `Q_pixel_layer()`，
#' 使用透明背景和无边框的默认设置绘制角色，返回一个可直接展示的 `ggplot` 对象。
#'
#' @param name 角色名称，支持中文、日文或罗马音。详见 `Q_pixel_layer()` 的 `name` 参数说明。
#'
#' @return 一个 `ggplot` 对象，包含该角色的像素头像。
#' @export
#'
#' @examples
#' # 绘制托尔的像素
#' Q_pixel_plot("托尔")
#'
#' # 使用罗马音绘制康娜的头像
#' Q_pixel_plot("Kanna Kamui")
Q_pixel_plot <- function(name, bakcgroud = NA, border = NA) {
  # 调用 Q_pixel_layer 生成图层列表，并用 ggplot() 初始化空画布
  return(
    ggplot2::ggplot() +
      Q_pixel_layer(name = name, bakcgroud = bakcgroud, border = border)
  )
}
