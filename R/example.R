

#' 绘制带误差棒的示例柱状图
#'
#' @description
#' 生成一个包含 5 个样本均值与标准差 (SD) 的虚拟数据集，并使用 \code{ggplot2} 绘制带有误差棒的柱状图。
#' 图表使用了自定义主题色 \code{Maid_color('托尔')}，并启用了 \code{showtext} 以保证中文字体的正常渲染。
#'
#' @return 返回一个 \code{ggplot} 对象，显示带有误差棒的柱状图。
#'
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual geom_errorbar theme_minimal labs theme element_text
#' @importFrom showtext showtext_auto
#' @export
#'
#' @examples
#' # 绘制并查看示例柱状图
#' example_col()
example_col<-function(){
  # 1. 生成5个样本的汇总数据
  # Error 可以代表标准差(SD)或标准误(SE)
  df_summary <- data.frame(
    Sample = c("Sample A", "Sample B", "Sample C", "Sample D", "Sample E"),
    Mean   = c(15.4, 22.1, 12.8, 18.5, 25.3),
    Sd  = c(1.2, 1.8, 0.9, 1.4, 2.0)
  )

  # 2. 绘制带误差棒的柱状图
  p1 <- ggplot2::ggplot(df_summary, ggplot2::aes(x = Sample, y = Mean, fill = Sample)) +
    # geom_col() 不需要指定 stat = "identity"
    ggplot2::geom_col(width = 0.6, color = "black", alpha = 0.8, show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = Maid_color('托尔')
    )+
    # 核心：设定误差棒的上下限 ymin 和 ymax
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Mean - Sd, ymax = Mean + Sd),
                  width = 0.2,      # 误差棒顶部横线的宽度
                  linewidth = 0.7,  # 线条粗细
                  color = "black") +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = "Tohru Color Example Bar plot",
      x = "Sample",
      y = "Value (Mean ± Sd)"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))+
    showtext::showtext_auto()

  # 输出图表
  return(p1)
}



#' 绘制小林与托尔集合的示例韦恩图
#'
#' @description
#' 生成两个包含部分重叠元素的虚拟集合，分别代表“小林”与“托尔”，
#' 并使用 \code{ggvenn} 绘制韦恩图。填充颜色使用 \code{getColor_CP('小林')}
#' 获取的 CP 代表色。
#'
#' @return 返回一个 \code{ggplot} 对象，显示两个集合的韦恩图。
#'
#' @importFrom ggvenn ggvenn
#' @export
#'
#' @examples
#' # 绘制并查看示例韦恩图
#' example_venn()
example_venn<-function(){
  # 列表 a：包含 5 个特有元素 + 8 个与 b 共享的元素
  a <- c(paste0("A", 1:5), paste0("AB", 1:8))
  # 列表 b：包含 4 个特有元素 + 同样的 8 个共享元素
  b <- c(paste0("B", 1:4), paste0("AB", 1:8))

  # 转换为 ggvenn 所需的命名列表格式
  venn_data <- list(Set_Kobayashi = a, Set_Tohru = b)

  ggvenn::ggvenn(
    venn_data,                     # 数据
    fill_color = getColor_CP('小林'),
    stroke_size = 0.5,             # 边框粗细
    set_name_size = 4              # 集合名称字号
  )
}
