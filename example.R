

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
  p1 <- ggplot2::ggplot(df_summary, aes(x = Sample, y = Mean, fill = Sample)) +
    # geom_col() 不需要指定 stat = "identity"
    ggplot2::geom_col(width = 0.6, color = "black", alpha = 0.8, show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = Maid_color('托尔')
    )+
    # 核心：设定误差棒的上下限 ymin 和 ymax
    ggplot2::geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
                  width = 0.2,      # 误差棒顶部横线的宽度
                  linewidth = 0.7,  # 线条粗细
                  color = "black") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = "5个样本的均值与误差棒图",
      x = "样本组别",
      y = "数值 (Mean ± Sd)"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))+
    showtext::showtextshowtext_auto()

  # 输出图表
  return(p1)
}


