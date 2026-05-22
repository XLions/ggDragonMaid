#' 绘制小林家的龙女仆同款兔子图标（底层函数）
#'
#' @description 这是一个底层函数，用于通过 `ggplot2` 和 `ggforce` 绘制《小林家的龙女仆》中，小林和托尔同款马克杯上的兔子图案。
#'
#' @param fill 兔子主体部分的填充颜色 (例如: 'gold', 'indianred', '#FFFFFF')。
#' @param linecolor 图形边框的颜色。如果不需要边框，可以使用 `NA`。
#' @param background 背景颜色。
#' @param icon_size 图标的整体缩放比例，默认为 1。
#' @param x0 图标中心的 X 轴坐标，默认为 0。
#' @param y0 图标中心的 Y 轴坐标，默认为 0。
#' @param angle 图标的旋转角度。
#'
#' @return 返回一个包含该兔子图案的 `ggplot` 对象。
#' @export
#'
#' @examples
#' # 绘制一个蓝色的自定义兔子
#' icon_rabbit_detail(fill="lightblue", linecolor="black", background="white", angle=0)
icon_rabbit_detail<-function(
    fill,
    linecolor=NA,
    background,
    icon_size=1,
    x0=0,
    y0=0,
    angle
){

  #脸部主体
  p_face<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+0, y0 = y0+0.5, # 椭圆中心
                              a = 1*icon_size, # 长轴
                              b = 0.65*icon_size, # 短轴
                              angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)

  #左耳
  p_leftEar<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+(-0.5), y0 = y0+1.5, # 椭圆中心
                                       a = 0.2*icon_size, # 长轴
                                       b = 0.6*icon_size, # 短轴
                                       angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)
  #右耳
  p_rightEar<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+(0.5), y0 = y0+1.5, # 椭圆中心
                                       a = 0.2*icon_size, # 长轴
                                       b = 0.6*icon_size, # 短轴
                                       angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)
  #底部左三角
  p_bottomLeftTriangle<-
    ggplot2::geom_polygon(ggplot2::aes(
      x=c(
        x0+0*icon_size,
        x0+(-1)*icon_size,
        x0+(-1)*icon_size
      ),y=c(
        y0+(-0.5)*icon_size,
        y0+0*icon_size,
        y0+(-1)*icon_size
      )
      ),
      fill = fill, color = linecolor, linewidth = 1.2)
  #底部右三角
  p_bottomRightTriangle<-
    ggplot2::geom_polygon(ggplot2::aes(
      x=c(
        x0+0*icon_size,
        x0+(1)*icon_size,
        x0+(1)*icon_size
      ),y=c(
        y0+(-0.5)*icon_size,
        y0+0*icon_size,
        y0+(-1)*icon_size
      )
    ),
    fill = fill, color = linecolor, linewidth = 1.2)

  #拼图
  Combined<-
  ggplot2::ggplot()+
    p_face+
    p_leftEar+p_rightEar+
    p_bottomLeftTriangle+p_bottomRightTriangle+
    ggplot2::theme_void()+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = background), # 面板背景
      panel.grid = ggplot2::element_blank() # 去除所有网格线
    )+
    ggplot2::coord_fixed()

  #输出
  return(Combined)
}




#' 绘制托尔款兔子图标
#'
#' @description 绘制托尔同款马克杯上的兔子图案（金色兔子，印度红背景）。
#'
#' @return 返回一个包含托尔款兔子图案的 `ggplot` 对象。
#' @export
#'
#' @examples
#' icon_rabbit_Tohru()
icon_rabbit_Tohru<-function(){
  return(
    icon_rabbit_detail(
      fill='gold',
      linecolor=NA,
      background='indianred',
      icon_size=1,
      x0=0,
      y0=0,
      angle=0
    )
  )
}




#' 绘制小林款兔子图标
#'
#' @description 绘制小林同款马克杯上的兔子图案（印度红兔子，金色背景）。
#'
#' @return 返回一个包含小林款兔子图案的 `ggplot` 对象。
#' @export
#'
#' @examples
#' icon_rabbit_Kobayashi()
icon_rabbit_Kobayashi<-function(){
  return(
    icon_rabbit_detail(
      fill='indianred',
      linecolor=NA,
      background='gold',
      icon_size=1,
      x0=0,
      y0=0,
      angle=0
    )
  )
}
