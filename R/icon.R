#' 绘制小林家的龙女仆同款兔子图标（ggplot完整对象）
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
  # 在 R 中计算点 A 绕点 B 旋转 C 度后的坐标
  rotate_point <- function(A, B=c(0,0), C, clockwise = FALSE) {
    # A: 向量 c(x, y)，待旋转点
    # B: 向量 c(x, y)，旋转中心
    # C: 角度（度），默认逆时针
    theta <- C * pi / 180
    if (clockwise) theta <- -theta
    dx <- A[1] - B[1]
    dy <- A[2] - B[2]
    x_new <- dx * cos(theta) - dy * sin(theta) + B[1]
    y_new <- dx * sin(theta) + dy * cos(theta) + B[2]
    c(x_new, y_new)
  }

  #脸部主体
  p_face<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+rotate_point(A=c(0*icon_size,0.5*icon_size),C=angle)[1],
                                       y0 = y0+rotate_point(A=c(0*icon_size,0.5*icon_size),C=angle)[2], # 椭圆中心
                              a = 1*icon_size, # 长轴
                              b = 0.65*icon_size, # 短轴
                              angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)

  #左耳
  p_leftEar<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+rotate_point(A=c((-0.5)*icon_size,1.5*icon_size),C=angle)[1],
                                       y0 = y0+rotate_point(A=c((-0.5)*icon_size,1.5*icon_size),C=angle)[2], # 椭圆中心
                                       a = 0.2*icon_size, # 长轴
                                       b = 0.6*icon_size, # 短轴
                                       angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)
  #右耳
  p_rightEar<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+rotate_point(A=c(0.5*icon_size,1.5*icon_size),C=angle)[1],
                                       y0 = y0+rotate_point(A=c(0.5*icon_size,1.5*icon_size),C=angle)[2],, # 椭圆中心
                                       a = 0.2*icon_size, # 长轴
                                       b = 0.6*icon_size, # 短轴
                                       angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)
  #底部左三角
  p_bottomLeftTriangle<-
    ggplot2::geom_polygon(ggplot2::aes(
      x=c(
        x0+rotate_point(A=c(0*icon_size,(-0.5)*icon_size),C=angle)[1],
        x0+rotate_point(A=c((-1)*icon_size,(0)*icon_size),C=angle)[1],
        x0+rotate_point(A=c((-1)*icon_size,(-1)*icon_size),C=angle)[1]
      ),y=c(
        y0+rotate_point(A=c(0*icon_size,(-0.5)*icon_size),C=angle)[2],
        y0+rotate_point(A=c((-1)*icon_size,(0)*icon_size),C=angle)[2],
        y0+rotate_point(A=c((-1)*icon_size,(-1)*icon_size),C=angle)[2]
      )
      ),
      fill = fill, color = linecolor, linewidth = 1.2)
  #底部右三角
  p_bottomRightTriangle<-
    ggplot2::geom_polygon(ggplot2::aes(
      x=c(
        x0+rotate_point(A=c(0*icon_size,(-0.5)*icon_size),C=angle)[1],
        x0+rotate_point(A=c((1)*icon_size,(0)*icon_size),C=angle)[1],
        x0+rotate_point(A=c((1)*icon_size,(-1)*icon_size),C=angle)[1]
      ),y=c(
        y0+rotate_point(A=c(0*icon_size,(-0.5)*icon_size),C=angle)[2],
        y0+rotate_point(A=c((1)*icon_size,(0)*icon_size),C=angle)[2],
        y0+rotate_point(A=c((1)*icon_size,(-1)*icon_size),C=angle)[2]
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



#' 绘制小林家的龙女仆同款兔子图标（ggplot图层）
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
#' @return 返回一个包含该兔子图案的 `ggplot` 图层。
#' @export
#'
#' @examples
#' # 绘制多个自定义兔子
#' ggplot2::ggplot()+icon_rabbit_layer(fill='lightskyblue',linecolor=NA,background=NA,icon_size=1,x0=(-2),y0=(0),angle=0)+icon_rabbit_layer(fill='darkgreen',linecolor=NA,background=NA,icon_size=2,x0=(5),y0=(0),angle=45)
icon_rabbit_layer<-function(
    fill,
    linecolor=NA,
    background,
    icon_size=1,
    x0=0,
    y0=0,
    angle
){
  # 在 R 中计算点 A 绕点 B 旋转 C 度后的坐标
  rotate_point <- function(A, B=c(0,0), C, clockwise = FALSE) {
    # A: 向量 c(x, y)，待旋转点
    # B: 向量 c(x, y)，旋转中心
    # C: 角度（度），默认逆时针
    theta <- C * pi / 180
    if (clockwise) theta <- -theta
    dx <- A[1] - B[1]
    dy <- A[2] - B[2]
    x_new <- dx * cos(theta) - dy * sin(theta) + B[1]
    y_new <- dx * sin(theta) + dy * cos(theta) + B[2]
    c(x_new, y_new)
  }

  #脸部主体
  p_face<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+rotate_point(A=c(0*icon_size,0.5*icon_size),C=angle)[1],
                                       y0 = y0+rotate_point(A=c(0*icon_size,0.5*icon_size),C=angle)[2], # 椭圆中心
                                       a = 1*icon_size, # 长轴
                                       b = 0.65*icon_size, # 短轴
                                       angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)

  #左耳
  p_leftEar<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+rotate_point(A=c((-0.5)*icon_size,1.5*icon_size),C=angle)[1],
                                       y0 = y0+rotate_point(A=c((-0.5)*icon_size,1.5*icon_size),C=angle)[2], # 椭圆中心
                                       a = 0.2*icon_size, # 长轴
                                       b = 0.6*icon_size, # 短轴
                                       angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)
  #右耳
  p_rightEar<-
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0+rotate_point(A=c(0.5*icon_size,1.5*icon_size),C=angle)[1],
                                       y0 = y0+rotate_point(A=c(0.5*icon_size,1.5*icon_size),C=angle)[2],, # 椭圆中心
                                       a = 0.2*icon_size, # 长轴
                                       b = 0.6*icon_size, # 短轴
                                       angle = (angle/180)*pi),
                          fill = fill,
                          color = linecolor, linewidth = 1.2)
  #底部左三角
  p_bottomLeftTriangle<-
    ggplot2::geom_polygon(ggplot2::aes(
      x=c(
        x0+rotate_point(A=c(0*icon_size,(-0.5)*icon_size),C=angle)[1],
        x0+rotate_point(A=c((-1)*icon_size,(0)*icon_size),C=angle)[1],
        x0+rotate_point(A=c((-1)*icon_size,(-1)*icon_size),C=angle)[1]
      ),y=c(
        y0+rotate_point(A=c(0*icon_size,(-0.5)*icon_size),C=angle)[2],
        y0+rotate_point(A=c((-1)*icon_size,(0)*icon_size),C=angle)[2],
        y0+rotate_point(A=c((-1)*icon_size,(-1)*icon_size),C=angle)[2]
      )
    ),
    fill = fill, color = linecolor, linewidth = 1.2)
  #底部右三角
  p_bottomRightTriangle<-
    ggplot2::geom_polygon(ggplot2::aes(
      x=c(
        x0+rotate_point(A=c(0*icon_size,(-0.5)*icon_size),C=angle)[1],
        x0+rotate_point(A=c((1)*icon_size,(0)*icon_size),C=angle)[1],
        x0+rotate_point(A=c((1)*icon_size,(-1)*icon_size),C=angle)[1]
      ),y=c(
        y0+rotate_point(A=c(0*icon_size,(-0.5)*icon_size),C=angle)[2],
        y0+rotate_point(A=c((1)*icon_size,(0)*icon_size),C=angle)[2],
        y0+rotate_point(A=c((1)*icon_size,(-1)*icon_size),C=angle)[2]
      )
    ),
    fill = fill, color = linecolor, linewidth = 1.2)

  #拼图
  Combined<-
    list(
    p_face,
    p_leftEar,p_rightEar,
    p_bottomLeftTriangle,p_bottomRightTriangle)

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
