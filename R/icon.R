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



#' 绘制小林送给托尔的围巾（龙图案）
#'
#' @description 绘制《小林家的龙女仆》中小林送给托尔的围巾上的龙图案，红色背景上有一条绿色龙纹。
#'
#' @return 返回一个包含该围巾龙图案的 ggplot 对象。
#' @export
#'
#' @examples
#' icon_dragon_scarf()
icon_dragon_scarf<-function(){

  # 巨大红色背景：围巾材质
  p_bg<-list(
    ggplot2::geom_rect(ggplot2::aes(xmin = -50, xmax = 17, ymax = 80, ymin = -30),
                       fill = 'indianred'))
  # 龙上方绿色横块
  p_greenAbove<-list(
    ggplot2::geom_rect(ggplot2::aes(xmin = -50, xmax = 17, ymax = 40+4*((3)^0.5), ymin = 40-4*((3)^0.5)),
                       fill = '#00CD00'),
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-50,xend=17,y=40-4*((3)^0.5),yend=40-4*((3)^0.5)),
                          color='darkgreen',linewidth=1),
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-50,xend=17,y=40,yend=40),
                          color='darkgreen',linewidth=1),
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-50,xend=17,y=40+4*((3)^0.5),yend=40+4*((3)^0.5)),
                          color='darkgreen',linewidth=1)
  )

  # 身子
  p_body<-list(
    ggplot2::geom_rect(ggplot2::aes(xmin = -50, xmax = -10, ymax = 2*((3)^0.5), ymin = -6*((3)^0.5)),
                       fill = '#00CD00'),
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-50,xend=-10,y=-4*((3)^0.5),yend=-4*((3)^0.5)),
                          color='darkgreen',linewidth=1))

  # 龙头基底背景【第二最难的部分】
  p_head_bg<-list(
    #大背景
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin = -10, xmax = 7+((3)^0.5)/2, ymax = 2*((3)^0.5), ymin = -6*((3)^0.5)),
                       fill = 'indianred'),
    #X轴上半部分背景和矩形
    ggplot2::geom_polygon(mapping=ggplot2::aes(x=c(0,0,-3,-3),y=c(0,2*((3)^0.5),2*((3)^0.5),0)),
                          fill = 'indianred'),
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin = -10, xmax = -3, ymax = 2*((3)^0.5), ymin = 0),
                       fill = '#00CD00'),
    #X轴上半部分圆角
    ggplot2::geom_polygon(mapping=ggplot2::aes(x=c(0,-1.5,-3,-3),y=c(0,3*((3)^0.5)/2,((3)^0.5),0)),
                          fill = '#00CD00'),
    ggforce::geom_circle(mapping=ggplot2::aes(x0 = -3, y0 = ((3)^0.5), r = ((3)^0.5)),
                         fill = "#00CD00",color=NA),
    #X轴下半部分圆角
    ggplot2::geom_polygon(mapping=ggplot2::aes(x=c(0,1.5,3,3,0),y=c(0,-3*((3)^0.5)/2,-((3)^0.5),-2*((3)^0.5),-2*((3)^0.5))),
                          fill = '#00CD00'),
    ggforce::geom_circle(mapping=ggplot2::aes(x0 = 3, y0 = -((3)^0.5), r = ((3)^0.5)),
                         fill = "indianred",color=NA),
    #突出的嘴巴部分
    ggplot2::geom_polygon(mapping=ggplot2::aes(x=c(0,0,-5,-5),y=c(0,-2*((3)^0.5),-2*((3)^0.5),0)),
                          fill = '#00CD00'),
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin = -10, xmax = 7, ymax = -2*((3)^0.5), ymin = -6*((3)^0.5)),
                       fill = '#00CD00'),
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin = 7, xmax = 7+((3)^0.5)/2, ymax = -2*((3)^0.5), ymin = -6*((3)^0.5)),
                       fill = 'indianred'),
    ggforce::geom_circle(mapping=ggplot2::aes(x0 = 7, y0 = -5*((3)^0.5)/2, r = ((3)^0.5)/2),
                         fill = "#00CD00",color=NA),
    ggforce::geom_circle(mapping=ggplot2::aes(x0 = 7, y0 = -11*((3)^0.5)/2, r = ((3)^0.5)/2),
                         fill = "#00CD00",color=NA),
    #嘴巴左侧补充的绿色部分
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin = 7, xmax = 7+((3)^0.5)/2, ymax = -5*((3)^0.5)/2, ymin = -11*((3)^0.5)/2),
                       fill = '#00CD00'),
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin = -10, xmax = -5, ymax = 2*((3)^0.5), ymin = -6*((3)^0.5)),
                       fill = '#00CD00'),
    ggplot2::coord_fixed())

  # 嘴巴纹路
  p_mouth_line<-list(
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-10,xend=0,y=-4*((3)^0.5),yend=-4*((3)^0.5)),
                          color='darkgreen',linewidth=1),
    ggplot2::geom_line(mapping=ggplot2::aes(x=seq((0),(7+((3)^0.5)/2),0.01),
                                            y=(sin(2*seq((0),(7+((3)^0.5)/2),0.01))-4*((3)^0.5))),
                       color='darkgreen',linewidth=1))

  # 眼睛（椭圆）
  p_eye<-list(
    ggforce::geom_ellipse(ggplot2::aes(x0 = -5,
                                       y0 = 0, # 椭圆中心
                                       a = 1.5, # 长轴
                                       b = 0.75, # 短轴
                                       angle = 0),
                          fill = 'lightgreen',color = 'darkgreen', linewidth=1))

  # 角【第三最难的部分】
  p_arc<-list(
    ggforce::geom_circle(mapping=ggplot2::aes(x0 = 0, y0 = 12, r = 14),
                         fill = "#00CD00",color='darkgreen',linewidth=1),
    ggforce::geom_circle(mapping=ggplot2::aes(x0 = 0, y0 = 12, r = 12),
                         fill = "indianred",color='darkgreen',linewidth=1),
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin = -25, xmax = 17, ymax = 30, ymin = 12),
                       fill = 'indianred'),
    ggplot2::geom_rect(mapping=ggplot2::aes(xmin = -8, xmax = 17, ymax = 12, ymin = 0),
                       fill = 'indianred')
  )

  # 龙的边缘描线【⚠️⚠️⚠️第一最难的部分！！！⚠️⚠️⚠️】
  p_dragon_line<-list(
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-50,xend=-(sqrt(40+48*sqrt(3))),y=2*sqrt(3),yend=2*sqrt(3)),
                          color='darkgreen',linewidth=1), #角左边的直线
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-12,xend=-14,y=12,yend=12),
                          color='darkgreen',linewidth=1), #角的最上边的直线
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-sqrt(48*sqrt(3)-12),xend=-3,y=2*sqrt(3),yend=2*sqrt(3)),
                          color='darkgreen',linewidth=1), #角右边的直线
    ggforce::geom_arc(ggplot2::aes(x0 = -3, y0 = sqrt(3), r = sqrt(3), start = 0, end = pi/3),
                      color = 'darkgreen', size = 1), #额头弧线
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-1.5,xend=0,y=1.5*sqrt(3),yend=0),
                          color='darkgreen',linewidth=1), #x轴上方鼻梁直线斜坡
    ggplot2::geom_segment(mapping=ggplot2::aes(x=0,xend=1.5,y=0,yend=-1.5*sqrt(3)),
                          color='darkgreen',linewidth=1), #x轴下方鼻梁直线斜坡
    ggforce::geom_arc(ggplot2::aes(x0 = 3, y0 = -sqrt(3), r = sqrt(3), start = pi, end = 4*pi/3),
                      color = 'darkgreen', size = 1), #鼻梁下方弧线
    ggplot2::geom_segment(mapping=ggplot2::aes(x=3,xend=7,y=-2*sqrt(3),yend=-2*sqrt(3)),
                          color='darkgreen',linewidth=1), #嘴巴上方直线
    ggforce::geom_arc(ggplot2::aes(x0 = (7), y0 = -5*sqrt(3)/2, r = sqrt(3)/2, start = 0, end = pi/2),
                      color = 'darkgreen', size = 1), #嘴巴突出部分上半弧线
    ggplot2::geom_segment(mapping=ggplot2::aes(x=(7+sqrt(3)/2),xend=(7+sqrt(3)/2),
                                               y=-5*sqrt(3)/2,yend=-11*sqrt(3)/2),
                          color='darkgreen',linewidth=1), #嘴巴右侧直线
    ggforce::geom_arc(ggplot2::aes(x0 = (7), y0 = -11*sqrt(3)/2, r = sqrt(3)/2, start = pi/2, end = pi),
                      color = 'darkgreen', size = 1), #嘴巴突出部分下半弧线
    ggplot2::geom_segment(mapping=ggplot2::aes(x=-50,xend=7,y=-6*sqrt(3),yend=-6*sqrt(3)),
                          color='darkgreen',linewidth=1) #龙最下方直线
  )

  # # 辅助线坐标系
  # xy<-list(
  #   geom_hline(yintercept = 0,color='black'),
  #   geom_vline(xintercept = 0,color='black')
  # )
  p_whole_dragon<-ggplot2::ggplot()+
    p_bg+
    p_greenAbove+
    p_arc+
    p_body+
    p_head_bg+
    p_mouth_line+
    p_eye+
    p_dragon_line+
    p_dragon_line+
    ggplot2::theme_void()
  return(p_whole_dragon)
}
