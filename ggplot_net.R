add_rink <- function() {
  red_line_segments <-
    tibble::tibble(
      x1 = 
        c(
          ## goal line/board intercept
          -36.75, -36.75,
          ## crease straight lines
          -4, 4, -4, 4, 
          ## goalie trapezoid
          11, 11, -11, -11,
          ## faceoff tickmarks
          rep(c(23, 21, -21, -23), each = 4),
          rep(c(-26, -21, 18, 23), 4),
          rep(
            c(
              -22 - sqrt(15 ^ 2 - 1.5 ^ 2) - 2, 
              -22 + sqrt(15 ^ 2 - 1.5 ^ 2), 
              22 - sqrt(15 ^ 2 - 1.5 ^ 2) - 2, 
              22 + sqrt(15 ^ 2 - 1.5 ^ 2)
            ), 
            4
          )
        ), 
      y1 = 
        c(
          ## goal line/board intercept
          89, -89, 
          ## crease straight lines
          89, 89, -89, -89, 
          ## goalie trapezoid
          89, -89, 89, -89,
          ## faceoff tickmarks
          rep(c(75, 67, -63, -71), 4), 
          rep(c(71, 67, -67, -71), each = 4),
          rep(c(72, 66, -66, -72), each = 4)
        ),
      x2 = 
        c(
          ## goal line/board intercept
          36.75, 36.75, 
          ## crease straight lines
          -4, 4, -4, 4, 
          ## goalie trapezoid
          14, 14, -14, -14,
          ## faceoff tickmarks
          rep(c(23, 21, -21, -23), each = 4),
          rep(c(-23, -18, 21, 26), 4),
          rep(
            c(
              -22 - sqrt(15 ^ 2 - 1.5 ^ 2), 
              -22 + sqrt(15 ^ 2 - 1.5 ^ 2) + 2, 
              22 - sqrt(15 ^ 2 - 1.5 ^ 2), 
              22 + sqrt(15 ^ 2 - 1.5 ^ 2) + 2
            ),
            4
          )
        ),
      y2 = 
        c(
          ## goal line/board intercept
          89, -89, 
          ## crease straight lines
          84.5, 84.5, -84.5, -84.5, 
          ## goalie trapezoid
          100, -100, 100, -100,
          ## faceoff tickmarks
          rep(c(71, 63, -67, -75), 4), 
          rep(c(71, 67, -67, -71), each = 4),
          rep(c(72, 66, -66, -72), each = 4)
        )
    )
  
  corners <-
    tibble::tibble(
      x0 = c(14.5, -14.5) %>% rep(each = 2),
      y0 = c(72, -72, -72, 72),
      r = 28,
      start = seq(0, 2 * pi, length.out = 5) %>% head(-1),
      end = seq(0, 2 * pi, length.out = 5) %>% tail(-1)
    )
  
  creases <-
    tibble::tibble(
      x0 = c(0, 0, 42.5),
      y0 = c(89, -89, 0),
      r = c(rep(sqrt(16 + 4.5 ^ 2), 2), 10),
      start = c(pi - atan(4/4.5), pi * 2 - atan(4/4.5), pi),
      end = c(pi +  atan(4/4.5), pi * 2 + atan(4/4.5), pi * 2)
    )
  
  dots <-
    tibble::tibble(
      x0 = c(22, -22) %>% rep(4),
      y0 = c(c(22, -22, 69, -69) %>% rep(each = 2))
    )
  
  circles <-
    tibble::tibble(
      x0 = c(0, 22, 22, -22, -22),
      y0 = c(0, 69, -69, 69, -69)
    )
  
  sides <- 
    tibble::tibble(
      x1 = c(42.5, -42.5, 14.5, 14.5),
      y1 = c(72, 72, 100, -100),
      x2 = c(42.5, -42.5, -14.5, -14.5),
      y2 = c(-72, -72, 100, -100)
    )
  
  nets <-
    tibble::tibble(
      xmin = -3,
      xmax = 3,
      ymin = c(89, -92.33),
      ymax = c(92.33, -89)
    )
  
  # ggplot2::ggplot() +
  list(
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = nets
    ),
    ggplot2::geom_segment(
      mapping = ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      data = red_line_segments,
      color = "red"
    ),
    ## center red line
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -42.5, xmax = 42.5, ymin = -0.5, ymax = 0.5),
      fill = "red"
    ),
    ## blue lines
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -42.5, xmax = 42.5, ymin = ymin, ymax = ymax),
      data = 
        tibble::tibble(
          ymin = c(25, -26),
          ymax = c(26, -25)
        ),
      fill = "blue"
    ),
    ggforce::geom_arc(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r, start = start, end = end),
      data = creases,
      color = "red"
    ),
    ## center dot
    ggforce::geom_circle(
      mapping = ggplot2::aes(x0 = 0, y0 = 0, r = 0.5),
      fill = "blue"
    ),
    ggforce::geom_circle(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = 1),
      data = dots,
      color = "red",
      fill = "red"
    ),
    ggforce::geom_circle(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = 15),
      data = circles,
      color = "red"
    ),
    ggplot2::geom_segment(
      mapping = ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      data = sides
    ),
    ggforce::geom_arc(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r, start = start, end = end),
      data = corners
    ), 
    ggplot2::coord_fixed(),
    ggplot2::theme_void()
  )
}
