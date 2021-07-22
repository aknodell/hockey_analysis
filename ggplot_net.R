add_rink <- function(direction = "vertical", zones = c("off", "def")) {
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
          ),
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
          rep(c(72, 66, -66, -72), each = 4),
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
          ),
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
          rep(c(72, 66, -66, -72), each = 4),
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
      dir = rep(c("vertical", "horizontal"), each = 58)
    )

  corners <-
    tibble::tibble(
      x0 = c(c(14.5, -14.5) %>% rep(each = 2), 72, 72, -72, -72),
      y0 = c(72, -72, -72, 72, 14.5, -14.5, -14.5, 14.5),
      r = 28,
      start = seq(0, 2 * pi, length.out = 5) %>% head(-1) %>% rep(2),
      end = seq(0, 2 * pi, length.out = 5) %>% tail(-1) %>% rep(2),
      dir = rep(c("vertical", "horizontal"), each = 4)
    )

  creases <-
    tibble::tibble(
      x0 = c(0, 0, 0, 0, 42.5, 0, 0, 89, -89, 0),
      y0 = c(0, 0, 89, -89, 0, 0, 0, 0, 0, -42.5),
      r = c(15, 15, rep(sqrt(16 + 4.5 ^ 2), 2), 10) %>% rep(2),
      start = c(0, pi, pi - atan(4/4.5), pi * 2 - atan(4/4.5), pi) %>% c(. + (0.5 * pi)),
      end = c(pi, 2 * pi, pi +  atan(4/4.5), pi * 2 + atan(4/4.5), pi * 2) %>% c(. + (0.5 * pi)),
      dir = rep(c("vertical", "horizontal"), each = 5)
    )

  dots <-
    tibble::tibble(
      x0 = c(c(22, -22) %>% rep(4), c(22, -22, 69, -69) %>% rep(each = 2)),
      y0 = c(c(22, -22, 69, -69) %>% rep(each = 2), c(22, -22) %>% rep(4)),
      r = 1,
      dir = rep(c("vertical", "horizontal"), each = 8)
    )

  circles <-
    tibble::tibble(
      x0 = c(22, 22, -22, -22, 69, -69, 69, -69),
      y0 = c(69, -69, 69, -69, 22, 22, -22, -22),
      r = 15,
      dir = rep(c("vertical", "horizontal"), each = 4)
    )

  sides <-
    tibble::tibble(
      x1 = c(42.5, 42.5, -42.5, -42.5, 14.5, 14.5, 72, 0.5, 72, 0.5, 100, -100),
      y1 = c(72, 0.5, 72, 0.5, 100, -100, 42.5, 42.5, -42.5, -42.5, 14.5, 14.5),
      x2 = c(42.5, 42.5, -42.5, -42.5, -14.5, -14.5, -0.5, -72, -0.5, -72, 100, -100),
      y2 = c(-0.5, -72, -0.5, -72, 100, -100, 42.5, 42.5, -42.5, -42.5, -14.5, -14.5),
      dir = rep(c("vertical", "horizontal"), each = 6)
    )

  nets <-
    tibble::tibble(
      xmin = c(-3, -3, 89, -92.33),
      xmax = c(3, 3, 92.33, -89),
      ymin = xmin %>% rev(),
      ymax = xmax %>% rev(),
      dir = rep(c("vertical", "horizontal"), each = 2)
    )

  center_line <-
    tibble::tibble(
      xmin = c(-42.5, -0.5),
      xmax = c(42.5, 0.5),
      ymin = c(-0.5, -42.5),
      ymax = c(0.5, 42.5),
      dir = c("vertical", "horizontal")
    )

  blue_lines <-
    tibble::tibble(
      xmin = c(-42.5, -42.5, -26, 25),
      xmax = c(42.5, 42.5, -25, 26),
      ymin = xmin %>% rev(),
      ymax = xmax %>% rev(),
      dir = c("vertical", "horizontal") %>% rep(each = 2)
    )

  center_dot <-
    tibble::tibble(
      x0 = 0,
      y0 = 0,
      r = 0.5
    )

  # ggplot2::ggplot() +
  list(
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = 
        nets %>% 
        dplyr::filter(dir == direction)
    ),
    ggplot2::geom_segment(
      mapping = ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      data = red_line_segments %>% dplyr::filter(dir == direction),
      color = "red"
    ),
    ## center red line
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = center_line %>% dplyr::filter(dir == direction),
      fill = "red"
    ),
    ## blue lines
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = blue_lines %>% dplyr::filter(dir == direction),
      fill = "blue"
    ),
    ggforce::geom_arc(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r, start = start, end = end),
      data = creases %>% dplyr::filter(dir == direction),
      color = "red"
    ),
    ## center dot
    ggforce::geom_circle(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
      data = center_dot,
      color = "blue",
      fill = "blue"
    ),
    ggforce::geom_circle(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
      data = dots %>% dplyr::filter(dir == direction),
      color = "red",
      fill = "red"
    ),
    ggforce::geom_circle(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r),
      data = circles %>% dplyr::filter(dir == direction),
      color = "red"
    ),
    ggplot2::geom_segment(
      mapping = ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      data = sides %>% dplyr::filter(dir == direction)
    ),
    ggforce::geom_arc(
      mapping = ggplot2::aes(x0 = x0, y0 = y0, r = r, start = start, end = end),
      data = corners %>% dplyr::filter(dir == direction)
    ),
    ggplot2::coord_fixed(),
    ggplot2::theme_void(),
    ggplot2::scale_x_continuous(
      limits = 
        c(
          ifelse(
            direction == "vertical",
            -42.5,
            ifelse(
              "def" %in% zones,
              -100,
              -0.5
            )
          ),
          ifelse(
            direction == "vertical",
            42.5,
            ifelse(
              "off" %in% zones,
              100,
              0.5
            )
          )
        )
    ),
    ggplot2::scale_y_continuous(
      limits = 
        c(
          ifelse(
            direction == "horizontal",
            -42.5,
            ifelse(
              "def" %in% zones,
              -100,
              -0.5
            )
          ),
          ifelse(
            direction == "horizontal",
            42.5,
            ifelse(
              "off" %in% zones,
              100,
              0.5
            )
          )
        )
    )
  )
}
