
r2_label <- function(r2) as.character(as.expression(substitute(
  italic(R)^2~"="~r2, 
  list(
    r2 = format(r2, digits = 3)))))


# based on ggplot2 draw_key_polygon and draw_key_path
draw_key_area <- function(data, params, size) {
  if (is.null(data$linetype)) {
    data$linetype <- 0
  } else {
    data$linetype[is.na(data$linetype)] <- 0
  }
  
  grid::gList(
    grid::rectGrob(
      width = 1,
      height = 1,
      gp = grid::gpar(
        fill = alpha(data$fill %||% "grey20", data$alpha),
        lty = 'blank'
      )
    ),
    grid::segmentsGrob(
      0, 1, 1, 1,
      gp = grid::gpar(
        col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
        fill = alpha(
          params$arrow.fill %||% data$colour %||% data$fill %||% "black",
          data$alpha),
        lwd = (data$linewidth %||% 0.5) * .pt,
        lty = data$linetype %||% 1,
        lineend = params$lineend %||% "butt"
      ),
      arrow = params$arrow
    )  
  )
}