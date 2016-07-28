smart_palette <- function(n_colors) {
  if (n_colors <= 9) {
    return(RColorBrewer::brewer.pal(max(3, n_colors), "Set1"))
  }
  else if (n_colors < 12) {
    return(RColorBrewer::brewer.pal(n_colors, "Set3"))
  } else {
    return(colorspace::rainbow_hcl(n_colors))
  }
}
