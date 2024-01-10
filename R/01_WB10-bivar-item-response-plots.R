# funs --------------------------------------------------------------------
bivar_prop <- function(x1, x2) {
  stopifnot(length(x1) == length(x2), typeof(x1) == "integer", typeof(x2) == "integer")
  props <- table(x1, x2, dnn = c(deparse(substitute(x1)), deparse(substitute(x2)))) / sum(table(x1, x2))
  props
}

plot_biver_prop <- function(x, title = "", xlab = "", ylab = "") {
  stopifnot(is.table(x))
  data <- expand.grid(as.numeric(dimnames(x)[[1]]), as.numeric(dimnames(x)[[2]]))
  data$p <- as.vector(x)
  ggplot(data, aes(data[ , 1], data[ ,2], fill = p)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    ggtitle(title) +
    xlab(xlab) + ylab(ylab)
}

#' Plot Bivariate Item Response Proportions in a grid
#' 
#' @param y_item Character string. WB10 item label
#' @param x_items Charactor vector. WB10 item labels
#'
#' @export
plot_WB10_responses <- function(data, y_item, x_items) {
  v2plot <- list()
  for(i in seq_along(x_items)) {
    v2table <- bivar_prop(data[[x_items[i]]], data[[y_item]])
    v2plot[[i]] <- plot_biver_prop(v2table, xlab = x_items[i], ylab = y_item)
  }
  v2plot
}
