#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyright (C) 2021 Michael Hahsler
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

plot.rules <- function(x,
  method = NULL,
  measure = "support",
  shading = "lift",
  limit = NULL,
  interactive = NULL,
  engine = "default",
  data = NULL,
  control = NULL,
  ...) {
  methods <- c(
    "matrix",
    "mosaic",
    "doubledecker",
    "graph",
    "paracoord",
    "scatterplot",
    "grouped matrix",
    "two-key plot",
    "matrix3D"
  )
  
  if (!is.null(method) && method == "help") {
    message("Available methods for plotting rules are:\n",
      paste0(methods, collapse = ", "))
    return(invisible(methods))
  }
  
  if (length(x) < 1)
    stop("x contains 0 rules!")
  
  ## add order and id
  quality(x)$order <- size(x)
  quality(x)$id <- seq_along(x)
  
  ## default is a scatter plot
  if (is.null(method))
    methodNr <- 6
  else
    methodNr <- pmatch(tolower(method), tolower(methods))
  if (is.na(methodNr))
    stop (paste(
      "Unknown method:",
      sQuote(method),
      "\nAvailable methods:",
      paste(sQuote(methods), collapse = ", ")
    ))
  
  ## complete measure and shading
  mid <- pmatch(measure, colnames(quality(x)), duplicates.ok = TRUE)
  if (any(is.na(mid)))
    stop("Measure not available in rule set: ",
      paste(sQuote(measure[is.na(mid)]), collapse = ", "))
  measure <- colnames(quality(x))[mid]
  
  ## NULL means no shading
  if (!is.null(shading)) {
    sid <- pmatch(shading, colnames(quality(x)))
    if (any(is.na(sid)))
      stop("Shading measure not available in rule set: ",
        paste(sQuote(shading[is.na(sid)]), collapse = ", "))
    shading <- colnames(quality(x))[sid]
  }
  
  ## limit
  x <- limit(x, limit, shading, measure, quiet = TRUE)
  
  ## add interactive and engine
  if (!is.null(interactive)) {
    warning("The parameter interactive is deprecated. Use engine='interactive' instead.")
    if (engine == "default" && interactive)
      engine <- "interactive"
  } else {
    interactive <- FALSE
  }
  
  ### if control is character then I think it is "help"
  if (is.character(control))
    control <- list(help = TRUE)
  if (is.null(control$engine))
    control$engine <- engine
  
  ## work horses
  if (methodNr == 1)
    matrixplot(x, measure = shading, control, ...)
  
  else if (methodNr == 2)
    doubledeckerplot(x,
      measure = measure,
      data = data,
      c(control, list(type = "mosaic")),
      ...)
  
  else if (methodNr == 3)
    doubledeckerplot(x,
      measure = measure,
      data = data,
      c(control, list(type = "doubledecker")),
      ...)
  
  else if (methodNr == 4)
    graphplot(x, measure = measure,
      shading = shading, control, ...)
  
  else if (methodNr == 5)
    paracoord_rules(x,
      measure = measure,
      shading = shading,
      control = control,
      ...)
  
  else if (methodNr == 6) {
    if (length(measure) < 2)
      measure[2] <- "confidence"
    scatterplot(x, measure = measure, shading = shading, control, ...)
  }
  
  else if (methodNr == 7)
    grouped_matrix_plot(x,
      measure = measure,
      shading = shading,
      control = control,
      ...)
  
  else if (methodNr == 8) {
    if (is.null(control$col))
      control$col <- grDevices::rainbow(max(size(x)) - 1L)
    scatterplot(
      x,
      measure = c("support", "confidence"),
      shading = "order",
      control,
      ...
    )
  }
  
  else if (methodNr == 9) {
    warning("method 'matrix3D' is deprecated use method 'matrix' with engine '3d'")
    control$engine <- "3d"
    matrixplot(x, measure = shading, control = control, ...)
  }
}

plot.itemsets <- function(x,
  method = NULL,
  measure = "support",
  shading = NULL,
  limit = NULL,
  interactive = NULL,
  engine = "default",
  data = NULL,
  control = NULL,
  ...) {
  ## methods
  methods <- c("graph",
    "paracoord",
    "scatterplot")
  
  if (!is.null(method) && method == "help") {
    message("Available methods for plotting itemsets are:\n",
      paste0(methods, collapse = ", "))
    return(invisible(methods))
  }
  
  ## add interactive and engine
  if (is.null(control$engine))
    control$engine <- engine
  if (!is.null(interactive)) {
    warning("The parameter interactive is deprecated. Use engine='interactive' instead.")
    if (engine == "default" && interactive)
      engine <- "interactive"
  } else {
    interactive <- FALSE
  }
  
  if (length(x) < 1)
    stop("x contains 0 itemsets!")
  
  ## add order
  quality(x)$order <- size(x)
  quality(x)$id <- seq_along(x)
  
  ## limit
  x <- limit(x, limit, shading, measure, quiet = TRUE)
  
  if (is.null(method))
    methodNr <- 3
  else
    methodNr <- pmatch(tolower(method), tolower(methods))
  if (is.na(methodNr))
    stop (paste(
      "Unknown method:",
      sQuote(method),
      "\nAvailable methods:",
      paste(sQuote(methods), collapse = ", ")
    ))
  
  
  ## work horses
  if (methodNr == 1)
    graphplot(x,
      measure = measure,
      shading = shading,
      control = control,
      ...)
  
  else if (methodNr == 2)
    paracoord_items(x,
      measure = measure,
      shading = shading,
      control = control,
      ...)
  
  else if (methodNr == 3) {
    if (length(measure) < 2)
      measure[2] <- "order"
    scatterplot(x, measure = measure, shading = shading, control, ...)
  }
  
}
