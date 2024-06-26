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

# plotly interactive plots using d3.js

## Interface used by plot
scatterplot_plotly <- function(
    x,
    measure = measure,
    shading = shading,
    control = control,
    ...) {
  control <- c(control, list(...))
  control <- .get_parameters(
    control,
    list(
      interactive = TRUE,
      engine = "htmlwidget",
      max = 1000,
      colors = default_colors(2),
      jitter = NA,
      precision = 3,
      main = "Unused",
      marker = list()
    )
  )

  quality(x)[["order"]] <- size(x)
  qnames <- names(quality(x))
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  if (!is.null(shading)) shading <- qnames[pmatch(shading, qnames)]

  .plotly_scatter(
    x,
    measure,
    shading,
    control$colors,
    control$jitter,
    control$precision,
    control$max,
    control$marker
  )
}

.plotly_scatter <- function(
    x,
    measure = c("support", "confidence"),
    shading = "lift",
    colors = default_colors(2),
    jitter = NA,
    precision = 3,
    max = 1000,
    marker = list(),
    ...) {
  colors <- rev(colors)

  quality(x)$id <- seq_along(x)
  x <- limit(x, max, shading, measure)

  ### reduce overplotting
  x <- rev(x)

  q <- quality(x)[, c(measure, shading)]
  for (i in 1:ncol(q)) {
    infin <- is.infinite(q[[i]])
    if (any(infin)) {
      replinfin <- signif(2 * max(q[[i]][!infin], na.rm = TRUE), 3)
      warning(
        "plot: ",
        colnames(q)[i],
        " contains infinite values! Replaced by twice the max (",
        replinfin,
        ")!",
        call. = FALSE
      )
      q[[i]][infin] <- replinfin
    }
  }

  if (is(x, "rules")) {
    l <- labels(
      x,
      itemSep = ",<BR>&nbsp;&nbsp;",
      ruleSep = "<BR>&nbsp;&nbsp; => ",
      setStart = "<B>{",
      setEnd = "}</B>"
    )
  } else {
    l <- labels(x,
      itemSep = ",<BR>&nbsp;&nbsp;",
      setStart = "<B>{",
      setEnd = "}</B>"
    )
  }

  txt <- paste(
    paste0("[", quality(x)$id, "]<BR>"),
    l,
    paste("<BR><BR>", measure[1], ": ", signif(q[, measure[1]], precision), sep = ""),
    paste("<BR>", measure[2], ": ", signif(q[, measure[2]], precision),
      sep =
        ""
    ),
    if (!is.null(shading)) {
      paste("<BR>", shading, ": ",
        if (is.numeric(q[, shading])) {
          signif(q[, shading], precision)
        } else {
          q[, shading]
        },
        sep = ""
      )
    } else {
      ""
    }
  )

  ### add x/y-jitter
  jitter <- jitter[1]
  if (is.na(jitter) && any(duplicated(q[, measure]))) {
    message("To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.")
    jitter <- .jitter_default
  }

  if (!is.na(jitter) && jitter > 0) {
    for (m in measure) {
      q[[m]] <- jitter(q[[m]], factor = jitter, amount = 0)
    }
  }

  if (is.null(shading)) {
    p <-
      plotly::plot_ly(
        q,
        type = "scatter",
        x = q[, measure[1]],
        y = q[, measure[2]],
        hoverinfo = "text",
        text = txt,
        mode = "markers",
        marker = marker
      )
  } else if (shading == "order") {
    p <-
      plotly::plot_ly(
        q,
        type = "scatter",
        x = q[, measure[1]],
        y = q[, measure[2]],
        hoverinfo = "text",
        text = txt,
        color = as.ordered(q[, shading]),
        mode = "markers",
        marker = marker
      )
  } else {
    p <-
      plotly::plot_ly(
        q,
        type = "scatter",
        x = q[, measure[1]],
        y = q[, measure[2]],
        hoverinfo = "text",
        text = txt,
        color = q[, shading],
        colors = colors,
        mode = "markers",
        marker = marker
      ) %>% plotly::colorbar(title = shading)
  }


  p %>%
    plotly::layout(
      hovermode = "closest",
      xaxis = list(title = measure[1]),
      yaxis = list(title = measure[2])
    )
}

matrix_plotly <- function(x, measure, shading, control, ...) {
  control <- c(control, list(...))
  control <- .get_parameters(
    control,
    list(
      interactive = TRUE,
      engine = "htmlwidget",
      max = 1000,
      colors = default_colors(2),
      reorder = "measure",
      precision = 3
    )
  )

  qnames <- names(quality(x))
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]

  .plotly_matrix(
    x,
    measure[1],
    reorder = control$reorder,
    colors = control$colors,
    precision = control$precision,
    max = control$max
  )
}

.plotly_matrix <- function(
    x,
    measure = "lift",
    reorder = "none",
    colors = default_colors(2),
    precision = 3,
    max = 1000) {
  colors <- rev(colors)

  x <- limit(x, max, measure = measure)

  m <- rules2matrix(x, measure, reorder)
  m <- m[nrow(m):1, , drop = FALSE] # reverse rows
  m_s <-
    rules2matrix(x, measure = "support", reorder = "none")[rownames(m), colnames(m)]
  m_c <-
    rules2matrix(x, measure = "confidence", reorder = "none")[rownames(m), colnames(m)]

  txt <-
    t(outer(colnames(m), rownames(m), paste, sep = "<BR>&nbsp;&nbsp; => "))
  txt[] <- paste(
    "<B>",
    txt,
    "</B>",
    "<BR>",
    measure,
    ": ",
    signif(m, precision),
    "<BR>",
    "support",
    ": ",
    signif(m_s, precision),
    "<BR>",
    "confidence",
    ": ",
    signif(m_c, precision),
    sep = ""
  )
  txt[is.na(m)] <- NA

  plotly::plot_ly(
    z = m,
    x = colnames(m),
    y = rownames(m),
    type = "heatmap",
    colors = colors,
    colorbar = list(title = measure),
    hoverinfo = "text",
    text = txt
  ) %>%
    plotly::layout(
      xaxis = list(
        title = "LHS",
        showticklabels = FALSE,
        showgrid = TRUE,
        ticks = ""
      ),
      yaxis = list(
        title = "RHS",
        showticklabels = FALSE,
        showgrid = TRUE,
        ticks = ""
      )
    )
}
