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

## mapping helper

map <- function(
    x,
    range = c(0, 1),
    from.range = NA) {
  if (is.null(from.range) || any(is.na(from.range))) {
    from.range <- range(x, na.rm = TRUE)
  }

  ## check if all values are the same
  if (!diff(from.range)) {
    if (is.matrix(x)) {
      return(matrix(
        mean(range),
        ncol = ncol(x),
        nrow = nrow(x),
        dimnames = dimnames(x)
      ))
    } else {
      return(structure(rep(mean(range), length(x)), names = names(x)))
    }
  }

  ## map to [0,1]
  x <- (x - from.range[1])
  x <- x / diff(from.range)
  ## handle single values
  if (diff(from.range) == 0) {
    x <- 0
  }

  ## map from [0,1] to [range]
  if (range[1] > range[2]) {
    x <- 1 - x
  }
  x <- x * (abs(diff(range))) + min(range)

  x[x < min(range) | x > max(range)] <- NA

  x
}


map_int <- function(
    x,
    range = c(1, 100),
    from.range = NA) {
  floor(map(x, c(range[1], range[2] + .9), from.range))
}
