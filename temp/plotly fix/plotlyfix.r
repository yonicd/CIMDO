`%||%`=function (x, y) 
{
  if (length(x) > 0) 
    x
  else y
}

gg2list.fix=function (p) 
{
  if (length(p$layers) == 0) 
    p <- p + geom_blank()
  layout <- list()
  trace.list <- list()
  for (layer.i in seq_along(p$layers)) {
    layer.aes <- p$layers[[layer.i]]$mapping
    if (p$layers[[layer.i]]$inherit.aes) {
      to.copy <- names(p$mapping)[!names(p$mapping) %in% 
                                    names(layer.aes)]
      layer.aes[to.copy] <- p$mapping[to.copy]
    }
    mark.names <- names(layer.aes)
    name.names <- sprintf("%s.name", mark.names)
    layer.aes[name.names] <- layer.aes[mark.names]
    p$layers[[layer.i]]$mapping <- layer.aes
    if (!is.data.frame(p$layers[[layer.i]]$data)) {
      p$layers[[layer.i]]$data <- p$data
    }
  }
  misc <- list()
  for (a in c("fill", "colour", "x", "y", "size")) {
    for (data.type in c("continuous", "date", "datetime", 
                        "discrete")) {
      fun.name <- sprintf("scale_%s_%s", a, data.type)
      misc.name <- paste0("is.", data.type)
      misc[[misc.name]][[a]] <- tryCatch({
        fun <- get(fun.name)
        suppressMessages({
          with.scale <- p + fun()
        })
        ggplot_build(with.scale)
        TRUE
      }, error = function(e) {
        FALSE
      })
    }
  }
  misc$breaks <- list()
  for (sc in p$scales$scales) {
    a.vec <- sc$aesthetics
    default.breaks <- inherits(sc$breaks, "waiver")
    if (length(a.vec) == 1 && (!default.breaks)) {
      br <- sc$breaks
      ranks <- seq_along(br)
      names(ranks) <- br
      misc$breaks[[a.vec]] <- ranks
    }
    if (is.character(sc$trans$name)) {
      misc$trans[sc$aesthetics] <- sc$trans$name
    }
  }
  reverse.aes <- names(misc$trans)[misc$trans == "reverse"]
  built <- ggplot_build2(p)
  ranges.list <- list()
  for (xy in c("x", "y")) {
    use.ranges <- misc$is.continuous[[xy]] || misc$is.date[[xy]] || 
      misc$is.datetime[[xy]]
    range.values <- if (use.ranges) {
      range.name <- paste0(xy, ".range")
      sapply(built$panel$ranges, "[[", range.name)
    }
    else {
      name.name <- paste0(xy, ".name")
      sapply(built$data, function(df) {
        if (name.name %in% names(df)) {
          paste(df[[name.name]])
        }
        else {
          df[[xy]]
        }
      })
    }
    ranges.list[[xy]] <- range(range.values)
  }
  if ("size.name" %in% name.names) {
    sizerange <- sapply(built$prestats.data, `[[`, "size")
    ggsizemin <- min(unlist(sizerange))
    ggsizemax <- max(unlist(sizerange))
  }
  layer.legends <- list()
  for (i in seq_along(built$plot$layers)) {
    L <- p$layers[[i]]
    df <- built$data[[i]]
    gglayout <- built$panel$layout
    gglayout$plotly.row <- max(gglayout$ROW) - gglayout$ROW + 
      1
    gglayout$plotly.panel <- with(gglayout, order(plotly.row, 
                                                  COL))
    df$order <- seq_len(nrow(df))
    df <- merge(df, gglayout[, c("PANEL", "plotly.row", 
                                 "COL")])
    df <- df[order(df$order), ]
    df$order <- NULL
    prestats <- built$prestats.data[[i]]
    replace.aes <- intersect(names(prestats), reverse.aes)
    for (a in replace.aes) {
      prestats[[a]] <- -1 * prestats[[a]]
    }
    L$prestats.data <- merge(prestats, gglayout[, c("PANEL", 
                                                    "plotly.row", "COL")])
    for (xy in names(ranges.list)) {
      range.vec <- ranges.list[[xy]]
      names(range.vec) <- c("min", "max")
      for (range.name in names(range.vec)) {
        glob.name <- paste0("glob", xy, range.name)
        L$prestats.data[[glob.name]] <- range.vec[[range.name]]
      }
    }
    if ("size.name" %in% name.names) {
      L$prestats.data$globsizemin <- ggsizemin
      L$prestats.data$globsizemax <- ggsizemax
    }
    traces <- layer2traces.fix(L, df, misc)
    possible.legends <- plotly:::markLegends[[plotly:::type(L, "geom")]]
    actual.legends <- possible.legends[possible.legends %in% 
                                         names(L$mapping)]
    layer.legends[[paste(i)]] <- actual.legends
    trace.list <- c(trace.list, traces)
  }
  barmodes <- do.call(c, lapply(trace.list, function(x) x$barmode))
  barmodes <- barmodes[!is.null(barmodes)]
  if (length(barmodes) > 0) {
    layout$barmode <- barmodes[1]
    if (!all(barmodes == barmodes[1])) 
      warning(paste0("You have multiple barcharts or histograms with different positions; ", 
                     "Plotly's layout barmode will be '", layout$barmode, 
                     "'."))
    if (layout$barmode == "stack") {
      unStack <- function(vec) {
        n <- length(vec)
        if (n == 1) 
          return(vec)
        seq.n <- seq_len(n)
        names(vec) <- seq.n
        vec <- sort(vec)
        for (k in seq(2, n)) {
          vec[k] <- vec[k] - sum(vec[seq(1, k - 1)])
        }
        as.numeric(vec[as.character(seq.n)])
      }
      ys <- lapply(trace.list, "[[", "y")
      xs <- lapply(trace.list, "[[", "x")
      x.vals <- unique(unlist(xs))
      for (val in x.vals) {
        zs <- lapply(xs, function(x) which(x == val))
        ys.given.x <- Map(function(x, y) y[x], zs, ys)
        if (length(unlist(ys.given.x)) < 2) 
          next
        st <- unStack(unlist(ys.given.x))
        lens <- sapply(ys.given.x, length)
        trace.seq <- seq_along(trace.list)
        ws <- split(st, rep(trace.seq, lens))
        for (tr in seq_along(ws)) {
          idx <- zs[[tr]]
          replacement <- ws[[tr]]
          if (length(idx) > 0 && length(replacement) > 
              0) 
            trace.list[[tr]]$y[idx] <- replacement
        }
      }
    }
  }
  bargaps <- do.call(c, lapply(trace.list, function(x) x$bargap))
  if (length(bargaps) > 0) {
    if (any(bargaps == 0)) {
      layout$bargap <- 0
      if (!all(bargaps == 0)) {
        warning("You have multiple bar charts and histograms;\n\n              Plotly's layout bargap will be 0 for all of them.")
      }
    }
    else {
      bargaps <- NULL
    }
  }
  theme.pars <- getFromNamespace("plot_theme", "ggplot2")(p)
  e <- function(el.name) {
    ggplot2::calc_element(el.name, p$theme)
  }
  is.blank <- function(el.name, null.is.blank = FALSE) {
    cls <- attr(e(el.name), "class")
    "element_blank" %in% cls || null.is.blank && is.null(cls)
  }
  trace.order.list <- list()
  trace.name.map <- c()
  for (xy in c("x", "y")) {
    ax.list <- list()
    coord.lim <- p$coordinates$limits[[xy]] %||% p$scales$get_scales(xy)$limits
    if (is.numeric(coord.lim)) {
      ax.list$range <- coord.lim
    }
    s <- function(tmp) sprintf(tmp, xy)
    ax.list$tickcolor <- toRGB(theme.pars$axis.ticks$colour)
    grid <- theme.pars$panel.grid
    grid.major <- theme.pars$panel.grid.major
    if ((!is.null(grid$linetype) || !is.null(grid.major$linetype)) && 
        c(grid$linetype, grid.major$linetype) %in% c(2, 
                                                     3, "dashed", "dotted")) {
      ax.list$gridcolor <- ifelse(is.null(grid.major$colour), 
                                  toRGB(grid$colour, 0.1), toRGB(grid.major$colour, 
                                                                 0.1))
    }
    else {
      ax.list$gridcolor <- toRGB(grid.major$colour)
    }
    ax.list$showgrid <- !is.blank(s("panel.grid.major.%s"))
    theme2font <- function(text) {
      if (!is.null(text)) {
        list(family = text$family, size = text$size, 
             color = toRGB(text$colour))
      }
    }
    if (is.blank("axis.ticks")) {
      ax.list$ticks <- ""
    }
    else if (is.blank(s("axis.ticks.%s"))) {
      ax.list$ticks <- ""
    }
    else {
      ax.list$ticks <- "outside"
    }
    ax.list$tickwidth <- theme.pars$axis.ticks$size
    tick.text.name <- s("axis.text.%s")
    ax.list$showticklabels <- !is.blank(tick.text.name)
    tick.text <- e(tick.text.name)
    if (is.numeric(tick.text$angle)) {
      ax.list$tickangle <- -tick.text$angle
    }
    ax.list$tickfont <- theme2font(tick.text)
    title.text <- e(s("axis.title.%s"))
    ax.list$titlefont <- theme2font(title.text)
    ax.list$type <- if (misc$is.continuous[[xy]]) {
      "linear"
    }
    else if (misc$is.discrete[[xy]]) {
      "category"
    }
    else if (misc$is.date[[xy]] || misc$is.datetime[[xy]]) {
      "date"
    }
    else {
      stop("unrecognized data type for ", xy, " axis")
    }
    scale.i <- which(p$scales$find(xy))
    ax.list$title <- if (length(scale.i)) {
      sc <- p$scales$scales[[scale.i]]
      if (ax.list$type == "category") {
        trace.order.list[[xy]] <- sc$limits
        if (is.character(sc$breaks)) {
          if (is.character(sc$labels)) {
            trace.name.map[sc$breaks] <- sc$labels
          }
        }
      }
      if (is.null(sc$breaks)) {
        ax.list$showticklabels <- FALSE
        ax.list$showgrid <- FALSE
        ax.list$ticks <- ""
      }
      if (is.numeric(sc$breaks)) {
        dticks <- diff(sc$breaks)
        dt <- dticks[1]
        if (all(dticks == dt)) {
          ax.list$dtick <- dt
          ax.list$autotick <- FALSE
        }
      }
      ax.list$range <- if (!is.null(sc$limits)) {
        sc$limits
      }
      else {
        if (misc$is.continuous[[xy]]) {
          built$panel$ranges[[1]][[s("%s.range")]]
        }
        else {
          NULL
        }
      }
      if (is.character(sc$trans$name) && sc$trans$name == 
          "reverse") {
        ax.list$range <- sort(-ax.list$range, decreasing = TRUE)
      }
      if (!is.null(sc$name)) {
        sc$name
      }
      else {
        p$labels[[xy]]
      }
    }
    else {
      p$labels[[xy]]
    }
    ax.list$zeroline <- FALSE
    ax.list$showline <- !is.blank("panel.border", TRUE)
    ax.list$linecolor <- toRGB(theme.pars$panel.border$colour)
    ax.list$linewidth <- theme.pars$panel.border$size
    !is.blank(s("axis.line.%s"))
    layout[[s("%saxis")]] <- ax.list
    nms <- unlist(lapply(traces, "[[", "name"))
    if (is.discrete(ax.list$range) && !is.null(nms)) 
      trace.list <- trace.list[nms %in% ax.list$range]
  }
  xaxis.title <- layout$xaxis$title
  yaxis.title <- layout$yaxis$title
  inner.margin <- 0.01
  outer.margin <- 0.05
  orig.xaxis <- layout$xaxis
  orig.yaxis <- layout$yaxis
  if (nrow(gglayout) > 1) {
    row.size <- 1/max(gglayout$ROW)
    col.size <- 1/max(gglayout$COL)
    npanels <- nrow(gglayout)
    for (i in seq_len(npanels)) {
      row <- gglayout[i, "plotly.row"]
      col <- gglayout[i, "COL"]
      panel <- gglayout[i, "plotly.panel"]
      x <- col * col.size
      xmin <- x - col.size
      xmax <- x - inner.margin
      y <- row * row.size
      ymin <- y - row.size
      ymax <- y - inner.margin
      xaxis.name <- if (col == 1) 
        "xaxis"
      else paste0("xaxis", col)
      yaxis.name <- if (row == 1) 
        "yaxis"
      else paste0("yaxis", row)
      xanchor <- "y"
      yanchor <- "x"
      if ("wrap" %in% class(p$facet)) {
        ymax <- ymax - 0.04
        if (col == 1) {
          xmax <- xmax - 0.02
        }
        else {
          xmin <- xmin + 0.02
        }
        if (row == 1) {
          ymax <- ymax - 0.02
        }
        else {
          ymin <- ymin + 0.02
        }
        if (p$facet$free$y && panel > 1) {
          yaxis.name <- paste0("yaxis", panel)
          trace.list[[i]]$yaxis <- paste0("y", panel)
          yanchor <- if (p$facet$free$x) 
            paste0("x", panel)
          else paste0("x", col)
        }
        if (p$facet$free$x && panel > 1) {
          xaxis.name <- paste0("xaxis", panel)
          trace.list[[i]]$xaxis <- paste0("x", panel)
          xanchor <- if (p$facet$free$y) 
            paste0("y", panel)
          else paste0("y", row)
        }
      }
      layout[[xaxis.name]] <- orig.xaxis
      layout[[xaxis.name]]$domain <- c(xmin, xmax)
      layout[[xaxis.name]]$anchor <- xanchor
      layout[[xaxis.name]]$title <- NULL
      layout[[yaxis.name]] <- orig.yaxis
      layout[[yaxis.name]]$domain <- c(ymin, ymax)
      layout[[yaxis.name]]$anchor <- yanchor
      layout[[yaxis.name]]$title <- NULL
      if (is.null(layout[[xaxis.name]]$anchor)) 
        layout[[xaxis.name]]$anchor <- "y"
      if (is.null(layout[[yaxis.name]]$anchor)) 
        layout[[yaxis.name]]$anchor <- "x"
      if (orig.xaxis$type == "linear") {
        layout[[xaxis.name]]$range <- built$panel$ranges[[i]]$x.range
        layout[[xaxis.name]]$autorange <- FALSE
      }
      if (orig.yaxis$type == "linear") {
        layout[[yaxis.name]]$range <- built$panel$ranges[[i]]$y.range
        layout[[yaxis.name]]$autorange <- FALSE
      }
    }
    annotations <- list()
    nann <- 1
    make.label <- function(text, x, y, xanchor = "auto", 
                           yanchor = "auto", textangle = 0) list(text = text, 
                                                                 showarrow = FALSE, x = x, y = y, ax = 0, ay = 0, 
                                                                 xref = "paper", yref = "paper", xanchor = xanchor, 
                                                                 yanchor = yanchor, textangle = textangle)
    if ("grid" %in% class(p$facet)) {
      frows <- names(p$facet$rows)
      nann <- 1
      for (i in seq_len(max(gglayout$ROW))) {
        text <- paste(lapply(gglayout[gglayout$ROW == 
                                        i, frows, drop = FALSE][1, ], as.character), 
                      collapse = ", ")
        if (text != "") {
          increase_margin_r <- TRUE
          annotations[[nann]] <- make.label(text, 1 + 
                                              outer.margin - 0.04, row.size * (max(gglayout$ROW) - 
                                                                                 i + 0.5), xanchor = "center", textangle = 90)
          nann <- nann + 1
        }
      }
      fcols <- names(p$facet$cols)
      for (i in seq_len(max(gglayout$COL))) {
        text <- paste(lapply(gglayout[gglayout$COL == 
                                        i, fcols, drop = FALSE][1, ], as.character), 
                      collapse = ", ")
        if (text != "") {
          annotations[[nann]] <- make.label(text, col.size * 
                                              (i - 0.5) - inner.margin/2, 1 + outer.margin, 
                                            xanchor = "center")
          nann <- nann + 1
        }
      }
      for (r in seq_len(max(gglayout$ROW))) for (c in seq_len(max(gglayout$COL))) trace.list <- c(trace.list, 
                                                                                                  list(list(xaxis = paste0("x", c), yaxis = paste0("y", 
                                                                                                                                                   r), showlegend = FALSE)))
    }
    else if ("wrap" %in% class(p$facet)) {
      facets <- names(p$facet$facets)
      for (i in seq_len(max(as.numeric(gglayout$PANEL)))) {
        ix <- gglayout$PANEL == i
        row <- gglayout$ROW[ix]
        col <- gglayout$COL[ix]
        text <- paste(lapply(gglayout[ix, facets, drop = FALSE][1, 
                                                                ], as.character), collapse = ", ")
        annotations[[nann]] <- make.label(text, col.size * 
                                            (col - 0.5) - inner.margin/2, row.size * (max(gglayout$ROW) - 
                                                                                        row + 0.985), xanchor = "center", yanchor = "top")
        nann <- nann + 1
      }
    }
    annotations[[nann]] <- make.label(xaxis.title, 0.5, 
                                      -outer.margin, yanchor = "top")
    nann <- nann + 1
    annotations[[nann]] <- make.label(yaxis.title, -outer.margin, 
                                      0.5, textangle = -90)
    layout$annotations <- annotations
  }
  layout$title <- built$plot$labels$title
  layout$plot_bgcolor <- toRGB(theme.pars$panel.background$fill)
  layout$paper_bgcolor <- toRGB(theme.pars$plot.background$fill)
  layout$margin$r <- 10
  if (exists("increase_margin_r")) {
    layout$margin$r <- 60
  }
  layout$legend <- list(bordercolor = "transparent", x = 1.01, 
                        y = 0.075 * 0.5 * length(trace.list) + 0.45, xref = "paper", 
                        yref = "paper", xanchor = "left", yanchor = "top")
  legends.present <- unique(unlist(layer.legends))
  is.false <- function(x) {
    is.logical(x) && length(x) == 1 && x == FALSE
  }
  is.none <- function(x) {
    is.character(x) && length(x) == 1 && x == "none"
  }
  is.hidden <- function(x) {
    is.false(x) || is.none(x)
  }
  layout$showlegend <- if (length(legends.present) == 0) 
    FALSE
  else TRUE
  for (a in legends.present) {
    if (is.hidden(p$guides[[a]])) {
      layout$showlegend <- FALSE
    }
  }
  if (theme.pars$legend.position == "none") {
    layout$showlegend <- FALSE
  }
  ggplot_labels <- ggplot2::labs(p)$labels
  trace.showlegend <- sapply(trace.list, "[[", "showlegend")
  if (any(trace.showlegend) && layout$showlegend && length(p$data)) {
    temp.title <- plotly:::guide_names(p)
    legend.title <- if (length(unique(temp.title)) > 1) {
      paste(temp.title, collapse = " / ")
    }
    else {
      unique(temp.title)
    }
    legend.title <- paste0("<b>", legend.title, "</b>")
    if (exists("annotations")) {
      nann <- nann + 1
    }
    else {
      annotations <- list()
      nann <- 1
    }
    annotations[[nann]] <- list(text = legend.title, x = layout$legend$x * 
                                  1.0154, y = 0.075 * 0.5 * length(trace.list) + 0.55, 
                                showarrow = FALSE, xref = "paper", yref = "paper", 
                                xanchor = "left", yanchor = "top", textangle = 0)
    layout$annotations <- annotations
  }
  if (!is.null(theme.pars$text$family)) {
    layout$titlefont$family <- theme.pars$text$family
    layout$legend$font$family <- theme.pars$text$family
  }
  if (!is.null(theme.pars$plot.title$family)) {
    layout$titlefont$family <- theme.pars$plot.title$family
  }
  if (!is.null(theme.pars$legend.text$family)) {
    layout$legend$font$family <- theme.pars$legend.text$family
  }
  text_face <- theme.pars$text$face
  if (!is.null(text_face)) {
    if (text_face == "bold") {
      layout$title <- paste0("<b>", layout$title, "</b>")
      layout$yaxis$title <- paste0("<b>", layout$yaxis$title, 
                                   "</b>")
      layout$xaxis$title <- paste0("<b>", layout$xaxis$title, 
                                   "</b>")
    }
    else if (text_face == "italic") {
      layout$title <- paste0("<i>", layout$title, "</i>")
      layout$yaxis$title <- paste0("<i>", layout$yaxis$title, 
                                   "</i>")
      layout$xaxis$title <- paste0("<i>", layout$xaxis$title, 
                                   "</i>")
    }
    else if (text_face == "bold.italic") {
      layout$title <- paste0("<b><i>", layout$title, "</i></b>")
      layout$yaxis$title <- paste0("<b><i>", layout$yaxis$title, 
                                   "</i></b>")
      layout$xaxis$title <- paste0("<b><i>", layout$xaxis$title, 
                                   "</i></b>")
    }
  }
  title_face <- theme.pars$plot.title$face
  if (!is.null(title_face)) {
    if (title_face == "bold") {
      layout$title <- paste0("<b>", layout$title, "</b>")
    }
    else if (title_face == "italic") {
      layout$title <- paste0("<i>", layout$title, "</i>")
    }
    else if (title_face == "bold.italic") {
      layout$title <- paste0("<b><i>", layout$title, "</i></b>")
    }
  }
  title_face <- list(theme.pars$axis.title.y$face, theme.pars$axis.title.x$face)
  sub_elem <- c("yaxis", "xaxis")
  for (i in seq_along(title_face)) {
    if (!is.null(title_face[[i]])) {
      if (title_face[[i]] == "bold") {
        layout[[sub_elem[i]]]["title"] <- paste0("<b>", 
                                                 layout[[sub_elem[i]]]["title"], "</b>")
      }
      else if (title_face[[i]] == "italic") {
        layout[[sub_elem[i]]]["title"] <- paste0("<i>", 
                                                 layout[[sub_elem[i]]]["title"], "</i>")
      }
      else if (title_face[[i]] == "bold.italic") {
        layout[[sub_elem[i]]]["title"] <- paste0("<b><i>", 
                                                 layout[[sub_elem[i]]]["title"], "</b></i>")
      }
    }
  }
  rect_fill <- theme.pars$rect$fill
  if (!is.null(rect_fill)) {
    if (is.null(layout$plot_bgcolor)) 
      layout$plot_bgcolor <- toRGB(s(rect_fill))
    if (is.null(layout$paper_bgcolor)) 
      layout$paper_bgcolor <- toRGB(s(rect_fill))
    if (is.null(layout$legend$bgcolor)) 
      layout$legend$bgcolor <- toRGB(s(rect_fill))
  }
  if (length(trace.list) == 0) {
    stop("No exportable traces")
  }
  mode.mat <- matrix(NA, 3, 3)
  rownames(mode.mat) <- colnames(mode.mat) <- c("markers", 
                                                "lines", "none")
  mode.mat["markers", "lines"] <- mode.mat["lines", "markers"] <- "lines+markers"
  mode.mat["markers", "none"] <- mode.mat["none", "markers"] <- "markers"
  mode.mat["lines", "none"] <- mode.mat["none", "lines"] <- "lines"
  merged.traces <- list()
  not.merged <- trace.list
  while (length(not.merged)) {
    tr <- not.merged[[1]]
    not.merged <- not.merged[-1]
    can.merge <- logical(length(not.merged))
    for (other.i in seq_along(not.merged)) {
      other <- not.merged[[other.i]]
      criteria <- c()
      for (must.be.equal in c("x", "y", "xaxis", "yaxis")) {
        other.attr <- other[[must.be.equal]]
        tr.attr <- tr[[must.be.equal]]
        criteria[[must.be.equal]] <- isTRUE(all.equal(other.attr, 
                                                      tr.attr)) && unique(other$type, tr$type) == 
          "scatter"
      }
      if (all(criteria)) {
        can.merge[[other.i]] <- TRUE
      }
    }
    to.merge <- not.merged[can.merge]
    not.merged <- not.merged[!can.merge]
    for (other in to.merge) {
      new.mode <- tryCatch({
        mode.mat[tr$mode, other$mode]
      }, error = function(e) {
        NA
      })
      if (is.character(new.mode) && !is.na(new.mode %||% 
                                           NA)) {
        tr$mode <- new.mode
      }
      attrs <- c("error_x", "error_y", "marker", "line")
      for (attr in attrs) {
        if (!is.null(other[[attr]]) && is.null(tr[[attr]])) {
          tr[[attr]] <- other[[attr]]
        }
      }
    }
    merged.traces[[length(merged.traces) + 1]] <- tr
  }
  rm_alpha <- function(x) {
    if (length(x) == 0) 
      return(x)
    pat <- "^rgba\\("
    if (any(!grepl(pat, x))) 
      return(x)
    sub(",\\s*[0]?[.]?[0-9]+\\)$", ")", sub(pat, "rgb(", 
                                            x))
  }
  entries <- function(x, y) {
    z <- try(x[[y]], silent = TRUE)
    if (inherits(e, "try-error")) {
      paste0(x$name, "-")
    }
    else {
      paste0(x$name, "-", rm_alpha(z))
    }
  }
  fill_set <- unlist(lapply(merged.traces, entries, "fillcolor"))
  line_set <- unlist(lapply(merged.traces, entries, c("line", 
                                                      "color")))
  mark_set <- unlist(lapply(merged.traces, entries, c("marker", 
                                                      "color")))
  mode_set <- lapply(merged.traces, "[[", "mode")
  legend_intersect <- function(x, y) {
    i <- intersect(x, y)
    i[grepl("-rgb[a]?\\(", i)]
  }
  t1 <- line_set %in% legend_intersect(mark_set, line_set)
  t1 <- t1 & !(mode_set %in% "lines+markers")
  t2 <- fill_set %in% legend_intersect(mark_set, fill_set)
  t3 <- fill_set %in% legend_intersect(line_set, fill_set)
  t <- t1 | t2 | t3
  for (m in seq_along(merged.traces)) if (isTRUE(merged.traces[[m]]$showlegend && 
                                                 t[m])) 
    merged.traces[[m]]$showlegend <- FALSE
  trace.order <- unlist(trace.order.list)
  ordered.traces <- if (length(trace.order)) {
    trace.order.score <- seq_along(trace.order)
    names(trace.order.score) <- trace.order
    trace.name <- sapply(merged.traces, "[[", "name")
    trace.score <- trace.order.score[trace.name]
    merged.traces[order(trace.score)]
  }
  else {
    merged.traces
  }
  named.traces <- ordered.traces
  for (trace.i in seq_along(named.traces)) {
    tr.name <- named.traces[[trace.i]][["name"]]
    new.name <- trace.name.map[[tr.name]]
    if (!is.null(new.name)) {
      named.traces[[trace.i]][["name"]] <- new.name
    }
  }
  flipped.traces <- named.traces
  flipped.layout <- layout
  coord_cl <- sub("coord", "", tolower(class(built$plot$coordinates)))
  if ("flip" %in% coord_cl) {
    if (!inherits(p$facet, "null")) {
      stop("coord_flip + facet conversion not supported")
    }
    for (trace.i in seq_along(flipped.traces)) {
      tr <- flipped.traces[[trace.i]]
      x <- tr[["x"]]
      y <- tr[["y"]]
      tr[["y"]] <- x
      tr[["x"]] <- y
      flipped.traces[[trace.i]] <- tr
    }
    x <- layout[["xaxis"]]
    y <- layout[["yaxis"]]
    flipped.layout[["xaxis"]] <- y
    flipped.layout[["yaxis"]] <- x
  }
  l <- list(data = flipped.traces, layout = flipped.layout)
  structure(add_boxed.fix(plotly:::rm_asis(l)), class = "plotly")
}

ggplotly.fix=function (p = ggplot2::last_plot(), filename, fileopt, world_readable = TRUE) 
{
  l <- gg2list.fix(p)
  if (!missing(filename)) 
    l$filename <- filename
  if (!missing(fileopt)) 
    l$fileopt <- fileopt
  l$world_readable <- world_readable
  plotly:::hash_plot(p$data, l)
}


hash_plot.fix=function (df, p) 
{
  if (missing(df) || is.null(df)) 
    df <- data.frame()
  hash <- digest::digest(p)
  hash <- paste(hash, length(ls(plotlyEnv)), sep = "#")
  assign(hash, p, envir = plotlyEnv)
  attr(df, "plotly_hash") <- hash
  class(df) <- unique(c("plotly", class(df)))
  df
}

add_boxed.fix=function (x) 
{
  for (i in 1:length(x$data)) {
    d <- x$data[[i]]
    idx <- names(d) %in% plotly:::get_boxed() & sapply(d, length) == 
      1
    if (any(idx)) 
      x$data[[i]][idx] <- lapply(d[idx], I)
  }
  x
}