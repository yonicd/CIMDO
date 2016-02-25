layer2traces.fix=function (l, d, misc) 
{
  g <- list(geom = plotly:::type(l, "geom"), data = d, prestats.data = l$prestats.data)
  g$aes <- sapply(l$mapping, function(k) as.character(as.expression(k)))
  if (g$geom == "violin") {
    g$geom <- "boxplot"
    warning("Converting violin plot into boxplot:\n\n            probability density estimation is not supported in Plotly yet.")
  }
  if (g$geom == "smooth") {
    if (isTRUE(misc$smoothLine)) {
      misc$smoothLine <- FALSE
      if (isTRUE(l$stat_params$se == FALSE)) {
        return(NULL)
      }
      else {
        g$geom <- "smoothRibbon"
        g$data <- g$data[!grepl("^colour[.name]?", names(g$data))]
      }
    }
    else {
      misc$smoothLine <- TRUE
      g$geom <- "smoothLine"
    }
  }
  for (axis.name in c("x", "y")) {
    if (!misc$is.continuous[[axis.name]]) {
      aes.names <- paste0(axis.name, c("", "end", "min", 
                                       "max"))
      aes.used <- aes.names[aes.names %in% names(g$aes)]
      for (a in aes.used) {
        a.name <- paste0(a, ".name")
        col.name <- g$aes[a.name]
        dtemp <- l$data[[col.name]]
        if (is.null(dtemp)) {
          if (!is.null(g$data[[a.name]])) {
            if (class(g$data[[a]]) != class(g$data[[a.name]])) {
              g$data[[a]] <- g$data[[a.name]]
              data.vec <- g$data[[a]]
            }
          }
        }
        else {
          data.vec <- dtemp
        }
        pdata.vec <- g$prestats.data[[a]]
        if (inherits(data.vec, "POSIXt")) {
          data.vec <- try(strftime(as.POSIXlt(g$data[[a]], 
                                              origin = the.epoch), "%Y-%m-%d %H:%M:%S"), 
                          silent = TRUE)
          pdata.vec <- strftime(as.POSIXlt(g$prestats.data[[a]], 
                                           origin = the.epoch), "%Y-%m-%d %H:%M:%S")
        }
        else if (inherits(data.vec, "Date")) {
          data.vec <- try(strftime(as.Date(g$data[[a]], 
                                           origin = the.epoch), "%Y-%m-%d %H:%M:%S"), 
                          silent = TRUE)
          pdata.vec <- strftime(as.Date(g$prestats.data[[a]], 
                                        origin = the.epoch), "%Y-%m-%d %H:%M:%S")
        }
        else if (inherits(data.vec, c("character", "factor"))) {
          data.vec <- as.factor(data.vec)
          g$data <- g$data[order(g$data[[a]]), ]
          vec.i <- match(g$data[[a]], as.numeric(data.vec))
          if (anyNA(vec.i)) {
            vec.i <- match(g$data[[a.name]], data.vec)
          }
          data.vec <- data.vec[vec.i]
          g$prestats.data <- g$prestats.data[order(g$prestats.data[[a]]), 
                                             ]
          pvec.i <- match(g$prestats.data[[a]], as.numeric(pdata.vec))
          pdata.vec <- pdata.vec[pvec.i]
          if (length(pdata.vec) == length(data.vec)) 
            pdata.vec <- data.vec
          if (!is.factor(pdata.vec)) 
            pdata.vec <- g$prestats.data[[a.name]]
        }
        g$data[[a]] <- data.vec
        g$prestats.data[[a]] <- pdata.vec
      }
    }
  }
  g$params <- c(l$geom_params, l$stat_params)
  for (p.name in names(g$params)) {
    names(g$params[[p.name]]) <- NULL
  }
  convert <- plotly:::toBasic[[g$geom]]
  basic <- if (is.function(convert)) 
    convert(g)
  else g
  data.list <- if (basic$geom %in% names(plotly:::markSplit)) {
    mark.names <- plotly:::markSplit[[basic$geom]]
    name.names <- sprintf("%s.name", mark.names)
    is.split <- names(basic$data) %in% c(name.names, "PANEL")
    if (any(is.split)) {
      data.i <- which(is.split)
      matched.names <- names(basic$data)[data.i]
      name.i <- name.names %in% matched.names
      invariable.names <- cbind(name.names, mark.names)[name.i, 
                                                        ]
      other.names <- !names(basic$data) %in% invariable.names
      vec.list <- basic$data[is.split]
      df.list <- split(basic$data, vec.list, drop = TRUE)
      lapply(df.list, function(df) {
        params <- basic$params
        params[invariable.names] <- if (ncol(x <- df[1, 
                                                     invariable.names]) > 0) 
          x
        else NULL
        list(data = df[other.names], params = params)
      })
    }
  }
  if (is.null(data.list)) {
    data.list <- structure(list(list(data = basic$data, 
                                     params = basic$params)), names = basic$params$name)
  }
  getTrace <- plotly:::geom2trace[[basic$geom]]
  if (is.null(getTrace)) {
    getTrace <- plotly:::geom2trace[["blank"]]
    warning("geom_", g$geom, " has yet to be implemented in plotly.\n", 
            "  If you'd like to see this geom implemented,\n", 
            "  Please open an issue with your example code at\n", 
            "  https://github.com/ropensci/plotly/issues")
  }
  traces <- NULL
  names.in.legend <- NULL
  for (data.i in seq_along(data.list)) {
    data.params <- data.list[[data.i]]
    data.params$params$stat.type <- plotly:::type(l, "stat")
    data.params$params <- modifyList(plotly:::dat2params(data.params$data), 
                                     data.params$params)
    tr <- do.call(getTrace, data.params)
    for (v.name in c("x", "y")) {
      vals <- tr[[v.name]]
      if (length(vals) > 0 && is.na(vals[length(vals)])) {
        tr[[v.name]] <- vals[-length(vals)]
      }
    }
    name.names <- grep("[.]name$", names(data.params$params), 
                       value = TRUE)
    not.group <- grep("group", name.names, value = TRUE, 
                      invert = TRUE)
    if (length(not.group)) {
      for (a.name in not.group) {
        a <- sub("[.]name$", "", a.name)
        tr$sort[[a.name]] <- if (a %in% names(misc$breaks)) {
          a.value <- as.character(data.params$params[[a.name]])
          ranks <- misc$breaks[[a]]
          if (a.value %in% names(ranks)) {
            ranks[[a.value]]
          }
          else {
            Inf
          }
        }
        else {
          1
        }
      }
      name.list <- data.params$params[not.group]
      tr$name <- paste(unlist(name.list), collapse = ".")
      if (length(unique(name.list)) < 2) 
        tr$name <- as.character(name.list[[1]])
    }
    dpd <- data.params$data
    if ("PANEL" %in% names(dpd) && nrow(dpd) > 0) {
      tr$xaxis <- paste0("x", dpd[1, "COL"])
      tr$yaxis <- paste0("y", dpd[1, "plotly.row"])
    }
    if (is.null(tr$name) || tr$name %in% names.in.legend) 
      tr$showlegend <- FALSE
    names.in.legend <- c(names.in.legend, tr$name)
    if (g$geom %in% c("bar","boxplot")) {
      is_hist <- misc$is.continuous[["x"]]
      tr$bargap <- if (is_hist) 
        0
      else "default"
      pos <- plotly:::type(l, "position")
      tr$barmode <- if (pos %in% "identity" && is_hist) {
        "overlay"
      }
      else if (pos %in% c("identity", "stack", "fill")) {
        "stack"
      }
      else {
        "group"
      }
    }
    traces <- c(traces, list(tr))
  }
  sort.val <- sapply(traces, function(tr) {
    rank.val <- unlist(tr$sort)
    if (is.null(rank.val)) {
      0
    }
    else if (length(rank.val) == 1) {
      rank.val
    }
    else {
      0
    }
  })
  if (g$geom %in% c("area", "density") && plotly:::type(l, "position") == 
      "stack") {
    traces <- rev(traces)
  }
  else {
    traces
  }
  ord <- order(sort.val)
  no.sort <- traces[ord]
  for (tr.i in seq_along(no.sort)) {
    s <- no.sort[[tr.i]]$sort
    no.sort[[tr.i]]$showlegend <- if (is.numeric(s)) {
      if (s == Inf) {
        FALSE
      }
      else {
        TRUE
      }
    }
    else {
      FALSE
    }
    no.sort[[tr.i]]$sort <- NULL
  }
  if (isTRUE(misc$smoothLine)) {
    c(layer2traces(l, d, misc), no.sort)
  }
  else {
    no.sort
  }
}
