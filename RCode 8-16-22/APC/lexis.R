lexis <- function(Rates, ...) {
  # Checkerboard plot of rates by age and time
  #
  # lexis(Rates, ...)
  #
  # Arguments:
  # Rates             - rates object
  # AxisLabels        - none (default), "x", "y", or c("x","y")
  # Grid              - TRUE or FALSE (default)
  # Title             - string containing plot title
  # plot.title.size   - scalar (default 10)
  # legend.title.size - scalar (default 8)
  # axis.title.size   - scalar (default 12)
  # legend            - TRUE (default) or FALSE 
  #
  # See also: rates.
  
  Inputs <- lexis_INPUTS(Rates, ...)

  R <- nrow(Rates$events)
  C <- ncol(Rates$events)
  RC <- R *C

  x0 <- 1:C
  y0 <- 1:R

  XC <- meshgrid(x0, y0)$X
  XR <- meshgrid(x0, y0)$Y

  xC <- matrix(data = as.integer(XC), nrow = RC, ncol = 1)
  xR <- matrix(data = as.integer(XR), nrow = RC, ncol = 1)

  X <- xC
  Y <- xR
  
  t <- Rates$events / Rates$offset * 100000
  t_comp <- t[!is.na(t)]
  
  rng <- quantile(t_comp, probs = c(.025, .975))

  t[which(t < rng[[1]])] = rng[[1]]
  t[which(t > rng[[2]])] = rng[[2]]

  Data_t <- matrix(t, nrow = RC, ncol = 1)
  Name_t <- Inputs$Title

  # Data frame for plotting.
  df_t <- data.frame("X" = X, "Y" = Y, "Data" = Data_t)

  plot_t <- ggplot(df_t, aes(X, Y, fill = Data)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("blue4", "blue", "cornflowerblue", "cyan", "lightgreen", "yellow", "orange", "red", "red4"), 
                         limits = c(floor(rng[1]), ceiling(rng[2])),
                         name = paste(" Rate\n per\n", as.character(Rates$offset_tick), "\n person-\n years"),
                         na.value = "white") +
    labs(x = "", y = "", title =  Name_t) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = Inputs$plot.title.size, face = "bold"),
          legend.title = element_text(size = Inputs$legend.title.size),
          panel.background = element_rect(fill = "white", colour = "white"),
          aspect.ratio = 2) +
    border(col = "grey")
  
  if (Inputs$legend == FALSE) {
    plot_t <- plot_t +
      theme(legend.position = "none",
            legend.title = element_blank()) 
  }

  if (Inputs$AxisLabels == 1 | Inputs$AxisLabels == 3) {
    plot_t = plot_t +
      scale_x_continuous(labels = as.character(Rates$periods[seq(1, length(Rates$periods), by = Inputs$Delta)]), 
                         breaks = seq(1, length(Rates$periods), by = Inputs$Delta) - 0.5) +
      theme(axis.text.x = element_text(size = Inputs$axis.text.size, angle=90),
            axis.title.x = element_text(size = Inputs$axis.title.size),
            axis.ticks.x = element_line()) +
      labs(x = "Year")
    }
  
  if (Inputs$AxisLabels == 2 | Inputs$AxisLabels == 3) {
      plot_t <- plot_t +
      scale_y_continuous(labels = as.character(Rates$ages[seq(1, length(Rates$ages), by = Inputs$Delta)]), 
                         breaks = seq(1, length(Rates$ages), by = Inputs$Delta) - 0.5) +
      theme(axis.text.y = element_text(size = Inputs$axis.text.size),
            axis.title.y = element_text(size = Inputs$axis.title.size, vjust = 4),
            axis.ticks.y = element_line()) +
      labs(y = "Age")
  }

  if (Inputs$Grid == TRUE) {
    plot_t <- plot_t + 
      geom_tile(col = "black")
  }

  plot_t

}

lexis_INPUTS <- function(Rates, ...) {
  # Helper. Process input variables and property-value pairs.
  Inputs <- list(AxisLabels = 0,
                Grid = FALSE,
                Delta = 1,
                Title = Rates$fullname,
                plot.title.size = 10,
                legend.title.size = 8,
                axis.text.size = 8,
                axis.title.size = 12,
                legend = TRUE)
  
  # Set delta.
  Inputs$Delta = Rates$ages[2] - Rates$ages[1]

  lArgin <- list(...)

  while (length(lArgin)>0) {
    propi <- names(lArgin)[[1]]
    vali <- lArgin[[1]]
    lArgin <- lArgin[-1]

    if (tolower(propi) == "axislabels") {
      vals <- c("x", "y")
      if (is.character(vali)) {
        if (size(vali)[1] > 1) {
          stop("The Shape property-value must be a string, not a character matrix.")
        }
        for(i in 1:length(vali)) {
          if (!any(vali[i] == vals[1:2])) {
            stop(vali, " is an unrecognized property-value.")
            }
        }
        if (length(vali) == 1) {
          if (vali == "x") {
            Inputs$AxisLabels <- 1
          } else if (vali == "y") {
            Inputs$AxisLabels <- 2
          } 
        } else {
          Inputs$AxisLabels <- 3
        }
      } 
      else {
        stop("The property-value is not recognized")
      }
    }  else if (tolower(propi) == "grid") {
      if (!is.logical(vali)) {
        stop("Labels must be logical - TRUE or FALSE.")
      }
      Inputs$Grid <- vali
    }  else if (tolower(propi) == "title") {
      if (!is.character(vali)) {
        stop("Title must be a string.")
    }
    Inputs$Title <- vali
    } else if (tolower(propi) == "plot.title.size") {
      if (!is.numeric(vali) | length(vali) > 1) {
        stop("Plot title size must be a scalar.")
      } 
      Inputs$plot.title.size <- vali
    } else if (tolower(propi) == "legend.title.size") {
      if (!is.numeric(vali) | length(vali) > 1) {
        stop("Legend title size must be a scalar.")
      } 
      Inputs$legend.title.size <- vali
    } else if (tolower(propi) == "axis.text.size") {
      if (!is.numeric(vali) | length(vali) > 1) {
        stop("Axis text size must be a scalar.")
      } 
      Inputs$axis.text.size <- vali
    } else if (tolower(propi) == "axis.title.size") {
      if (!is.numeric(vali) | length(vali) > 1) {
        stop("Axis title size must be a scalar.")
      } 
      Inputs$axis.title.size <- vali
    } else if (tolower(propi) == "legend") {
      if (!is.logical(vali)) {
        stop("legend must be logical - TRUE or FALSE.")
      }
      Inputs$legend <- vali
    }
  }
  return(Inputs)
}
