# Funciones auxiliares para gráficos
# Utilidades para crear visualizaciones

#' Crear gráfico de descomposición
#' @param decomp Objeto de descomposición
#' @return ggplot object
plot_decomposition <- function(decomp) {
  # Convertir componentes a data.frame
  df <- data.frame(
    time = time(decomp$x),
    observed = as.numeric(decomp$x),
    trend = as.numeric(decomp$trend),
    seasonal = as.numeric(decomp$seasonal),
    random = as.numeric(decomp$random)
  )

  # Gráfico con ggplot2
  ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = observed), color = "#7BA7C9") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Decomposition", x = "Time", y = "Value")
}

#' Crear gráfico ACF/PACF
#' @param ts_data Serie temporal
#' @param type "acf" o "pacf"
#' @return ggplot object
plot_acf_pacf <- function(ts_data, type = "acf") {
  if (type == "acf") {
    acf_obj <- acf(ts_data, plot = FALSE)
  } else {
    acf_obj <- pacf(ts_data, plot = FALSE)
  }

  df <- data.frame(
    lag = acf_obj$lag,
    acf = acf_obj$acf
  )

  ggplot2::ggplot(df, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = if (type == "acf") "Autocorrelation Function" else "Partial Autocorrelation Function",
      x = "Lag",
      y = if (type == "acf") "ACF" else "PACF"
    )
}

#' Aplicar tema consistente a gráficos plotly
#' @param p Objeto plotly
#' @return Objeto plotly con tema aplicado
apply_plotly_theme <- function(p) {
  p %>%
    plotly::layout(
      font = list(family = "Inter"),
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      xaxis = list(gridcolor = "#F5F0EB"),
      yaxis = list(gridcolor = "#F5F0EB")
    )
}

# Static plots used by HTML/PDF reports -------------------------------------

#' Original time series (ggplot, for embedding in reports)
plot_report_original <- function(ts_data) {
  df <- data.frame(
    time = as.numeric(time(ts_data)),
    value = as.numeric(ts_data)
  )
  ggplot2::ggplot(df, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(color = "#7BA7C9", linewidth = 0.7) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(title = "Original Time Series", x = "Time", y = "Value") +
    ggplot2::theme(plot.title = ggplot2::element_text(color = "#5C3D99", face = "bold"))
}

#' Seasonal index bar plot
plot_report_seasonal <- function(decomposition, decomp_type) {
  freq <- frequency(decomposition$x)
  seasonal_vals <- as.vector(decomposition$seasonal)[1:freq]
  period_names <- if (freq == 12) {
    c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  } else {
    paste0("Q", 1:freq)
  }
  ref_line <- if (identical(decomp_type, "multiplicative")) 1 else 0
  df <- data.frame(
    period = factor(period_names, levels = period_names),
    index = seasonal_vals
  )
  ggplot2::ggplot(df, ggplot2::aes(x = period, y = index)) +
    ggplot2::geom_col(fill = "#7BA7C9") +
    ggplot2::geom_hline(yintercept = ref_line, color = "#888", linetype = "dashed") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::labs(title = "Seasonal Index", x = NULL, y = "Index") +
    ggplot2::theme(plot.title = ggplot2::element_text(color = "#5C3D99", face = "bold"))
}

#' Autoregression residuals diagnostic (base R, 2 panels)
plot_report_autoreg <- function(ar) {
  resid <- as.numeric(residuals(ar$model, type = "response"))
  op <- par(mfrow = c(2, 1), mar = c(4, 4, 2.5, 1))
  on.exit(par(op))
  plot(resid, type = "l", col = "#B092C5", lwd = 1.4,
       main = "Autoregression Residuals", xlab = "Index", ylab = "Residual")
  abline(h = 0, col = "gray60", lty = 2)
  acf_obj <- acf(resid, plot = FALSE)
  plot(acf_obj, main = "Residuals ACF", col = "#7BA7C9", lwd = 2)
}
