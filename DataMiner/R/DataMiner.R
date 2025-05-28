#' @keywords internal
"_PACKAGE"

#' DataMiner: A Simple Data Analysis Package
#'
#' @description
#' Provides basic descriptive statistics and visualization functions
#' for exploratory data analysis.
#'
#' @details
#' Main functions include:
#' \itemize{
#'   \item \code{desc_stats()} - descriptive statistics
#'   \item \code{plot_distribution()} - diagnostic plots
#'   \item \code{plot_correlations()} - correlation matrix
#' }
#' @name DataMiner
NULL

#' Sample dataset
#'
#' A randomly generated dataset for demonstration purposes
#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{id}{Unique identifier}
#'   \item{value}{Numeric values}
#'   \item{category}{Grouping factor}
#' }
#' @source Generated internally
#' @export
sample_data <- data.frame(
  id = 1:100,
  value = rnorm(100),
  category = sample(c("A", "B", "C"), 100, replace = TRUE)
)

#' Расчет описательной статистики
#'
#' @param data Датафрейм
#' @param cols Вектор колонок (по умолчанию все числовые)
#' @return Список со статистиками
#' @export
#' @examples
#' data(mtcars)
#' desc_stats(mtcars)
desc_stats <- function(data, cols = NULL) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  if (is.null(cols)) {
    cols <- names(data)[sapply(data, is.numeric)]
  }

  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found: ", paste(missing_cols, collapse = ", "))
  }

  stats_list <- lapply(cols, function(col) {
    x <- data[[col]]
    list(
      n = length(x),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      na_count = sum(is.na(x)),
      q25 = quantile(x, 0.25, na.rm = TRUE),
      q75 = quantile(x, 0.75, na.rm = TRUE)
    )
  })

  names(stats_list) <- cols
  return(stats_list)
}

#' Format statistics as data frame
#'
#' @param stats_list Result from desc_stats()
#' @return Formatted data frame
#' @export
format_stats <- function(stats_list) {
  if (!is.list(stats_list)) {
    stop("Input must be a list from desc_stats()")
  }
  do.call(rbind, lapply(stats_list, as.data.frame))
}

#' Create diagnostic plots
#'
#' @param data Input data frame
#' @param col Name of numeric column to plot
#' @param bins Number of histogram bins (default: 30)
#' @return Combined histogram and boxplot
#' @export
#' @examples
#' plot_distribution(mtcars, "mpg")
plot_distribution <- function(data, col, bins = 30) {
  if (!col %in% names(data)) {
    stop("Column not found in data")
  }

  old_par <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
  on.exit(par(old_par))

  hist(data[[col]], breaks = bins, main = paste("Histogram of", col),
       xlab = col, col = "lightblue", probability = TRUE)
  lines(density(data[[col]], na.rm = TRUE), col = "red", lwd = 2)

  boxplot(data[[col]], main = paste("Boxplot of", col),
          col = "lightgreen", outpch = 19, outcol = "red")
}

#' Create correlation plot
#'
#' @param data Data frame with numeric columns
#' @param method Correlation method ("pearson", "kendall", "spearman")
#' @return Heatmap of correlations
#' @export
#' @examples
#' plot_correlations(mtcars)
plot_correlations <- function(data, method = "pearson") {
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 2) {
    stop("Need at least 2 numeric columns")
  }

  cor_matrix <- cor(data[, numeric_cols], use = "complete.obs", method = method)

  heatmap(cor_matrix,
          col = colorRampPalette(c("blue", "white", "red"))(20),
          symm = TRUE,
          margins = c(10, 10),
          main = "Correlation Matrix")
}

# Internal utility function (not exported)
validate_numeric <- function(x) {
  is.numeric(x) && !all(is.na(x))
}

.onLoad <- function(libname, pkgname) {
  # Make sample data available in package namespace
  assign("sample_data", sample_data, envir = parent.env(environment()))
}
