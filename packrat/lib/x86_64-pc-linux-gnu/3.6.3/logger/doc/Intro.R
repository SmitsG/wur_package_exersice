## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(logger)
## backup settings
oldconf <- list(
    threshold = log_threshold(),
    layout = log_layout(),
    formatter = log_formatter(),
    appender = log_appender())
## knitr not picking up stderr
log_appender(appender_stdout)

## -----------------------------------------------------------------------------
library(logger)
log_info('Loading data')
data(mtcars)
log_info('The dataset includes {nrow(mtcars)} rows')
if (max(mtcars$hp) < 1000) {
    log_warn('Oh, no! There are no cars with more than 1K horsepower in the dataset :/')
    log_debug('The most powerful car is {rownames(mtcars)[which.max(mtcars$hp)]} with {max(mtcars$hp)} hp')
}

## -----------------------------------------------------------------------------
log_threshold()

## -----------------------------------------------------------------------------
log_threshold(TRACE)

## -----------------------------------------------------------------------------
log_info('Loading data')
data(mtcars)
log_info('The dataset includes {nrow(mtcars)} rows')
if (max(mtcars$hp) < 1000) {
    log_warn('Oh, no! There are no cars with more than 1K horsepower in the dataset :/')
    log_debug('The most powerful car is {rownames(mtcars)[which.max(mtcars$hp)]} with {max(mtcars$hp)} hp')
}

## -----------------------------------------------------------------------------
f <- sqrt
g <- mean
x <- 1:31
log_eval(y <- f(g(x)), level = INFO)
str(y)

## ----cleanup, include = FALSE-------------------------------------------------
## restore settings
log_threshold(oldconf$threshold)
log_layout(oldconf$layout)
log_formatter(oldconf$formatter)
log_appender(oldconf$appender)

