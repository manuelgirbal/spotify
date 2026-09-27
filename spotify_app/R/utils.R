with_notify <- function(expr, msg) {
  tryCatch(expr, error = function(e) {
    showNotification(paste0(msg, ": ", conditionMessage(e)), type = "error", duration = NULL)
    NULL
  })
}
