#' Gets the internal ID of a connection
#'
#' @param con A [base::connection].
#'
#' @return A positive integer.
#'
#' @importFrom utils capture.output
#' @export
connectionId <- function(con) {
  stop_if_not(inherits(con, "connection"))
  
  ## stdin, stdout, or stderr?
  index <- as.integer(con)
  if (index <= 2) return(index)

  id <- attr(con, "conn_id")
  id <- capture.output(print(id))

  ## If a 'connection' object has been serialized, we get a 'nil' pointer.
  if (id == "<pointer: (nil)>") return(-1L)
  
  id <- gsub("(<pointer:| |>)", "", id)
  id <- strtoi(id, base = 16L)
  
  id
}


#' Gets a string summary of a connection
#'
#' @param con A [base::connection].
#'
#' @return A character string.
#'
#' @export
connectionInfo <- function(con) {
  index <- as.integer(con)
  if (is.element(index, getAllConnections())) {
    details <- summary(con)
  } else {
    details <- as.list(rep(NA_character_, times = 7L))
    names(details) <- c("description", "class", "mode", "text", "opened", "can read", "can write")
  }
  details$id <- connectionId(con)
  info <- unlist(lapply(details, FUN = function(x) {
    if (is.character(x)) paste0('"', x, '"') else x
  }), use.names = FALSE)
  info <- sprintf("%s=%s", names(details), info)
  info <- paste(info, collapse = ", ")
  info <- sprintf("connection: %s", info)
  info
}


#' Checks whether a connection is valid or not
#'
#' @param con A [base::connection].
#'
#' @return Returns TRUE if the connection is valid, otherwise FALSE.
#' If FALSE, the attribute `reason` explains why it is invalid.
#'
#' @export
isValidConnection <- function(con) {
  stop_if_not(inherits(con, "connection"))
  index <- as.integer(con)
  
  ## stdin, stdout, or stderr?
  if (index <= 2) return(TRUE)

  ## No such connection index?
  if (!is.element(index, getAllConnections())) {
    res <- FALSE
    attr(res, "reason") <- sprintf("Connection (%s) is no longer valid. There is currently no registered R connection with that index %d", connectionInfo(con), index)
    return(res)
  }

  ## That connection is no longer registered?
  current_con <- getConnection(index)
  res <- identical(attr(con, "conn_id"), attr(current_con, "conn_id"))
  if (!isTRUE(res)) {
    attr(res, "reason") <- sprintf("Connection (%s) is no longer valid. It differ from the currently registered R connection with the same index %d (%s)", connectionInfo(con), index, connectionInfo(current_con))
    return(res)
  }

  ## A valid connection
  TRUE
}


#' Asserts that a connection is valid
#'
#' @param con A [base::connection].
#'
#' @return Nothing or produces an informative error if the connection
#' is invalid.
#'
#' @export
assertValidConnection <- function(con) {
  isValid <- isValidConnection(con)
  if (isTRUE(isValid)) return()
  reason <- attr(isValid, "reason", exact = TRUE)
  stop(reason)
}
