#' @export
nbr_of_connections <- function() {
  length(getAllConnections())
}

#' @export
list_connections <- function() {
  showConnections(all = TRUE)
}

#' @export
get_connection <- function(what) {
  if (inherits(what, "connection")) return(what)
  
  stop_if_not(length(what) == 1L, !is.na(what))
  if (is.numeric(what)) what <- as.integer(what)

  if (is.integer(what)) {
    if (what < 0L || what > nbr_of_connections() - 1L) {
      stopf("Connection index 'what' out of range [0,%d]: %s",
            nbr_of_connections() - 1L, what)
    }
  } else {
    stop("Unknown type of argument 'what': ", mode(what))
  }
  
  getConnection(what)
}

#' @export
get_annotated_connection <- function(what) {
  if (inherits(what, "annotated_connection")) return(what)
  con <- get_connection(what)
  structure(con, details = connection_details(con),
                 class = c("annotated_connection", class(con)))
}


#' @importFrom digest digest
#' @export
connection_details <- function(con, drop = "opened", fix_names = TRUE) {
  if (inherits(con, "annotated_connection")) {
    details <- attr(con, "details")
    stop_if_not(is.list(details))
  } else {
    con <- get_connection(con)
    index <- as.integer(con)
    if (index > nbr_of_connections() - 1L) {
      stopf("No such connection: index out of range [0,%d]: %d", nbr_of_connections() - 1L, index)
    }
    details <- summary(con)
  
    ## Generate ID checksum excluding 'opened' status
    t <- details
    t$opened <- NULL
    id <- digest(t, algo = "crc32")
    details <- c(list(index = index, id = id), details)
  }
  
  names <- names(details)
  if (fix_names) {
    names <- gsub(" ", "_", names)
    names(details) <- names
  }
  details <- details[setdiff(names, drop)]
  structure(details, class = c("annotated_connection_details", class(details)))
}

#' @export
connection_info <- function(con) {
  if (inherits(con, "annotated_connection_details")) {
    details <- con
  } else {
    details <- connection_details(con)
  }
  info <- sapply(details, FUN = paste0, collapse = " ")
  info <- sprintf("%s=%s", names(details), info)
  info <- paste(info, collapse = ", ")
  info <- sprintf("connection: %s", info)
  info
}


#' @export
is_connection_valid <- function(con) {
  res <- TRUE
  
  if (inherits(con, "annotated_connection")) {
    details <- connection_details(con)
    if (details$index > nbr_of_connections() - 1L) {
      res <- FALSE
      attr(res, "reason") <- sprintf("Connection index is out of range [0,%d]: %d", nbr_of_connections() - 1L, details$index)
    } else {
      index <- as.integer(con)
      current_details <- connection_details(index)
      res <- identical(details, current_details)
      if (!isTRUE(res)) {
        attr(res, "reason") <- sprintf("Connection (%s) is different from the currently registered R connection with index %d (%s)", connection_info(con), index, connection_info(current_details))
      }
    }
  } else {
    stop_if_not(inherits(con, "connection"))
  }
  
  res
}
