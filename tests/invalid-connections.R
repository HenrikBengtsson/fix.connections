library(fix.connections)

stopifnot(isValidConnection(stdin()))
stopifnot(isValidConnection(stdout()))
stopifnot(isValidConnection(stderr()))

con1 <- file(tempfile(), open = "w")
stopifnot(isValidConnection(con1))
close(con1)
stopifnot(!isValidConnection(con1))

con2 <- file(tempfile(), open = "w")
stopifnot(isValidConnection(con2))

stopifnot(!isValidConnection(con1))

