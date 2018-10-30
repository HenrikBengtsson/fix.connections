library(fix.connections)

message("* isValidConnection() ...")

message("- stdin, stdout, stderr")
stopifnot(isValidConnection(stdin()))
stopifnot(connectionId(stdin()) == 0L)

stopifnot(isValidConnection(stdout()))
stopifnot(connectionId(stdout()) == 1L)

stopifnot(isValidConnection(stderr()))
stopifnot(connectionId(stderr()) == 2L)

message("- file connection")
con <- file(tempfile(), open = "w")
stopifnot(isValidConnection(con))
stopifnot(connectionId(con) > 2L)
close(con)
stopifnot(!isValidConnection(con))

message("- exception: overridden file connection")
con2 <- file(tempfile(), open = "w")
stopifnot(isValidConnection(con2))
stopifnot(connectionId(con2) > 2L)
stopifnot(!isValidConnection(con))

message("- exception: serialized file connection")

con <- file(tempfile(), open = "w")
stopifnot(isValidConnection(con))
stopifnot(connectionId(con) > 2L)

tf <- tempfile()
saveRDS(con, file = tf)
con2 <- readRDS(tf)
print(con2)
stopifnot(!isValidConnection(con2))
print(attr(con2, "conn_id")) ## <pointer: (nil)>
stopifnot(connectionId(con2) == -1L)

message("* isValidConnection() ... DONE")
