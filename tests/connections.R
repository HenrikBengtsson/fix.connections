library(fix.connections)

con <- file(tempfile(), open = "w")

for (idx in getAllConnections()) {
  con <- getConnection(idx)
  print(con)
  print(connectionId(con))
  print(connectionInfo(con))
}

close(con)
