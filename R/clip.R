### Send a table-like object to clipboard (Windows only).
clip  <- function(x, sep = "\t", row.names = FALSE, quote = FALSE, ...){
  x.out <- write.table(x, file = "clipboard", sep = sep, row.names = row.names, quote = quote, ...)
}
