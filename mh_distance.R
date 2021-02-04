mh <- function(x,y) {
  if (typeof(x) == "logical") {
    if (x != y) {
      return (1)}
    else return(0)}
  else if (typeof(x)=="character") {
    xlist <- unlist(strsplit(x, split = ""))
    ylist <- unlist(strsplit(y, split = ""))
    dis <- (xlist==ylist)
    return(length(dis)-sum(dis))}
  else if (any(typeof(x) == c("double","integer"))) {
    xdigits <- unlist(strsplit(as.character(x), split = ""))
    ydigits <- unlist(strsplit(as.character(y), split = ""))
    disn <- (xdigits == ydigits)
    return((length(disn)- sum(disn)))
  }
}

mh_distance <- function(x, y) {
  if (typeof(x) != typeof(y)) {
    warning("Vectors are not of the same type")
    return(-1)}
  else if (nchar(x) != nchar(y) & typeof(x) !="logical" &
           typeof(y)!="logical") {
    warning("x and y are different lengths")
    return(-1)
  }
  else if (is.na(x) | is.na(y)) {
    warning("x or y is NA")
    return(-1)}
  else if (is.nan(x) | is.nan(y)) {
    warning("x or y is NaN")
    return(-1)}
  else if (is.infinite(x) | is.infinite(y)) {
    warning("x or y is Inf/-Inf")
    return(-1)}
  else if (all(typeof(x) != c("logical", "character", "integer", "double"))) {
    warning("x is not logical/character/numeric")
    return(-1)}
  else if (all(typeof(y) != c("logical", "character", "integer", "double"))) {
    warning("y is not logical/character/numeric")
    return(-1)}
  else if (typeof(x)=="double" | typeof(y) == "double"){
    if (x %% 1 !=0 | y%%1 !=0) {
      warning("Decimals in x or y")
      return(-1)}
    else return(mh(x,y))}
  else return(mh(x,y))
}