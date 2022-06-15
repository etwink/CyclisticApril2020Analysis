normalize_vector <- function(vec = c()) {
  process_vec <- preProcess(as.data.frame(vec), method=c("range"))
  norm_scale_vec <- unlist(as.vector(predict(process_vec, as.data.frame(vec))), use.names = FALSE)
  return(norm_scale_vec)
}