#' print.AlternateLasso
#'
#' @rdname AlternateLasso
#' @param obj    "AlternateLasso" class object.
#'
#' @export

print.AlternateLasso <- function(obj){
  stopifnot(any(class(obj) ==  "AlternateLasso"))

  for(i in 1:NROW(obj$alternatives)){
    this <- obj$alternatives[[i]]
    catf("[ %s ] has [ %d ] alternatives", this$feature,  NROW(this$alternatives))
  }
}
