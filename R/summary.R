#' summary.AlternateLasso
#'
#' @param obj    "AlternateLasso" class object.
#' @rdname AlternateLasso
#'
#' @export

summary.AlternateLasso <- function(obj){
  stopifnot(any(class(obj) ==  "AlternateLasso"))

  cbind(feature = obj$original$feature, coef = obj$original$coef) %>%
    print()

  for(i in 1:NROW(obj$alternatives)){
    this <- obj$alternatives[[i]]
    catf("Feature: [ %s ], Coef. = %f, Aiternative: %i", this$feature, this$coef, NROW(this$alternatives))
    # catf("Obj0 = %f", this$objective)
    if(is.null(this$alternatives)){
      catf("\t ** No Alternate Features **")
    }

    for(j in 1:NROW(this$alternatives)){
      this.alt <- this$alternatives[j, ]
      catf("\t Alternate Feature: %s, \tCoef. = %f, Score = %f",
           this.alt$feature, this.alt$coef, this.alt$objective - obj$original$objective)
    }
  }
}

