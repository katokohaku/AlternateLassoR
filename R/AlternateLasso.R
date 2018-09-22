
#' Alternate Lasso Feature Selection
#'
#' R package for the alternate features search proposed in the following paper.
#' S. Hara, T. Maehara, Finding Alternate Features in Lasso, arXiv:1611.05940, 2016.
#' Complete and original Python code is available from https://github.com/sato9hara/LassoVariants
#'
#' @name AlternateLasso
#'
#' @param X            [matrix] Desgin matrix
#' @param y            [vector] Response valuables
#' @param model        \code{\link{glmnet}} class object.
#' @param rho          [numeric] Cut-off value. See \code{lambda} of \code{\link{glmnet}}
#' @param featurename  [character vector] If NULL, set colnames(X)
#' @param verbose      Show message during fit. default is \code{TRUE.}
#' @param ...          Other options path to \code{\link{pforeach}}
#'
#' @return             "AlternateLasso" class object. A list of original model and alternative features.
#'
#' @importFrom stats coef
#' @export

AlternateLasso <- function(X, y, model = NULL, rho = 0.1,
                           featurename = NULL, verbose = TRUE, ...){

  stopifnot(
    !missing(X), !missing(y), !is.null(model),
    any(class(model) ==  "glmnet"))

  if(!is.null(featurename)){
    colnames(X) <- featurename
  }

  featurename <- colnames(X)
  if(is.null(featurename)){
    featurename <- paste0("X",1:NCOL(X))
  }

  # original coefficients by lasso
  coeffs.orig <- as.numeric(coef(model, s=rho))[-1]
  # Intercept
  b <- as.numeric(coef(model, s=rho))[1]
  # objective function value of original lasso model
  obj.orig <- getObjective(X, y, coeffs.orig, b, rho)

  nonzeros <- which(abs(coeffs.orig) > 0)

  # iterative search for alternative features of non-zero features
  p <- getP(X, y, coeffs.orig, b)
  # print("p"); print(p)
  q <- colMeans(X^2)
  # print("q"); print(q)

  alternatives <- pforeach::npforeach(d = nonzeros, .c=list)({

    coeffs.this <- coeffs.orig
    coeffs.this[d] <- 0
    obj0 <- AlternateLassoR:::getObjective(X, y, coeffs.this, b, rho)


    g <- t(X) %*% X[, d] / NROW(X) # getG() in original code
    pp <- p - g * coeffs.orig[d] # remove effect of the target coeffcient.

    # soft-thresholding operator
    r  <-  pp / q
    rr <- abs(r) - rho / q

    alt.coeffs <- sign(- r) * ifelse(rr > 0, rr, 0)
    alt.coeffs[nonzeros] <- 0
    alt.coeffs[is.nan(alt.coeffs)] <- 0
    alt.coeffs # da

    nonzero.pos <- which(abs(alt.coeffs) > 0)
    if(verbose){
      catf("[ %s ] has [ %d ] alternatives", featurename[d], length(nonzero.pos))
    }

    alt.this <- pforeach::npforeach(d.alt = nonzero.pos,
                                   .combine=rbind, ...)({

      coeffs.this[d.alt] <- alt.coeffs[d.alt]

      obj.tmp <- AlternateLassoR:::getObjective(X, y, coeffs.this, b, rho)
      coeffs.this[d.alt] <- 0

      df <-data.frame(
        feature   = featurename[d.alt],
        coef      = alt.coeffs[d.alt] ,
        objective = obj.tmp,
        stringsAsFactors = FALSE)

      return(df)
    })

    list(
      feature = featurename[d],
      coef    = coeffs.orig[d],
      objective    = obj0,
      alternatives = alt.this)
  })
  original <- list(
    feature  = featurename,
    coef     = coeffs.orig,
    objective= obj.orig
  )

  result <- list(model = model,
                 rho = rho,
                 original = original,
                 alternatives = alternatives)
  class(result) <- c(class(result), "AlternateLasso")

  return(result)
}


#' partial function for Lasso model

getP <- function(x, y, a, b){
  n.obs <- NROW(y)
  p <- - t(x) %*% (y - b) / n.obs # getH() in original code

  for(d in which(abs(a) > 0)){
    g <- t(x) %*% x[, d] / n.obs # getG() in original code
    p <- p + g * a[d]
  }
  return(p)
}


#' Objective function of regularized linear model

getObjective <- function(x, y, a, b, rho){
  z <- x %*% a + b
  f <- mean((y - z)^2) / 2
  return( f + rho * sum(abs(a)) )
}


