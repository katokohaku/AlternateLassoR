#' Same function as 'catf()' in BBmisc except for returning [str]
#'
#' @param ...     See sprintf
#' @param file    See cat. Default is ""
#' @param append  See cat. Default is FALSE.
#' @param newline Append newline at the end? Default is TRUE.
#'
#' @export

catf <- function (..., file = "", append = FALSE, newline = TRUE)
{
  msg <- sprintf(...)
  cat(msg, ifelse(newline, "\n", ""),
      sep = "", file = file, append = append)
  invisible(msg)
}


#' partial function for Lasso model
#'
#' @description internal
#' @param x  [matrix]    Desgin matrix
#' @param y  [vector]    Response valuables
#' @param a  [vector]    Coeffcients.
#' @param b  [numeric]   an intercept

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
#'
#' @description internal
#' @param x  [matrix]    Desgin matrix
#' @param y  [vector]    Response valuables
#' @param a  [vector]    Coeffcients.
#' @param b  [numeric]   an intercept

getObjective <- function(x, y, a, b, rho){
  z <- x %*% a + b
  f <- mean((y - z)^2) / 2
  return( f + rho * sum(abs(a)) )
}


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
#'
#' @return             "AlternateLasso" class object. A list of original model and alternative features.
#'
#' @examples
#' # data preparation
#' num = 1000
#' dim = 2
#' dim_extra = 2
#'
#' set.seed(123)
#' X <- matrix(rnorm(num * (dim + dim_extra)), num, dim + dim_extra)
#' colnames(X) <-paste0("feature",1:NCOL(X))
#' for (i in 1:dim_extra){
#'   X[, dim + i] <- X[, 1] + 0.5 * rnorm(num)
#' }
#' y <- X[, 1] + 0.3 * X[, 2] + 0.5 * rnorm(num)
#'
#' # fit lasso model
#' require(glmnet)
#' fit1 <- glmnet(X,y)
#'
#' # fit Alternate Lasso model
#' alt1 <- AlternateLasso(X, y, model = fit1, rho = 0.1)
#'
#' print(alt1)
#' g <- plot(alt1)
#' g
#'
#' @export

require(foreach)
AlternateLasso <- function(X, y, model = NULL, rho = 0.1, featurename = NULL, verbose = TRUE){

  stopifnot(
    !missing(X), !missing(y), !is.null(model),
    any(class(model) ==  "glmnet"))

  featurename <- colnames(X)
  if(is.null(featurename)){
    featurename <- paste0("X",1:NCOL(X))
  }

  # original coefficients by lasso
  coeffs.orig <- as.numeric(coef(model, s=rho))[-1]
  # Intercept
  b <- as.numeric(coef(model, s=rho))[1]
  nonzeros <- which(abs(coeffs.orig) > 0)

  # iterative search for alternative features of non-zero features
  p <- getP(X, y, coeffs.orig, b)
  q <- colMeans(X^2)

  if(verbose){
    print('> [feature name, # of alternate feature candidates]')
  }
  alternatives <- foreach(d = nonzeros) %do% {

    coeffs.this <- coeffs.orig
    coeffs.this[d] <- 0
    obj0 <- getObjective(X, y, coeffs.this, b, rho)


    g <- t(X) %*% X[, d] / NROW(X) # getG() in original code
    pp <- p - g * coeffs.orig[d] # remove effect of the target coeffcient.

    # soft-thresholding operator
    rr <- (abs(pp) - rho) / q
    alt.coeffs <- sign(- rr) * ifelse(rr > 0, rr, 0)
    alt.coeffs[nonzeros] <- 0
    alt.coeffs[is.nan(alt.coeffs)] <- 0
    alt.coeffs # da

    if(verbose){
      catf("[ %s ] has [ %d ] alternatives", featurename[d], sum(abs(alt.coeffs)>0))
    }

    alt.this <- foreach(d.alt = which(abs(alt.coeffs) > 0), .combine=rbind) %do% {
      coeffs.this[d.alt] <- alt.coeffs[d.alt]

      obj.tmp <- getObjective(X, y, coeffs.this, b, rho)
      coeffs.this[d.alt] <- 0

      df <-data.frame(
        feature   = featurename[d.alt],
        coef      = alt.coeffs[d.alt] ,
        objective = obj.tmp,
        stringsAsFactors = FALSE)

      return(df)
    }

    list(
      feature = featurename[d],
      coef    = coeffs.orig[d],
      objective    = obj0,
      alternatives = alt.this)
  }

  result <- list(model = model, alternatives = alternatives)
  class(result) <- c(class(result), "AlternateLasso")

  return(result)
}


#' print.AlternateLasso
#'
#' @rdname AlternateLasso
#' @export

print.AlternateLasso <- function(obj){
  stopifnot(any(class(obj) ==  "AlternateLasso"))

  for(i in 1:NROW(obj$alternatives)){
    this <- obj$alternatives[[i]]
    catf("Feature: %s, Coef. = %f", this$feature, this$coef)
    if(is.null(this$alternatives)){
      catf("\t ** No Alternate Features **")
    }

    for(j in 1:NROW(this$alternatives)){
      this.alt <- this$alternatives[j, ]
      catf("\t Alternate Feature: %s, Score = %f, Coef. = %f",
           this.alt$feature, this.alt$objective - this$objective, this$coef)
    }
  }
}

#' convert from Alternate Lasso object to data.frame
#'
#' @param obj    "AlternateLasso" class object.
#' @return data.frame
#' @examples
#' #not run
#' convertDF.AlternateLasso(alt1)

require(foreach)
convertDF.AlternateLasso <- function(obj){
  stopifnot(any(class(obj) ==  "AlternateLasso"))

  df <- foreach::foreach(i=seq_len(NROW(obj$alternatives)), .combine = rbind) %do% {

    LL <- obj$alternatives[[i]]
    foreach::foreach(i = seq_len(NROW(LL$alt)), .combine = rbind) %do% {
      data.frame(feature = LL$feature, alt = LL$alternatives$feature[i],
                 score = LL$objective - LL$alternatives$objective[i])
    }
  }
  sorted.fname <- table(df$feature) %>% data.frame %>% arrange(desc(Freq))
  sorted.df <- foreach(fname = sorted.fname$Var1, .combine=rbind) %do% {
    df.this <- df %>% filter(feature == fname) %>% arrange(score)
  }
  return(sorted.df)
}


#' Plot Bipartite Graph Vertically
#'
#'
#' @param left     [vector] Nodes on the left side
#' @param right    [vector] Nodes on the right side
#'
#' @return \code{\link{igraph}} object
#'
#' @examples
#' from <- c("A", "A", "A", "B", "B", "B", "B", "C")
#' to   <- c(1, 1, 2, 3, 1, 4, 1, 2)
#' plotVerticalBipartiteGraph(left = from, right = to)
#'
#' @import  igraph, RColorBrewer
#' @export

require(igraph)
plotVerticalBipartiteGraph <- function(left, right){
  df <- data.frame(left, right)
  labs.left  <- unique(df[, 1])
  labs.right <- unique(df[, 2])

  g <- graph_from_data_frame(unique(df), directed = FALSE)

  V(g)$x <- c(rep(1, NROW(labs.left)), rep(2, NROW(labs.right)))

  V(g)$y <- c(seq(from=NROW(labs.right), to=1, length.out=NROW(labs.left)),
              NROW(labs.right):1)
  
  n <- NROW(labs.left)
  if(n < 3){ n <- 3 }
  cols <-  c(RColorBrewer::brewer.pal(n, name="Set1"))
  for(i in seq_len(NROW(labs.left))){
    E(g)$color[E(g)[labs.left[i] %--% V(g)]] <- cols[i]
  }

  V(g)$size <- 0
  E(g)$curved <- FALSE

  plot(g)
  invisible(g)
}


#' plot.AlternateLasso
#'
#' @rdname AlternateLasso
#' @import  dplyr
#' @export

plot.AlternateLasso <- function(obj){
  stopifnot(any(class(obj) ==  "AlternateLasso"))
  this <- convertDF.AlternateLasso(obj)
  
  g <- plotVerticalBipartiteGraph(left = this$feature, right = this$alt)
  invisible(g)
}

