#' plot.AlternateLasso
#'
#' @rdname AlternateLasso
#'
#' @param ... adjuster options path to \code{networkD3::sankeyNetwork}.
#'
#' @importFrom dplyr mutate
#' @export

plot.AlternateLasso <- function(obj, ...){
  stopifnot(any(class(obj) ==  "AlternateLasso"))

  gdf <- convert.graphDataFrame(obj)

  links <- gdf$links %>%
    mutate(log.rev.abs.score = log10(1 + 1/abs(score)))

  networkD3::sankeyNetwork(Links = links, Nodes = gdf$nodes,
                           Source = "from", Target = "to",
                           Value = "log.rev.abs.score",
                           NodeID = "name",
                           LinkGroup = "feature",
                           ...)
  # %>%
  #   print()
  #
  # invisible(list(nodes = gdf$nodes, links = links))
}

#' convert from Alternate Lasso object to data.frame
#'

convert.graphDataFrame <- function(obj){
  stopifnot(any(class(obj) ==  "AlternateLasso"))

  df <- foreach::foreach(i=seq_len(NROW(obj$alternatives)), .combine = rbind) %do% {

    LL <- obj$alternatives[[i]]
    foreach::foreach(i = seq_len(NROW(LL$alt)), .combine = rbind) %do% {
      data.frame(feature = LL$feature,
                 alt     = LL$alternatives$feature[i],
                 coef    = LL$alternatives$coef[i],
                 score   = LL$alternatives$objective[i] - obj$original$objective,
                 stringsAsFactors = FALSE)
    }
  }

  node.from <- data.frame(name = unique(df$feature), group = "origin", stringsAsFactors = FALSE)
  node.to <- data.frame(name = unique(df$alt), group = "alternative", stringsAsFactors = FALSE)

  nodes <- rbind(node.from, node.to) %>%
    dplyr::mutate(id = 1:n() - 1)
  links <- df %>%
    dplyr::left_join(nodes %>%
                       dplyr::rename(feature = name), by = "feature") %>%
    dplyr::rename(from = id) %>%
    dplyr::left_join(nodes %>%
                       dplyr::rename(alt = name), by = "alt") %>%
    dplyr::rename(to = id) %>%
    dplyr::select(from,to,feature,alt,coef,score)

  return(list(nodes=nodes, links=links))

}

