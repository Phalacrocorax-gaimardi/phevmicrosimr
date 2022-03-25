

#' createArtificialSociety
#' 
#' Creates a homophilous social influence network
#'
#' @param society tibble containing society
#' @param homophily tibble with homophily parameters for society
#' @param nu gower distance exponent
#'
#' @return a tidygraph object
#' @export
#' @importFrom magrittr %>%
#' @examples
createArtificalSociety <- function(society=society,homophily=homophily,nu=4.5){
  #create a random homophilous social network
  #social distance measure=gower distance
  #nu gives the social distance decay exponentlarger mu higher assortativity
  #agents with degree zero remain degree zero but there may be additional nodes with degree zero
  society_factor <- unclass(society %>% dplyr::mutate_if(is.character,as.factor)) %>% as.data.frame()
  society_factor1 <- dplyr::filter(society_factor,degree != 0)
  society1 <- dplyr::filter(society, degree != 0)
  
  N_society1 <- dim(society1)[1]
  
  
  zeronodes <- dplyr::filter(society,degree==0)$ID #nodes with no influencers
  
  dist_mat <- cluster::daisy(society_factor1[,2:12], metric ="gower", weights=homophily$weights) %>% as.matrix()
  prob_mat <- (1-dist_mat)^nu
  prob_mat1 <- 1.1*prob_mat %*% diag(society_factor1$degree/apply(prob_mat,2,sum)) #adjust this parameter
  
  nodes <- tidyr::tibble(ID=society1$ID)
  edges <- tidyr::expand_grid(from=1:N_society1,to=1:N_society1)
  edges <- dplyr::filter(edges, from < to) #avoid loops
  #edges <- filter(edges, !(from %in% zeronodes)) #no edges from zeronodes
  edges <- edges %>% dplyr::rowwise() %>% dplyr::mutate(p=prob_mat1[from,to])
  edges <- edges %>% dplyr::rowwise() %>% dplyr::mutate(keep=ifelse(stats::runif(1)< p,T,F))
  edges <- edges %>% dplyr::filter(keep)
  
  edges <- edges[,1:2]
  edges$from <- nodes$ID[edges$from] #relable to orginal ids
  edges$to <- nodes$ID[edges$to]
  #restore zero nodes
  nodes <- dplyr::bind_rows(nodes,tidyr::tibble(ID=zeronodes)) %>% dplyr::arrange(ID)
  #restore
  g <- tidygraph::tbl_graph(nodes=nodes,edges=edges,directed=F) %>% dplyr::inner_join(society,by="ID")
  return(g)
  
}


