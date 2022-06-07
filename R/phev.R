#' uf
#' 
#' utility factor from phev all electric range
#' https://circabc.europa.eu/sd/a/92324676-bd8c-4075-8301-6caf12283beb/Technical%20Report_UF_final.pdf
#' 
#' 
#' @param AER all electric range (e.g. WLTP)
#'
#' @return utilisation factor 0-1
#' @export
#'
#' @examples
uf <- function(AER){
  
  c <- c(26.25,-38.94,-631.05,5964.83,-25094.6,60380.21,-87517.16, 75513.77,-35748.77,7154.94)
  a <- sapply(1:10, function(i) (AER/800)^i)
  1-exp(-sum(a*c))
  
}


#' e_ice
#' 
#' emissions from a charged depleted phev based in European utility factor
#'
#' @param wltp WLTP quoted emissions for phev
#' @param AER all electric range 
#'
#' @return gCO2/km
#' @export
#'
#' @examples
e_ice <- function(wltp,AER){
   #
   wltp/(1-uf(AER))
  
}


#' e_phev
#' 
#' real world phev emissions model based on European utility factor daily charging fraction xi
#'
#' @param wltp WLTP quoted emissions for phev
#' @param AER all electric range 
#' @param mileage annual km
#' @param xi charging behaviour 0.25,0.5,0.75,1
#'
#' @return gCO2/km
#' @export
#'
#' @examples
e_phev <- function(wltp,AER, mileage, xi=1){
  #
  wltp*(1-uf_phev_interp(mileage,xi,AER))/(1-uf(AER))
  
}


#' uf_phev
#'
#' @param mileage1 mileage category km
#' @param xi1 charging behaviour parameter
#' @param AER PHEV range
#'
#' @return utility factor in 0,1
#' @export
#'
#' @examples
uf_phev <- function(mileage1,xi1,AER){
  
  c <- uf_model_wide %>% dplyr::filter(xi==xi1,mileage==mileage1)
  c <- c[,3:13] %>% as.numeric()
  uf <- sapply(1:11, function(i) c[i]*(AER/50)^(i-1) ) %>% sum()
  uf <- 1- exp(uf)
  return(uf)
  
}


#' uf_phev_interp
#' 
#' fast, interpolated phev utilisation factor
#'
#' @param mileage1 kms, any mileage <=60000
#' @param xi1 charging behaviour allowed values 0.5,1,1.5,2
#' @param AER electric range km
#'
#' @return real
#' @export
#'
#' @examples
uf_phev_interp <- function(mileage1,xi1,AER){
  
  x <- c(0,mileage_vals)
  y <- 1:200 #AERs
  uf <- dplyr::case_when(xi1==0.5~pracma::interp2(y  = y, x  = x, Z = uf_interp_data_xi_0.5,xp = mileage1,yp=AER),
                   xi1==1.0~pracma::interp2(y  = y, x  = x, Z = uf_interp_data_xi_1.0,xp = mileage1,yp=AER),
                   xi1==1.5~pracma::interp2(y  = y, x  = x, Z = uf_interp_data_xi_1.5,xp = mileage1,yp=AER),
                   xi1==2.0~pracma::interp2(y  = y, x  = x, Z = uf_interp_data_xi_2.0,xp = mileage1,yp=AER)
                   )
  return(uf)
  
}

#cppFunction('double xiC(NumericVector c, double AER) {
#  int n = c.size();
#  double total = 0;
#  for(int i = 0; i < n; ++i) {
#    total += c[i]*pow(AER/50,i);
#  }
#  return 1-exp(total);
#}')

#uf_phev1 <- function(mileage1,xi1,AER){
#  
#  c <- uf_model_wide %>% dplyr::filter(xi==xi1,mileage==mileage1)
#  c <- c[,3:13] %>% as.numeric()
#  uf <- xiC(c,AER)
#  return(uf)
#  }




