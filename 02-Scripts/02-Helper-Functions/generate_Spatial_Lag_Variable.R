






generate_Spatial_Lag <- function(this.df, 
                                 these.neighbors, 
                                 this.neighbor.matrix, 
                                 this.subtype, 
                                 this.variable, 
                                 new.var.suffix = "_spatial_lag1"){
  require(tidyr)
  require(dplyr)
  require(spdep)
  
  
  this.data.frame <- this.df %>%
    filter(location%in%these.neighbors)%>%
    filter(subtype==this.subtype) %>%
    select(all_of(c("season", "location", this.variable))) %>%
    pivot_wider(names_from = season, values_from = all_of(this.variable))
  
  this.data.frame <- this.data.frame[match(this.data.frame$location, these.neighbors),]
  
  
  this.data.matrix <- data.matrix(this.data.frame[,2:11])
  
  
  
  # spatial.lag1a <- solve(diag(apply(this.neighbor.matrix,1,sum)))%*%this.neighbor.matrix%*%this.data.matrix
  # spatial.lag1b <- solve(diag(apply(this.neighbor.matrix,
  #                                  1,
  #                                  function(this.row){
  #                                    ifelse(sum(this.row)==0,
  #                                           0.1,
  #                                           sum(this.row))
  #                                    })))%*%this.neighbor.matrix%*%this.data.matrix
  
  spatial.lag1 <- matrix(data=NA, nrow=nrow(this.data.matrix), ncol=ncol(this.data.matrix))
  
  
  notmissing.flag <- !is.na(this.data.matrix)
  these.diagonals <- this.neighbor.matrix%*%notmissing.flag
  these.diagonals[which(these.diagonals==0)] <- 0.1
  
  
  for(ii in 1:ncol(this.data.matrix)){
    
    spatial.lag1[,ii] <- solve(diag(these.diagonals[,ii]))%*%this.neighbor.matrix%*%ifelse(is.na(this.data.matrix[,ii]),0,this.data.matrix[,ii]+1)
    
  }
  
  spatial.lag1[which(spatial.lag1==0)] <- NA
  spatial.lag1 <- spatial.lag1-1
  
  colnames(spatial.lag1) <- colnames(this.data.matrix)
  
  
  # > identical(spatial.lag1a, spatial.lag1b)
  # [1] TRUE
  # > identical(spatial.lag1a, spatial.lag1)
  # [1] TRUE
  
  
  
  spatial.lag1 <- spatial.lag1 %>% 
    as.data.frame()
  
  spatial.lag1$location <- this.data.frame$location
  
  
  spatial.lag1 <- spatial.lag1 %>%
    pivot_longer(!matches("^location"), names_to = "season", values_to = paste0(this.variable, new.var.suffix)) %>%
    mutate(subtype = this.subtype) %>%
    select(subtype, season, location, everything())
  
  
  return(spatial.lag1)
  
}




