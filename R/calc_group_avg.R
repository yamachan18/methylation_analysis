# feed in a dataframe of beta values (betadf) and a list of groups with sample IDs as the values.
# colnames in betadf are what is used in the groupings list
# groupings don't have to be mutually exclusive, though they will be if built from a named vector.

calc_group_avg <- function(betadf, l_groupings){
    
    l_groupavg <- list()
    for (i in 1:length(l_groupings)){
      l_groupavg[[i]] <- rowMeans(betadf[, which( colnames(betadf) %in% unlist(l_groupings[[i]]))], na.rm=TRUE)
    }
    names(l_groupavg) <- names(l_groupings)
    df_groupavg <- do.call(cbind, l_groupavg)
        
  return(df_groupavg)
  
}