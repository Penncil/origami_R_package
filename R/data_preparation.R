#' @title Function to prepare the data into designated format
#' @param df dataset with each column representing a variable name paired with its value and each row representing a graph
#' @param min_value auxiliary point in the graph, default is min(df)/2
#' @details This function takes a data frame as input and output a list of formatted data frames.
#' It introduces an auxiliary point for each variable, positioned equidistantly from the central point along auxiliary axes.
#' Users can customize the distance from the point to the center.
#' Without user customization, the distance defaults to half of the smallest value within the dataset.
#' @return df_list
#'
#' @examples
#' data(data)
#' data_preparation(data,min_value=0.15)
#'
#' @export

data_preparation <- function(df, min_value = NULL, ...){
  df_names <- names(df)
  n_cols <- ncol(df)
  n_rows <- nrow(df)
  if(is.null(min_value)){
    min_value <- as.numeric(min(df)/2)
  }
  df_list <- list()
  for(i in 1:n_rows){
    df_sub <- df[i,]
    df_sub <- data.frame(mapply(cbind, df_sub, "aux"=min_value, SIMPLIFY=F))
    aux_array_even <- seq(2,2*n_cols,2)
    aux_array_odd <- seq(1,2*n_cols-1,2)
    colnames(df_sub)[aux_array_even]=""
    colnames(df_sub)[aux_array_odd]=df_names
    df_sub <- as.data.frame(rbind(1,0,df_sub))
    df_list[[i]] <- df_sub
  }
  return(df_list)
}
