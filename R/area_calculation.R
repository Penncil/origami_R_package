#' @title Function to calculate area of the generated polygon
#' @param df dataset processed with data_preparation or in the designated form
#' @details This function serves as a supplementary tool to compute the area of a generated origami
#' plot when the maximal area achievable within the defined parameters (when all the variables attain 1) is set to 1.
#' The resulting calculated area offers an interpretation of the proportion between the actual origami plot and the
#' maximum achievable area. An example of calculated area is shown in Figure 1.
#'
#' @return area
#'
#' @examples
#' data(data)
#' df_list <- data_preparation(data, min_value = 0.15)
#' area_calculation(df_list[[1]])
#'
#' @export

area_calculation <- function(df){
  n <- ncol(df)/2
  aux_array_odd <- seq(1,2*n-1,2)
  area <- sum(df[3,aux_array_odd])/n
  return(area)
}
