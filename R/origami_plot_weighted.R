#' @title Function to generate weighted origami plot
#' @import plotrix
#' @import fmsb
#'
#' @param df dataset processed with data_preparation or in the designated form
#' @param weight weight of each variable, sum up to 1
#' @param pcol color of the line of the original polygon
#' @param pfcol color to fill the area of the original polygon, default is NULL.
#' @param pcol2 color of the line of the weighted polygon, default is rgb(0.6,0.3,0.3,1).
#' @param pfcol2 color to fill the area of the weighted polygon, default is NULL.
#' @param axistype type of axes. 0:no axis label. 1:center axis label only. 2:around-the-chart label only. 3:both center and around-the-chart labels. Default is 0.
#' @param seg number of segments for each axis, default is 4.
#' @param pty point symbol, default is 16. 32 means not printing the points.
#' @param plty line types for plot data, default is 1:6
#' @param plwd line widths for plot data, default is 1
#' @param pdensity filling density of polygons, default is NULL
#' @param pangle angles of lines used as filling polygons, default is 45
#' @param cglty line type for radar grids, default is 1.4
#' @param cglwd line width for radar grids, default is 0.1
#' @param cglcol line color for radar grids, default is #000000
#' @param axislabcol color of axis label and numbers, default is #808080
#' @param title title of the chart, default is blank
#' @param na.itp logical. If true, items with NA values are interpolated from nearest neighbor items and connect them. If false, items with NA are treated as the origin. Default is TRUE.
#' @param centerzero logical. If true, this function draws charts with scaling originated from (0,0). If false, charts originated from (1/segments). Default is TRUE.
#' @param vlabels character vector for the names for variables, default is NULL
#' @param vlcex font size magnification for vlabels, default is 1
#' @param caxislabels center axis labels, default is seq(0,1,by = 0.25)
#' @param calcex font size magnification for caxislabels, default is NULL
#' @param paxislabels around-the-chart labels, default is NULL
#' @param palcex font size magnification for paxislabels, default is NULL
#' @details This function allows the creation of an origami plot with user-specified weights for different
#' outcomes. The weighted origami plot is a refined analytical tool that facilitates the adjustment of individual
#' attribute weights to accurately reflect their significance in determining overall performance. For instance, if
#' certain outcomes hold greater clinical relevance based on a scientific question, the user can assign higher weights
#' to these outcomes relative to others. Note that the weights assigned should sum up to 1.
#' @return NULL
#'
#' @examples
#' data(data)
#' df_list <- data_preparation(data, min_value = 0.15)
#' origami_plot_weighted(df = df_list[[6]], weight = c(0.15,0.25,0.3,0.2,0.1),pcol = rgb(0.2,0.5,0.5,1),
#' pfcol = rgb(0.2,0.5,0.5,0.1),axistype=1)
#'
#' @export

origami_plot_weighted<- function(df, weight, pcol, pfcol=NULL, pcol2 = rgb(0.6,0.3,0.3,1), pfcol2 = NULL, axistype=0, seg=4, pty=16, plty=1:6, plwd=1,
                                 pdensity=NULL, pangle=45, cglty=1.4, cglwd=0.1,
                                 cglcol="#000000", axislabcol="#808080", title="",
                                 na.itp=TRUE, centerzero=FALSE, vlabels=NULL, vlcex=1,
                                 caxislabels=seq(0,1,by = 0.25), calcex=NULL,
                                 paxislabels=NULL, palcex=NULL, ...) {

  if (sum(weight)!=1) { cat("The weight must sum up to 1\n"); return() }
  n_prime <- ncol(df)/2
  aux_array_odd <- as.vector(seq(1,2*n_prime-1,2))
  aux_array_even <- as.vector(seq(2,2*n_prime,2))
  df2_original <- df[3,aux_array_odd]
  #weight_original <- 1/n_prime
  max_weight <- max(weight)
  df2 <- df2_original * (weight / max_weight)

  min_value <- min(df[3,])

  df2_list <- data_preparation(df2, min_value = min_value)
  df2 <- df2_list[[1]]

  df_list <- list(df,df2)

  pcol_list <- list(pcol,pcol2)
  pfcol_list <- list(pfcol,pfcol2)
  num_figure = 2




  n_col = dim(df)[2]
  if (!is.data.frame(df)) { cat("The data must  be given as dataframe.\n"); return() }
  if ((n <- length(df))<3) { cat("The number of variables must be 3 or more.\n"); return() }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type="n", frame.plot=FALSE, axes=FALSE,
       xlab="", ylab="", main=title, asp=1) # define x-y coordinates without any plot
  theta <- seq(90, 450, length=n+1)*pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  points(0,0, pch = 16, col  = rgb(0,0,0,0.2))
  for (ind in 1:n_col){
    if (ind == 1){
      segments(0, 0, 0, 1, lwd = 2, lty = 1, col = rgb(0,0,0,0.2)) # factor 1
    } else{
      if ((ind %% 2) == 0){
        draw.radial.line(0, 1, center=c(0,0), deg = 90+(360/n_col)*(ind-1), lwd = 1, lty = 2, col = rgb(0,0,0,0.4))
      } else{
        draw.radial.line(0, 1, center=c(0,0), deg = 90+(360/n_col)*(ind-1), lwd = 1, lty = 1, col = rgb(0,0,0,0.4))
      }

    }
  }
  for (i in 1:num_figure) {

    df <- df_list[[i]]
    pcol <- pcol_list[[i]]
    pfcol<- pfcol_list[[i]]

    if(i==2) {
      plty = "longdash"
      pfcol <- NULL
      plwd <- 2
    }
    if (centerzero) {
     arrows(0, 0, xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
    } else {
     arrows(xx/(seg+CGap), yy/(seg+CGap), xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
    }
    PAXISLABELS <- df[1,1:n]
    if (!is.null(paxislabels)) PAXISLABELS <- paxislabels
    if (axistype==2|axistype==3|axistype==5) {
      if (is.null(palcex)) text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol) else
        text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol, cex=palcex)
    }
    VLABELS <- colnames(df)
    if (!is.null(vlabels)) VLABELS <- vlabels
    if (is.null(vlcex)) text(xx*1.2, yy*1.2, VLABELS) else
      text(xx*1.2, yy*1.2, VLABELS, cex=vlcex)
    series <- length(df[[1]])
    SX <- series-2
    if (length(pty) < SX) { ptys <- rep(pty, SX) } else { ptys <- pty }
    if (length(pcol) < SX) { pcols <- rep(pcol, SX) } else { pcols <- pcol }
    if (length(plty) < SX) { pltys <- rep(plty, SX) } else { pltys <- plty }
    if (length(plwd) < SX) { plwds <- rep(plwd, SX) } else { plwds <- plwd }
    if (length(pdensity) < SX) { pdensities <- rep(pdensity, SX) } else { pdensities <- pdensity }
    if (length(pangle) < SX) { pangles <- rep(pangle, SX)} else { pangles <- pangle }
    if (length(pfcol) < SX) { pfcols <- rep(pfcol, SX) } else { pfcols <- pfcol }


    for (i in 3:series) {
      xxs <- xx
      yys <- yy
      scale <- CGap/(seg+CGap)+(df[i,]-df[2,])/(df[1,]-df[2,])*seg/(seg+CGap)
      if (sum(!is.na(df[i,]))<3) { cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n",i,df[i,])) # for too many NA's (1.2.2012)
      } else {
        for (j in 1:n) {
          if (is.na(df[i, j])) { # how to treat NA
            if (na.itp) { # treat NA using interpolation
              left <- ifelse(j>1, j-1, n)
              while (is.na(df[i, left])) {
                left <- ifelse(left>1, left-1, n)
              }
              right <- ifelse(j<n, j+1, 1)
              while (is.na(df[i, right])) {
                right <- ifelse(right<n, right+1, 1)
              }
              xxleft <- xx[left]*CGap/(seg+CGap)+xx[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
              yyleft <- yy[left]*CGap/(seg+CGap)+yy[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
              xxright <- xx[right]*CGap/(seg+CGap)+xx[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
              yyright <- yy[right]*CGap/(seg+CGap)+yy[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
              if (xxleft > xxright) {
                xxtmp <- xxleft; yytmp <- yyleft;
                xxleft <- xxright; yyleft <- yyright;
                xxright <- xxtmp; yyright <- yytmp;
              }
              xxs[j] <- xx[j]*(yyleft*xxright-yyright*xxleft)/(yy[j]*(xxright-xxleft)-xx[j]*(yyright-yyleft))
              yys[j] <- (yy[j]/xx[j])*xxs[j]
            } else { # treat NA as zero (origin)
              xxs[j] <- 0
              yys[j] <- 0
            }
          }
          else {
            xxs[j] <- xx[j]*CGap/(seg+CGap)+xx[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
            yys[j] <- yy[j]*CGap/(seg+CGap)+yy[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
          }
        }
        if (is.null(pdensities)) {
          polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2], col=pfcols[i-2])
        } else {
          polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2],
                  density=pdensities[i-2], angle=pangles[i-2], col=pfcols[i-2])
        }
        points(xx[aux_array_odd]*scale[aux_array_odd], yy[aux_array_odd]*scale[aux_array_odd], pch=ptys[i-2], col=pcols[i-2])
        points(xx[aux_array_even]*scale[aux_array_even], yy[aux_array_even]*scale[aux_array_even], pch=ptys[i-2], col=rgb(0,0,0,0.2))
        # points(xx*scale, yy*scale, pch=ptys[i-2], col=rgb(0,0,0,0.2))
      }
    }

    for (i in 1:seg) { # complementary guide lines, dotted navy line by default
      polygon(xx*(i+CGap)/(seg+CGap), yy*(i+CGap)/(seg+CGap), lty=cglty, lwd=cglwd, border=cglcol)
      if (axistype==1|axistype==3) CAXISLABELS <- paste(i/seg*100,"(%)")
      if (axistype==4|axistype==5) CAXISLABELS <- sprintf("%3.2f",i/seg)
      if (!is.null(caxislabels)&(i<length(caxislabels))) CAXISLABELS <- caxislabels[i+1]
      if (axistype==1|axistype==3|axistype==4|axistype==5) {
        if (is.null(calcex)) text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol) else
          text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol, cex=calcex)
      }
    }

  }
}
