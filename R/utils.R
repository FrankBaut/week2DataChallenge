#'  Calculate Cohen's d (effect size)
#'
#'  Helper method to calculate effect size using Cohen's d formula.
#'
#' @param lowestMean Mean in lowest group
#' @param LowestSD SD in lowest group
#' @param highestMean Mean in highest group
#' @param highestSD SD in highest group
#' @return Effect size estimate
#' @export
effectSize <- function(lowestMean, lowestSD, highestMean, highestSD){
  sd <- sqrt((lowestSD*lowestSD + highestSD*highestSD)/2)
  return ((abs(highestMean-lowestMean))/sd)
}

#'  Format mean and SD
#'
#'  Helper method to show formatted mean and SD to specified number of decimal places for use in table cell.
#'
#' @param col A numeric vector
#' @param numDigits Number of decimal places for output.
#' @return Cell for HTML summary table giving mean and SD of vector to a specified number of decimal places.
#' @export
summary4tableHTML_simple <- function(col, numDigits){
  mean <- format(round(mean(col, na.rm=TRUE), numDigits), nsmall = numDigits)
  std <- format(round(sd(col, na.rm=TRUE), numDigits), nsmall = numDigits)
  cell <- paste('<td align="center">', mean, ' &plusmn ', std, '</td>', sep="")
  return (cell)
}

#'  Format p-value
#'
#'  Helper method to show formatted p-value in HTML table
#'
#' @param pVal p-value
#' @param cut Smallest p-value to be shown
#' @return Cell for HTML summary table with formatted p-values.
#' @export
pValueHTML_cell <- function(pVal, cut){
  pStr <- ""
  if(pVal<cut){
    valStr <- formatC(cut, digits=0, format="e")
    ele <- strsplit(valStr, "e")[[1]]
    pStr <- paste('<1x10<sup>', ele[2], '</sup>', sep="")
  }
  else if(pVal<.001){
    valStr <- formatC(pVal, digits=0, format="e")
    ele <- strsplit(valStr, "e")[[1]]
    pStr <- paste(ele[1], 'x10<sup>', ele[2], '</sup>', sep="")
  }
  else {
    pStr <- format(round(pVal, 3), nsmall = 3, nbig=0)
    pStr <- sub('^(-)?0[.]', '\\1.', pStr)
  }
  cell <- paste('<td align="center">', pStr, '</td>', sep="")
  return (cell)
}



#' Plot variables within quintiles of another variable
#'
#' @param dataInput Dataset to use
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @param writePDF Logical parameter. If TRUE, writes pdf file with output.
#' @export
plotVarAndQuintile <- function(dataInput, exposure, outcome, writePDF = FALSE){
  #cat('\n\n\n =========== \n', outcome, ' AND ', exposure, '(confounders = ', confounders, ')\n =========== \n')
  expVar <- dataInput[[exposure]]
  outVar <- dataInput[[outcome]]
  filename <- paste(outcome, '-', exposure, '.pdf', sep="")
  expVarQuintile <- as.factor(cut(expVar,
                                  quantile(expVar, c(0,0.2,0.4,0.6,0.8,1.0), na.rm=TRUE),
                                  include.lowest=TRUE,
                                  labels=FALSE)
  )
  if (writePDF) {
    pdf(filename, height=8, width=16)
  }
  par(mfrow=c(1,2))
  plot(outVar ~ expVar,
       main=exposure, xlab=exposure, ylab=outcome, xlim=c(0,quantile(expVar, c(0.99), na.rm=TRUE)))

  boxplot(outVar ~ expVarQuintile,# data=tmp,
          main=paste(exposure,'-quintile',sep=""), xlab=exposure, ylab=outcome)
  if (writePDF) {
    dev.off()
    cat('Plots written to: ', filename)
  }
}

#' Plot behaviour profiles over an average day
#'
#' @param data Dataset to use
#' @param exposurePrefix Name of behaviour variable to plot
#' @param exposureSuffix Suffix of behaviour variable columns in dataset
#' @param yAxisLabel Label for y-axis
#' @param outPng Optional filename to save plot
#' @return Plot of behaviour variable over hours of day
#' @export
plotAverageDay <- function(data, exposurePrefix, exposureSuffix, yAxisLabel = exposurePrefix, outPng = NULL){

  hrPACols <- c()
  mean_PACols <- c()
  se_PACols <- c()
  low_PACols <- c()
  high_PACols <- c()
  hrs <- c()
  for (hr in 0:23){
    hrs <- c(hrs, as.numeric(hr))
    hrPACols <- c(hrPACols , paste(exposurePrefix, hr, exposureSuffix, sep = ""))
    mean_PACols <- c(mean_PACols, as.numeric(mean(data[,hrPACols[hr+1]], na.rm = TRUE)))
    se_PACols <- c(se_PACols, as.numeric(sqrt(var(data[, hrPACols[hr+1]], na.rm = TRUE))/sqrt(nrow(data))))
    low_PACols <- c(low_PACols, mean_PACols[hr+1] - 1.96*se_PACols[hr+1])
    high_PACols <- c(high_PACols, mean_PACols[hr+1] + 1.96*se_PACols[hr+1])
  }
  plot <- ggplot2::ggplot(data = data.frame(cbind(hrs, mean_PACols, low_PACols, high_PACols)), aes(x = hrs, y = mean_PACols))+
    geom_ribbon(aes(x = hrs, ymin = low_PACols,
                    ymax = high_PACols), colour = "grey")+
    geom_line()+
    labs(title = "Time-of-day behaviour profile",
         y = yAxisLabel,
         x = "Hour of Day")

  if (!(is.null(outPng))){
    ggplot2::ggsave(outPng, plot = plot, device = png())
  }

  return(plot)

}
