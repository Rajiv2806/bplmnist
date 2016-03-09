#' displayDigit
#' Author: Hrvoje Stojic
#' Prints a 16x16 MNIST digit using pixel values
#'
#' @param pixels 
#' @param label 
#' @param predictedLabel 
#' @param saveImage 
#' @param newDevice 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
displayDigit <- function(pixels, 
                         label, 
                         predictedLabel = NULL, 
                         saveImage = FALSE,
                         newDevice = TRUE) {

    # converting pixel row into a matrix required by image
    img = matrix(pixels, 16, 16, byrow = TRUE)

    # image is upside down, need to rotate it for 180 degrees
    rotate <- function(x) t(apply(x, 2, rev))
    img <- rotate(img)

    # do we have predictions as well?
    if (is.null(predictedLabel)) predictedLabel <- "-"

    # small plot function
    plotDigit <- function() {
        par(mar = c(0, 0, 2, 0))
        image(img, axes = FALSE, col = grey(seq(0, 1, length = 256)))
        title(main = paste("Label: ", label, "|  Prediction: ",predictedLabel))
    }

    # saving image if instructed
    if (saveImage) {
        png("digit.png")
        plotDigit()
        dev.off()
    }

    # plotting
    if (newDevice ) dev.new(width=4, height=4.2)
    plotDigit()
}
