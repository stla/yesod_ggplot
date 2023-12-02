library(jsonlite)
library(ggplot2)
library(base64enc)

# extract data from the JSON file
jsonData <- fromJSON(jsonFile)
x <- jsonData[["_x"]]
y <- jsonData[["_y"]]
w <- jsonData[["_width"]]
h <- jsonData[["_height"]]

# if `y` is not numeric, we throw an error
if(!is.numeric(y)) {
  stop("The `y` column is not numeric.")
}

# function to convert x to numeric if possible
maybeNumeric <- function(x) {
  xx <- as.numeric(x)
  if(anyNA(xx)) x else xx
}

# data
dat <- data.frame(x = maybeNumeric(x), y = y)

# plot
gg <- ggplot(dat, aes(x = x, y = y)) + 
  geom_point(size = 0.3) + 
  theme(
    axis.title = element_text(size = 5),
    axis.text  = element_text(size = 3),
    axis.ticks = element_line(linewidth = 0.3)
  )

# save plot as PNG
png <- tempfile(fileext = ".png")
ggsave(png, gg, width = w, height = h, units = "px", dpi = "print")

# convert the PNG file to a base64 string
base64 <- dataURI(file = png, mime = "image/png")

# print the base64 string
cat(base64)
