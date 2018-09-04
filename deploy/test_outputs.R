library(ggplot2)
library(raster)

areaSize <- "MEDIUM"
outputDir <- file.path("~/GitHub/LandWeb/outputs", paste0(areaSize, "_All"))
vtmRasters <- list.files(outputDir, "^vegTypeMap.*[.]tif$", full.names = TRUE) %>%
  grep("LFLT", ., value = TRUE, invert = TRUE)

## TODO: plot biomass not leading species; burn events confounded with changes in dominant species

## TODO: also plot annual area burned

rm(props)
for (i in 1:length(vtmRasters)) {
  vals <- raster(vtmRasters[i])[]

  if (exists("props"))
    props <- rbind(props, table(vals))
  else
    props <- table(vals)
}

colnames(props) <- c("none", "Abie_sp", "Pice_gla", "Pice_mar", "Pinu_sp", "Popu_tre")
rownames(props) <- NULL
props.df <- as.data.frame(props)
props.df$time <- 1:NROW(props)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

p0 <- ggplot(props.df, aes(x = time, y = none)) +
  geom_line() +
  geom_vline(xintercept = seq(9, 299, 10), color = "red")

p1 <- ggplot(props.df, aes(x = time, y = Abie_sp)) +
  geom_line() +
  geom_vline(xintercept = seq(9, 299, 10), color = "red")

p2 <- ggplot(props.df, aes(x = time, y = Pice_gla)) +
  geom_line() +
  geom_vline(xintercept = seq(9, 299, 10), color = "red")

p3 <- ggplot(props.df, aes(x = time, y = Pice_mar)) +
  geom_line() +
  geom_vline(xintercept = seq(9, 299, 10), color = "red")

p4 <- ggplot(props.df, aes(x = time, y = Pinu_sp)) +
  geom_line() +
  geom_vline(xintercept = seq(9, 299, 10), color = "red")

p5 <- ggplot(props.df, aes(x = time, y = Popu_tre)) +
  geom_line() +
  geom_vline(xintercept = seq(9, 299, 10), color = "red")

multiplot(p0, p1, p2, p3, p4, p5, cols = 3)
