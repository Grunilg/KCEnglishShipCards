library("magick")
library("cowplot")
library("ggplot2")
library("dplyr")
library("grid")

theme_set(theme_cowplot())

shipinfo <- read.csv("shipinfotemp.csv", stringsAsFactors = FALSE)
allFiles <- list.files(path = "ShipsDamaged/", pattern = ".png")
shipinfo <- data.frame(shipinfo[,1:3], stringsAsFactors = FALSE)

for (i in 1:length(allFiles)) {
  CPath <- paste0("ShipsDamaged/",allFiles[i])
  tmp <- image_read(CPath)
  tmpbg <- image_read("CropStapel.png")
  tmpmosaic <- c(tmpbg,tmp)
  tmp <- image_mosaic(tmpmosaic)
  tmp <- image_crop(tmp,"700x600")
  image_write(tmp, path = paste0("CropTestDamaged/",allFiles[i]))
  print(i)
}

for (i in 1:length(allFiles)) {
  CPath <- paste0("CropTestDamaged/",allFiles[i])
  print(ggdraw() +
          draw_image(CPath, hjust = 0, vjust = 0, halign = 0, valign = 0))
  croploc <- grid.locator()
  xcoord <- as.numeric(croploc[1])
  ycoord <- as.numeric(croploc[2])
  shipinfo[i,2] <- xcoord
  shipinfo[i,3] <- ycoord
}

write.csv(shipinfo,file = "facecoordinatesDamaged.csv",quote=FALSE)