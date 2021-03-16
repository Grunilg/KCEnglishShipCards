library("magick")
library("dplyr")
library("stringr")

#import all templates
##### 

#backgrounds
rarityc1 <- image_read("Rarity/c1.png")
assign("rarityc1",rarityc1)
rarityc2 <- image_read("Rarity/c2.png")
assign("rarityc2",rarityc2)
rarityc3 <- image_read("Rarity/c3.png")
assign("rarityc3",rarityc3)
rarityr1 <- image_read("Rarity/r1.png")
assign("rarityr1",rarityr1)
rarityr2 <- image_read("Rarity/r2.png")
assign("rarityr2",rarityr2)
raritysr1 <- image_read("Rarity/sr1.png")
assign("raritysr1",raritysr1)
raritysr2 <- image_read("Rarity/sr2.png")
assign("raritysr2",raritysr2)
raritysr3 <- image_read("Rarity/sr3.png")
assign("raritysr3",raritysr3)

#namebgs
NameSC1 <- image_read("Name overlay/NameSC1.png")
assign("NameSC1",NameSC1)
NameSC2 <- image_read("Name overlay/NameSC2.png")
assign("NameSC2",NameSC2)
NameSC3 <- image_read("Name overlay/NameSC3.png")
assign("NameSC3",NameSC3)
NameSC4 <- image_read("Name overlay/NameSC4.png")
assign("NameSC4",NameSC4)
NameSC5 <- image_read("Name overlay/NameSC5.png")
assign("NameSC5",NameSC5)

NameSC1L <- image_read("Name overlay/NameSC1L.png")
assign("NameSC1L",NameSC1L)
NameSC2L <- image_read("Name overlay/NameSC2L.png")
assign("NameSC2L",NameSC2L)
NameSC3L <- image_read("Name overlay/NameSC3L.png")
assign("NameSC3L",NameSC3L)
NameSC4L <- image_read("Name overlay/NameSC4L.png")
assign("NameSC4L",NameSC4L)
NameSC5L <- image_read("Name overlay/NameSC5L.png")
assign("NameSC5L",NameSC5L)

#top left ship type
stypeAO <- image_read("Ship Types/AO.png")
assign("AO",stypeAO)
stypeAR <- image_read("Ship Types/AR.png")
assign("AR",stypeAR)
stypeAS <- image_read("Ship Types/AS.png")
assign("AS",stypeAS)
stypeAV <- image_read("Ship Types/AV.png")
assign("AV",stypeAV)
stypeBB <- image_read("Ship Types/BB.png")
assign("BB",stypeBB)
stypeBBV <- image_read("Ship Types/BBV.png")
assign("BBV",stypeBBV)
stypeCA <- image_read("Ship Types/CA.png")
assign("CA",stypeCA)
stypeCAV <- image_read("Ship Types/CAV.png")
assign("CAV",stypeCAV)
stypeCL <- image_read("Ship Types/CL.png")
assign("CL",stypeCL)
stypeCLT <- image_read("Ship Types/CLT.png")
assign("CLT",stypeCLT)
stypeCT <- image_read("Ship Types/CT.png")
assign("CT",stypeCT)
stypeCV <- image_read("Ship Types/CV.png")
assign("CV",stypeCV)
stypeCVB <- image_read("Ship Types/CVB.png")
assign("CVB",stypeCVB)
stypeCVL <- image_read("Ship Types/CVL.png")
assign("CVL",stypeCVL)
stypeDD <- image_read("Ship Types/DD.png")
assign("DD",stypeDD)
stypeDE <- image_read("Ship Types/DE.png")
assign("DE",stypeDE)
stypeFBB <- image_read("Ship Types/FBB.png")
assign("FBB",stypeFBB)
stypeLHA <- image_read("Ship Types/LHA.png")
assign("LHA",stypeLHA)
stypeSS <- image_read("Ship Types/SS.png")
assign("SS",stypeSS)
stypeSSV <- image_read("Ship Types/SSV.png")
assign("SSV",stypeSSV)

frameB <- image_read("Frames/Bottom.png")
frameUL <- image_read("Frames/Left Upper.png")

kaiOverlay <- image_read("Kai Overlay/Kai Overlay.png")

nameOverlay <- image_read("Name overlay/Name.png")

sakura <- image_read("overlay/Sakura.png")
stars <- image_read("overlay/Stars.png")

vStripes <- image_read("V Stripes/VStr.png")

#top left empblem
emblem1 <- image_read("Ship Emblem/Emblem1.png")
assign("Emblem1",emblem1)
emblem2 <- image_read("Ship Emblem/Emblem2.png")
assign("Emblem2",emblem2)
emblem3 <- image_read("Ship Emblem/Emblem3.png")
assign("Emblem3",emblem3)
emblem4 <- image_read("Ship Emblem/Emblem4.png")
assign("Emblem4",emblem4)

#create images
#####

allFiles <- list.files(path = "Ships/", pattern = ".png")
write.csv(allFiles,"missingno.csv")

####crop the images

shipinfo <- read.csv("shipinfotemp.csv", stringsAsFactors = FALSE)

for (i in 579:length(allFiles)) {
  CPath <- paste0("Ships/",allFiles[i])
  
  coorx <- as.numeric(shipinfo[i,9]) + 400 - 218
  coory <- as.numeric(shipinfo[i,10]) + 400 - 200
  
  tmp <- image_read(CPath)
  tmpinfo <- image_info(tmp)
  
  tmp <- image_border(image_background(tmp, "transparent"), "transparent", "400x400")
  
  tmp <- image_crop(tmp, paste0("436x600+",coorx,"+",coory))
  
  tmp <- image_scale(tmp, "327x450")
  #tmp <- image_annotate(tmp,".",location = paste0("+",coorx,"+",coory),size=40,color = "#FF0000")
  image_write(tmp, path = paste0("CropsShips/",allFiles[i]), format = "png")
  print(i)
}

newnames <- list.files(path = "original cards/", pattern = ".png")

for (i in 1:length(shipinfo[,1])) {
  
  #prepare elements
  
  tmpname <- shipinfo[i,2]
  tmprarity <- get(paste0("rarity",shipinfo[i,3]))
  tmpshiptype <- shipinfo[i,5]
  tmpsshortshiptype <- get(paste0(shipinfo[i,6]))
  tmpshipgirl <- image_read(paste0("CropsShips/",allFiles[i]))
  tmpemblem <- get(paste0(shipinfo[i,7]))
  
  #determine name sizeclass and prepare overlay
  
      #determine line breaks
  
  longname <- FALSE
  if(shipinfo[i,11]!=""){
    longname <- TRUE
  }
  
  tmpsizeclass <- shipinfo[i,13]
  
  if(longname==FALSE){
    tmpnameoverlay <- get(paste0("NameSC",tmpsizeclass))
  }else{
    tmpnameoverlay <- get(paste0("NameSC",tmpsizeclass,"L"))
    tmpsizeclass <- tmpsizeclass+10
  }
  
  #determine remodel status and font colours
  
  tmpkai <- NULL
  tmpoverlay <- NULL
  kaicolour <- "#FFFFFF"
  formcolour <- "#FFFFFF"
  formsize <- 20
  
  if(shipinfo[i,4]=="Kai" || shipinfo[i,4]=="Kou Kai"){
    tmpkai <- kaiOverlay
    kaicolour <- "#FBE9AA"
    formcolour <- "#FBE9AA"
  }else if(shipinfo[i,4]=="exp"){
    tmpkai <- kaiOverlay
    kaicolour <- "#FBE9AA"
    formcolour <- "transparent"
    formsize <- 0
  }else if(shipinfo[i,4]!=""){
    tmpkai <- kaiOverlay
    kaicolour <- "#FBDCA0"
    formcolour <- "#FBDCA0"
    if(str_length(shipinfo[i,4])==7){
      formsize <- 18
    }
    if(str_length(shipinfo[i,4])==8){
      formsize <- 16
    }
    if(str_length(shipinfo[i,4])==9){
      formsize <- 14
    }
    if(str_length(shipinfo[i,4])>9){
      formsize <- 12
    }
  }
  if(shipinfo[i,8]=="Stars"){
    tmpoverlay <- stars
  }
  if(shipinfo[i,8]=="Sakura"){
    tmpoverlay <- sakura
  }
  
  #create BG
  
  tmpbgc <- c(tmprarity,vStripes,frameUL)
  tmpbg <- image_mosaic(tmpbgc)
  tmpbg <- image_annotate(tmpbg,tmpshiptype,location = "+315+20",degrees=90,size = 36,color="#FFFFFF", font = "Georgia", weight=700)
  
  #add shipgirl, medal and kai status
  
  tmpbgc <- c(tmpbg,tmpshipgirl,tmpkai,tmpemblem,tmpsshortshiptype)
  
  tmpbg <- image_mosaic(tmpbgc)
  
  #add sakura, namecard
  
  tmpbgc <- c(tmpbg,tmpoverlay,tmpnameoverlay)
  tmpbg <- image_mosaic(tmpbgc)
  
  #add name
  
  if(tmpsizeclass==1){
    tmpbg <- image_annotate(tmpbg,tmpname,location = "-104+160",degrees=0,size = 35,color = kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==2){
    tmpbg <- image_annotate(tmpbg,tmpname,location = "-78+160",degrees=0,size = 35,color = kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==3){
    tmpbg <- image_annotate(tmpbg,tmpname,location = "-55+160",degrees=0,size = 35,color = kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==4){
    tmpbg <- image_annotate(tmpbg,tmpname,location = "-55+162",degrees=0,size = 30,color = kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==5){
    tmpbg <- image_annotate(tmpbg,tmpname,location = "-55+165",degrees=0,size = 25,color = kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==11){
    tmpbg <- image_annotate(tmpbg,shipinfo[i,11],location = "-104+140",degrees=0,size = 35,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
    tmpbg <- image_annotate(tmpbg,shipinfo[i,12],location = "-104+175",degrees=0,size = 35,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==12){
    tmpbg <- image_annotate(tmpbg,shipinfo[i,11],location = "-78+140",degrees=0,size = 35,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
    tmpbg <- image_annotate(tmpbg,shipinfo[i,12],location = "-78+175",degrees=0,size = 35,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==13){
    tmpbg <- image_annotate(tmpbg,shipinfo[i,11],location = "-55+140",degrees=0,size = 35,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
    tmpbg <- image_annotate(tmpbg,shipinfo[i,12],location = "-55+175",degrees=0,size = 35,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==14){
    tmpbg <- image_annotate(tmpbg,shipinfo[i,11],location = "-45+140",degrees=0,size = 30,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
    tmpbg <- image_annotate(tmpbg,shipinfo[i,12],location = "-55+175",degrees=0,size = 30,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
  }else if(tmpsizeclass==15){
    #just throwing in oktober
    tmpbg <- image_annotate(tmpbg,shipinfo[i,11],location = "-55+150",degrees=0,size = 25,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
    tmpbg <- image_annotate(tmpbg,shipinfo[i,12],location = "-55+175",degrees=0,size = 25,color=kaicolour, font = "Georgia", weight=700, gravity = "center")
  }
  
  #add form
  
  tmpbg <- image_annotate(tmpbg,shipinfo[i,4], location = "-104+200", size = formsize, color = formcolour, font = "Georgia", weight = 700, gravity = "center")
  
  
  #add bottom border
  
  tmpbgc <- c(tmpbg,frameB)
  tmpbg <- image_mosaic(tmpbgc)
  
  #print
  image_write(tmpbg,paste0("Results/",newnames[i]))
  print(i)
}

