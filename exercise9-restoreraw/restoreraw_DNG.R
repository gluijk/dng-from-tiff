# Building DNG RAW files from Bayer TIFF data
# Exercise 9: RAW restoration
# www.overfitting.net
# https://www.overfitting.net/2022/02/restaurando-un-archivo-raw.html

library(tiff)


# PARAMETERS
NAME="rayas"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename
BLACK=2046  # sensor black level
SAT=13583

# READ RAW DATA

# RAW integer extraction using DCRAW: dcraw -v -D -t 0 -4 -T *.dng
# IMPORTANT: note that -D DCRAW extraction instead of -d is used
img=readTIFF(paste0(NAME, ".tiff"), native=F, convert=F, as.is=TRUE)

img=img-BLACK  # subtract black level
img[img<0]=0  # clip negative values
img=img/(SAT-BLACK)  # normalize to 0..1
writeTIFF(img, paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")

img2=img
NROW=nrow(img2)


# METHOD 1

# G and B photosites
i=which(!col(img2)%%8)  # affected pixels (1 out of each 8 columns)
img2[i]=(img2[i-2*NROW]+img2[i+2*NROW])/2  # replace averaging both sides

writeTIFF(img2, paste0(OUTNAME,"2.tif"), bits.per.sample=16,
          compression="none")


# METHOD 2

# B photosites
i=which(!col(img2)%%8 & !row(img2)%%2)
img2[i]=(img2[i-2*NROW]+img2[i+2*NROW])/2

# G photosites
i=which(!col(img2)%%8 & row(img2)%%2)
img2[i]=(img2[i-NROW-1]+img2[i-NROW+1]+img2[i+NROW-1]+img2[i+NROW+1])/4

writeTIFF(img2, paste0(OUTNAME,"3.tif"), bits.per.sample=16,
          compression="none")


# Before running the last part of dngmaker.bat do:
# exiftool -overwrite_original -Activearea="0 0 3465 5202" rayas.dng
# to adapt to the size of rayas.tiff: 3465 x 5202