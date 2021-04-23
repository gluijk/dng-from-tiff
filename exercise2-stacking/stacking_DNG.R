# Fusión HDR de archivos RAW con R
# www.overfitting.net
# https://www.overfitting.net/2018/07/fusion-hdr-de-imagenes-con-r.html

rm(list=ls())
library(tiff)


# PARAMETERS
N=16  # number of RAW files to merge
NAME="_DSC38"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -4 -T *.dng
img=readTIFF(paste0(NAME, 18, ".tiff"), native=F, convert=F)
for (i in 19:(19+N-2)) {
    img=img+readTIFF(paste0(NAME, i, ".tiff"), native=F, convert=F)
}


# AVERAGING
img=img/N


# BUILD OUTPUT DNG
writeTIFF(img/max(img), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
writeTIFF(img, paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")