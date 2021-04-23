# Building DNG RAW files from Bayer TIFF data
# Exercise 2: linear light decomposition
# www.overfitting.net
# https://www.overfitting.net/2021/04/generando-un-raw-en-formato-dng-partir.html

library(tiff)


# PARAMETERS
N=2  # number of RAW files
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW files must be named: raw1.dng, raw2.dng
# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -4 -T *.dng
img=list()
txt=list()
for (i in 1:N) {
    img[[i]]=readTIFF(paste0(NAME, i, ".tiff"), native=F, convert=F)
    txt[[i]]=paste0(NAME, i, "vs", NAME, i+1)
}


# LINEAR SUBTRACTION
imag=img[[1]]-img[[2]]  # (Ambiente + Artificial) - Ambiente
imag[imag<0]=0  # clip negative subtractions


# BUILD OUTPUT DNG
writeTIFF(imag, paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
