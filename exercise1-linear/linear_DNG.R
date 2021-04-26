# Building DNG RAW files from Bayer TIFF data
# Exercise 1: linear light decomposition
# www.overfitting.net
# https://www.overfitting.net/2021/04/generando-un-raw-en-formato-dng-partir.html

library(tiff)


# PARAMETERS
N=3  # number of RAW files to process
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW files must be named: raw1.dng (Ambiente + Artificial),
# raw2.dng (Ambiente), raw3.dng (Artificial for check)
# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -t 0 -4 -T *.dng
img=list()
for (i in 1:N) img[[i]]=readTIFF(paste0(NAME, i, ".tiff"), native=F, convert=F)


# LINEAR SUBTRACTION
imag=img[[1]]-img[[2]]  # (Ambiente + Artificial) - Ambiente
imag[imag<0]=0  # clip negative subtractions


# BUILD OUTPUT DNG
writeTIFF(imag, paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")



# Comparison composite (1/3, 1/3, 1/3)
DIMTHIRD=as.integer(ncol(imag)/3)
INITCOL=3515
imag[,1:DIMTHIRD]=img[[1]][,INITCOL:(INITCOL+DIMTHIRD-1)]
imag[,(DIMTHIRD+1):(2*DIMTHIRD)]=img[[2]][,INITCOL:(INITCOL+DIMTHIRD-1)]
imag[,(2*DIMTHIRD+1):(3*DIMTHIRD)]=img[[3]][,INITCOL:(INITCOL+DIMTHIRD-1)]
writeTIFF(imag, paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
