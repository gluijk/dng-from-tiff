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
imag2=imag
DIMTHIRD=as.integer(ncol(imag2)/3)
INITCOL=3515
imag2[,1:DIMTHIRD]=img[[1]][,INITCOL:(INITCOL+DIMTHIRD-1)]
imag2[,(DIMTHIRD+1):(2*DIMTHIRD)]=img[[2]][,INITCOL:(INITCOL+DIMTHIRD-1)]
imag2[,(2*DIMTHIRD+1):(3*DIMTHIRD)]=img[[3]][,INITCOL:(INITCOL+DIMTHIRD-1)]
writeTIFF(imag2, paste0(OUTNAME,"_composite.tif"), bits.per.sample=16,
          compression="none")

# Set DNG effective size to 6048x4024 (same as DCRAW output):
# exiftool -overwrite_original -DefaultOrigin="0 0" rawcomposite.dng
# exiftool -overwrite_original -DefaultCropSize="6048 4024" rawcomposite.dng


# COLOUR TEMPERATURE EQUALIZATION
wb=readTIFF("relativewb.tif", native=F, convert=F)  # 2 wall reference pixels
wb[1,1,]=wb[1,1,]/max(wb[1,1,])
wb[1,2,]=wb[1,2,]/max(wb[1,2,])
mulart=wb[1,1,]/wb[1,2,]  # linear colour temperature difference

imag3=imag
NROW=nrow(imag3)
i=which(row(imag3)%%2 & col(imag3)%%2)
imag3[i]=imag3[i]*mulart[1]                # R
imag3[i+NROW]=imag3[i+NROW]*mulart[2]      # G1
imag3[i+1]=imag3[i+1]*mulart[2]            # G2
imag3[i+NROW+1]=imag3[i+NROW+1]*mulart[3]  # B

imag3=imag3+img[[2]]  # scaled Artificial + Ambiente
imag3[imag3>1]=1

# BUILD OUTPUT DNG
writeTIFF(imag3, paste0(OUTNAME,"_whitebalanced.tif"), bits.per.sample=16,
          compression="none")
