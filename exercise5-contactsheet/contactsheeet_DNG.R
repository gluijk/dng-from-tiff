# Building DNG RAW files from Bayer TIFF data
# Exercise 5: RAW subsampling contact sheet
# www.overfitting.net
# https://www.overfitting.net/2021/04/generando-un-raw-en-formato-dng-partir.html

library(tiff)


# PARAMETERS
N=16  # number of RAW files to merge
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename

# READ RAW DATA

# RAW files must be named: raw1.dng, raw2.dng,...
# RAW extraction using DCRAW: dcraw -v -d -r 1 1 1 1 -4 -T *.dng
imag=readTIFF(paste0(NAME, 1, ".tiff"), native=F, convert=F, as.is=TRUE)*0
DIEZMADO=4
NROW=nrow(imag)/DIEZMADO
NCOL=ncol(imag)/DIEZMADO

tmp=array(0,c(NROW,NCOL))  # temporary array

# Precalculated indexes
i=which((row(imag)-1)%%(DIEZMADO*2)==0 & (col(imag)-1)%%(DIEZMADO*2)==0)
j=which(row(tmp)%%2 & col(tmp)%%2)
for (k in 1:N) {
    img=readTIFF(paste0(NAME, k, ".tiff"), native=F, convert=F, as.is=TRUE)
    
    tmp[j]=img[i]  # R
    tmp[j+NROW]=img[i+NROW*DIEZMADO*(DIEZMADO-1)]  # G1
    tmp[j+1]=img[i+DIEZMADO-1]  # G2
    tmp[j+NROW+1]=img[i+NROW*DIEZMADO*(DIEZMADO-1)+DIEZMADO-1]  # B
    
    OFFSETROW=NROW*as.integer((k-1)/DIEZMADO)
    OFFSETCOL=NCOL*(k-1)%%DIEZMADO
    imag[(1+OFFSETROW):(NROW+OFFSETROW),(1+OFFSETCOL):(NCOL+OFFSETCOL)]=tmp
}


# BUILD OUTPUT DNG
if (max(imag)<1) print(paste0("Output ETTR'ed by: +",
                             round(-log(max(imag),2),2), "EV"))
writeTIFF((imag/max(imag)), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
