# Building DNG RAW files from Bayer TIFF data
# Exercise 8: max light
# www.overfitting.net
# https://www.overfitting.net/2022/01/acumulando-luz-en-raw.html

library(tiff)


# PARAMETERS
N=12  # number of RAW files to merge
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -D -t 0 -4 -T *.dng
# dcraw -v -d -r 1 1 1 1 -t 0 -k 512 -S 16383 -4 -T *.dng
# doesn't seem to work properly with these RAW files

BLACK=512/65535  # 512 obtained by visual inspection

imag=readTIFF(paste0(NAME, 1, ".tiff"), native=FALSE, convert=FALSE)
mapafusion=imag*0+1
for (i in 2:N) {
    img=readTIFF(paste0(NAME, i, ".tiff"), native=FALSE, convert=FALSE)
    indices=which(img>imag)  # keep any higher value
    imag[indices]=img[indices]
    mapafusion[indices]=i
}

# Linearize and normalize
imag=imag-BLACK  # linearize
imag[imag<0]=0  # clip to 0 negative values


# BUILD OUTPUT DNG
if (max(imag)<1) print(paste0("Output ETTR'ed by: +",
                             round(-log(max(imag),2),2), "EV"))
writeTIFF(imag/max(imag), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")

# Fusion map and RAW data files contributions
writeTIFF((mapafusion-1)/(N-1), "mapafusion.tif",  # grayscale fusion map
          bits.per.sample=8, compression="LZW")
for (i in 1:N) print(paste0("Contribution of ", NAME, i, ".tiff: ",
                            round(length(which(mapafusion==i))/length(mapafusion)*100,2),"%"))
