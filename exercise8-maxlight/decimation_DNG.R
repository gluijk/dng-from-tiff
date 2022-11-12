# Building DNG RAW files from Bayer TIFF data
# Exercise 7: RAW decimation
# www.overfitting.net
# https://www.overfitting.net/2021/11/diezmado-de-bits-en-raw.html

library(tiff)


# PARAMETERS
NAME="gh5s"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename


# READ RAW DATA

# RAW extraction using DCRAW: dcraw -v -D -t 0 -4 -T *.dng
# dcraw -v -d -r 1 1 1 1 -t 0 -k 512 -4 -T *.dng
# doesn't work properly with this DNG clipping to 0 a lot of R and G pixels
img=readTIFF(paste0(NAME, ".tiff"), native=FALSE, convert=FALSE)


# Linearize and normalize
BLACK=512/65535  # 512 obtained by visual inspection
img=img-BLACK  # linearize
img[img<0]=0  # clip to 0 negative values
img=img/max(img)  # normalize 0..1
imgout=img*0


# Cropping deep shadows
# Total area: 3744 x 2776
# Image area: 3744-36 x 2776 = 3708 x 2776
# Crop: 1236 x 924

# Bit decimation
for (bits in seq(6, 14)) {
    cropdec=img[745:1668, 1:1236]  # deep shadows
    cropdec=round(cropdec*(2^bits-1))/(2^bits-1)  # round into 2^bits int values

    if (bits==8)  imgout[1:924,     1:1236]=cropdec
    if (bits==7)  imgout[925:1848,  1:1236]=cropdec
    if (bits==6)  imgout[1849:2772, 1:1236]=cropdec
    if (bits==11) imgout[1:924,     1237:2472]=cropdec
    if (bits==10) imgout[925:1848,  1237:2472]=cropdec
    if (bits==9)  imgout[1849:2772, 1237:2472]=cropdec
    if (bits==14) imgout[1:924,     2473:3708]=cropdec
    if (bits==13) imgout[925:1848,  2473:3708]=cropdec
    if (bits==12) imgout[1849:2772, 2473:3708]=cropdec
}

# Write labels
labels=readTIFF(paste0("labels.tif"), native=FALSE, convert=FALSE)
labelsindices=which(labels>0)
imgout[labelsindices]=labels[labelsindices]

writeTIFF(imgout, paste0(OUTNAME,".tif"),
          bits.per.sample=16, compression="none")



# Soft RAW histogram to measure DR
imgdr=img+rnorm(length(img), sd=1/4000)  # soften deep shadows
imgdr[imgdr<0]=0
imgdr[imgdr>1]=1
writeTIFF(imgdr^(1/2.2), "drhistogram.tif",
          bits.per.sample=16, compression="none")


# Distortion grid measurement
indices=which(col(imgout)%%150==0 | (col(imgout)+1)%%150==0)
imgout[indices]=1

indices=which(row(imgout)%%150==0 | (row(imgout)+1)%%150==0)
imgout[indices]=1
