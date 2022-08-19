# Building DNG RAW files from Bayer TIFF data
# Exercise 10: RAW distortion correction
# www.overfitting.net
# https://www.overfitting.net/2022/02/restaurando-un-archivo-raw.html

library(tiff)


# PARAMETERS
NAME="leicaq2"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename
BLACK=512  # sensor black level (Leica Q2)
SAT=16383  # sensor sat level (Leica Q2)

# READ RAW DATA

# RAW integer extraction using DCRAW: dcraw -v -D -t 0 -4 -T *.dng
# IMPORTANT: note that -D DCRAW extraction instead of -d is used
# 8392 x 5632 (DCRAW) -> 8368 x 5584 (ACR) ((12,24) in corners)
# Active Area: 0 0 5632 8392
img=readTIFF(paste0(NAME, ".tiff"), native=FALSE, convert=FALSE, as.is=TRUE)

img=img-BLACK  # subtract black level
img[img<0]=0  # clip negative values
img=img/(SAT-BLACK)  # normalize to 0..1


# DRAW GRIDS
NROW=nrow(img)
NCOL=ncol(img)

img=img/4  # image (-2EV) + grid 
# img=img*0  # only grid

# Draw main grid
i=which(!col(img)%%100)  # grid size=100 pixels
img[i-4*NROW]=1
img[i-4*NROW-NROW]=1
img[i-4*NROW+NROW]=1

i=which(!row(img)%%100)  # grid size=100 pixels
img[i+16]=1
img[i+16-1]=1
img[i+16+1]=1

# Draw axis
XAXIS=round(NCOL/2)
YAXIS=round(NROW/2)
i=which(
    (col(img)>=XAXIS-4 & col(img)<=XAXIS+4) |
    (row(img)>=YAXIS-4 & row(img)<=YAXIS+4)         
)
img[i]=1

# Draw APS frame
cf=1.6  # APS crop factor
XMIN=round(NCOL/2*(1-1/cf))
XMAX=round(NCOL*(1-1/2*(1-1/cf)))
YMIN=round(NROW/2*(1-1/cf))
YMAX=round(NROW*(1-1/2*(1-1/cf)))

i=which(
    col(img)>=XMIN & col(img)<=XMAX &
    ((row(img)>=YMIN-4 & row(img)<=YMIN+4) |
     (row(img)>=YMAX-4 & row(img)<=YMAX+4))
    )
img[i]=1

i=which(
    row(img)>=YMIN & row(img)<=YMAX &
    ((col(img)>=XMIN-4 & col(img)<=XMIN+4) |
     (col(img)>=XMAX-4 & col(img)<=XMAX+4))
    )
img[i]=1


writeTIFF(img, paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
