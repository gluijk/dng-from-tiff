# Building DNG RAW files from Bayer TIFF data
# Exercise 11: antimedian stacking
# www.overfitting.net
# https://www.overfitting.net/2022/11/apilado-por-antimediana-para-replicar.html

library(tiff)

# Improve the performance of the R median function with this C++ code:
# https://stackoverflow.com/questions/34771088/why-is-standard-r-median-function-so-much-slower-than-a-simple-c-alternative

library(Rcpp)
cppFunction('
            double cpp_med2(Rcpp::NumericVector xx) {
            Rcpp::NumericVector x = Rcpp::clone(xx);
            std::size_t n = x.size() / 2;
            std::nth_element(x.begin(), x.begin() + n, x.end());
            
            if (x.size() % 2) return x[n]; 
            // return (*std::max_element(x.begin(), x.begin() + n) + x[n]) / 2.;
            // This is simpler:
            return (x[n-1] + x[n]) / 2.;
            }
            ')

antimedian = function(x) {  # antimedian function
    absdev = abs(x - cpp_med2(x))  # absdev = abs(x-median(x))
    x[absdev == max(absdev)][1]  # [1] to return single value
}

statmode = function(x) {  # statistical mode function
    ux=unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}


# PARAMETERS
N=11  # number of RAW files to merge
NAME="raw"  # input RAW filenames
OUTNAME="bayer"  # output RAW composite filename
BLACK=512  # sensor black level (Sony A7 II)
SAT=16376  # sensor sat level (Sony A7 II)


# READ RAW DATA

# RAW integer extraction using DCRAW: dcraw -v -D -4 -T *.dng
# IMPORTANT: note that -D DCRAW extraction instead of -d is used
img=readTIFF(paste0(NAME, 1, ".tiff"), native=FALSE, convert=FALSE)
img=array(0, c(nrow(img), ncol(img), N))
for (i in 1:N) {
    img[,,i]=readTIFF(paste0(NAME, i, ".tiff"),
                      native=FALSE, convert=FALSE, as.is=TRUE)
}


# SEQUENCE AVERAGING
# c(1,2) means 1st and 2nd dimensions
# imag=apply(img, c(1,2), mean)  # mean
# imag=apply(img, c(1,2), cpp_med2)  # median (optimised)
imag=apply(img, c(1,2), antimedian)  # antimedian

# Fusion map and RAW data files contributions
mapafusion=imag*0
for (i in 1:N) {
    indices=which(imag==img[,,i])
    mapafusion[indices]=i
}
writeTIFF((mapafusion-1)/(N-1), "mapafusion.tif", # grayscale fusion map
    bits.per.sample=8, compression="LZW")
for (i in 1:N) print(paste0("Contribution of ", NAME, i, ".tiff: ",
    round(length(which(mapafusion==i))/length(mapafusion)*100,2),"%"))


# IMPROVE FUSION MAP TO REDUCE MOVING/BAYER ARTIFACTS
DIMX=nrow(mapafusion)
DIMY=ncol(mapafusion)


# REFINEMENT 1: 9px mode
# Calculate statistical mode over a 3x3 pixels matrix
mapa=array(0, c(DIMX, DIMY, 9))
mapa[2:(DIMX-1), 2:(DIMY-1), 1] = mapafusion[(2-1):(DIMX-1-1), (2-1):(DIMY-1-1)]  # -1, -1
mapa[2:(DIMX-1), 2:(DIMY-1), 2] = mapafusion[(2):(DIMX-1), (2-1):(DIMY-1-1)]  #  0, -1
mapa[2:(DIMX-1), 2:(DIMY-1), 3] = mapafusion[(2+1):(DIMX-1+1), (2-1):(DIMY-1-1)]  # +1, -1
mapa[2:(DIMX-1), 2:(DIMY-1), 4] = mapafusion[(2-1):(DIMX-1-1), (2):(DIMY-1)]  # -1,  0
mapa[2:(DIMX-1), 2:(DIMY-1), 5] = mapafusion[(2):(DIMX-1), (2):(DIMY-1)]  #  0,  0
mapa[2:(DIMX-1), 2:(DIMY-1), 6] = mapafusion[(2+1):(DIMX-1+1), (2):(DIMY-1)]  # +1,  0
mapa[2:(DIMX-1), 2:(DIMY-1), 7] = mapafusion[(2-1):(DIMX-1-1), (2+1):(DIMY-1+1)]  # -1, +1
mapa[2:(DIMX-1), 2:(DIMY-1), 8] = mapafusion[(2):(DIMX-1), (2+1):(DIMY-1+1)]  #  0, +1
mapa[2:(DIMX-1), 2:(DIMY-1), 9] = mapafusion[(2+1):(DIMX-1+1), (2+1):(DIMY-1+1)]  # +1, +1
mapafusion2=apply(mapa, c(1,2), statmode)

# REFINEMENT 2: 21px mode
# Calculate statistical mode over a 3x3 pixels matrix + 12 surrounding pixels
mapa=array(0, c(DIMX, DIMY, 9+12))
mapa[3:(DIMX-2), 3:(DIMY-2), 1] = mapafusion[(3-1):(DIMX-2-1), (3-1):(DIMY-2-1)]  # -1, -1
mapa[3:(DIMX-2), 3:(DIMY-2), 2] = mapafusion[(3):(DIMX-2), (3-1):(DIMY-2-1)]  #  0, -1
mapa[3:(DIMX-2), 3:(DIMY-2), 3] = mapafusion[(3+1):(DIMX-2+1), (3-1):(DIMY-2-1)]  # +1, -1
mapa[3:(DIMX-2), 3:(DIMY-2), 4] = mapafusion[(3-1):(DIMX-2-1), (3):(DIMY-2)]  # -1,  0
mapa[3:(DIMX-2), 3:(DIMY-2), 5] = mapafusion[(3):(DIMX-2), (3):(DIMY-2)]  #  0,  0
mapa[3:(DIMX-2), 3:(DIMY-2), 6] = mapafusion[(3+1):(DIMX-2+1), (3):(DIMY-2)]  # +1,  0
mapa[3:(DIMX-2), 3:(DIMY-2), 7] = mapafusion[(3-1):(DIMX-2-1), (3+1):(DIMY-2+1)]  # -1, +1
mapa[3:(DIMX-2), 3:(DIMY-2), 8] = mapafusion[(3):(DIMX-2), (3+1):(DIMY-2+1)]  #  0, +1
mapa[3:(DIMX-2), 3:(DIMY-2), 9] = mapafusion[(3+1):(DIMX-2+1), (3+1):(DIMY-2+1)]  # +1, +1
#
mapa[3:(DIMX-2), 3:(DIMY-2), 10] = mapafusion[(3-1):(DIMX-2-1), (3-2):(DIMY-2-2)]  # -1, -2
mapa[3:(DIMX-2), 3:(DIMY-2), 11] = mapafusion[(3):(DIMX-2), (3-2):(DIMY-2-2)]  #  0, -2
mapa[3:(DIMX-2), 3:(DIMY-2), 12] = mapafusion[(3+1):(DIMX-2+1), (3-2):(DIMY-2-2)]  # +1, -2
mapa[3:(DIMX-2), 3:(DIMY-2), 13] = mapafusion[(3-1):(DIMX-2-1), (3+2):(DIMY-2+2)]  # -1, +2
mapa[3:(DIMX-2), 3:(DIMY-2), 14] = mapafusion[(3):(DIMX-2), (3+2):(DIMY-2+2)]  #  0, +2
mapa[3:(DIMX-2), 3:(DIMY-2), 15] = mapafusion[(3+1):(DIMX-2+1), (3+2):(DIMY-2+2)]  # +1, +2
mapa[3:(DIMX-2), 3:(DIMY-2), 16] = mapafusion[(3-2):(DIMX-2-2), (3-1):(DIMY-2-1)]  # -2, -1
mapa[3:(DIMX-2), 3:(DIMY-2), 17] = mapafusion[(3-2):(DIMX-2-2), (3):(DIMY-2)]  #  -2, 0
mapa[3:(DIMX-2), 3:(DIMY-2), 18] = mapafusion[(3-2):(DIMX-2-2), (3+1):(DIMY-2+1)]  # -2, +1
mapa[3:(DIMX-2), 3:(DIMY-2), 10] = mapafusion[(3+2):(DIMX-2+2), (3-1):(DIMY-2-1)]  # +2, -1
mapa[3:(DIMX-2), 3:(DIMY-2), 20] = mapafusion[(3+2):(DIMX-2+2), (3):(DIMY-2)]  #  +2, 0
mapa[3:(DIMX-2), 3:(DIMY-2), 21] = mapafusion[(3+2):(DIMX-2+2), (3+1):(DIMY-2+1)]  # +2, +1
mapafusion2=apply(mapa, c(1,2), statmode)

# REFINEMENT 3: 49px mode
# Calculate statistical mode over a R=4 pixles circle
mapafusion2=mapafusion*0
R=4  # circle radius
R2=R^2
for (x in (1+R):(DIMX-R)) {  # slow loop...
    for (y in (1+R):(DIMY-R)) {
        valores=c()
        for (i in -R:R) {
            for (j in -R:R) {
                if (i*i + j*j <= R2) valores=c(valores, mapafusion[x+i, y+j])
            }
        }
        mapafusion2[x,y]=statmode(valores)
    }
}


mapafusion2[mapafusion2==0]=1  # rescue borders
writeTIFF((mapafusion2-1)/(N-1), "mapafusion2.tif",  # grayscale fusion map
          bits.per.sample=8, compression="LZW")
for (i in 1:N) print(paste0("Contribution of ", NAME, i, ".tiff: ",
    round(length(which(mapafusion2==i))/length(mapafusion2)*100,2),"%"))


# Use refined fusion map
imag2=imag*0
for (i in 1:N) {
    indices=which(mapafusion2==i)
    imag2[indices]=img[,,i][indices]
}


# BUILD OUTPUT DNG
imag=imag2  # run this to use improved fusion map

# Normalize RAW data
imag=imag-BLACK  # subtract black level
imag[imag<0]=0  # clip negative values
imag=imag/(SAT-BLACK)  # normalize to 0..1
hist(imag, breaks=1024)

if (max(imag)<1) print(paste0("Output ETTR'ed by: +",
                              round(-log(max(imag),2),2), "EV"))
writeTIFF(imag/max(imag), paste0(OUTNAME,".tif"), bits.per.sample=16,
          compression="none")
