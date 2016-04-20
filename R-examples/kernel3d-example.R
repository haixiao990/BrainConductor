library(ks)
library(MASS)
x <- iris[,1:3]
H.pi <- Hpi(x, pilot="samse")
H2 = H.pi
fhat <- kde(x, H=H.pi, compute.cont=TRUE)  
material3d(shininess = 0, lit = F, specular = "#000000")
plot(fhat, drawpoints=TRUE, axes = F, box = F)
clear3d(type = "lights")
light3d(specular = "black")
light3d(diffuse = rgb(0.3, 0.3, 0.3), specular = "black")

plot(fhat, drawpoints = F, cont = c(75), xlab = "", ylab = "", zlab ="",
     alpha = 1, alphavec = 1)

#let's try sampling from a cube
x = matrix(0, ncol = 3, nrow = 5^3)
x[,1] = rep(1:5, each = 25)
x[,2] = rep(rep(1:5, each = 5), by = 5)
x[,3] = rep(1:5, by = 25)
x = as.data.frame(x)

H.pi = matrix(1, ncol = 3, nrow = 3)
diag(H.pi) = 10

fhat = kde(x, H=H.pi, compute.cont = T )

plot(fhat, drawpoints = T, cont = c(75), xlab = "", ylab = "", zlab ="",
     alpha = 1, alphavec = 1)

######################################

#try it on AAL now
setwd("C:/Users/UikosPC/Dropbox/Collaboration and People/Felix_Kevin/Felix_Kevin_Han-seniorthesis2015-16/data")
library(oro.nifti)
library(rgl)
library(misc3d)
library(assertthat)

template <- readNIfTI("MNI152_T1_2mm_brain.nii.gz")
template <- template@.Data
dtemp <- dim(template)


target.val = c(9032, 9140) #cerebellum_4_5_R and Vermis_7

aal = readNIfTI("AAL_MNI_2mm.nii")
aal = aal@.Data

par3d("windowRect" = c(0,23,1366,728))
contour3d(template, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3],
          level = 5000, alpha = .2, 
          #screen = list(x = 90, y = 90, z = 90), 
          light = c(0,0,-1))

aal1 = aal
aal1[which(aal != target.val[1])] = 0

#convert aal1 into 3d coordinates
idx = which(aal1 != 0, arr.ind = T)

H.pi <- Hpi(idx, pilot="samse")

H.pi = matrix(1, ncol = 3, nrow = 3)
diag(H.pi) = 3

fhat = kde(idx, H=H.pi, compute.cont = T )

plot(fhat, drawpoints = T, cont = c(60), xlab = "", ylab = "", zlab ="",
     alpha = 1, alphavec = 1, colors = rgb(0,1,0),
     xlim = c(1,dtemp[1]), ylim = c(1, dtemp[2]), zlim = c(1, dtemp[3]))

colfunc<-colorRampPalette(c(rgb(252, 245, 192, max = 255), 
                            rgb(186, 109, 26, max = 255)))

plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)

empty = array(0, dim = c(dtemp[1],dtemp[2],dtemp[3]))
contour3d(empty, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3],
          level = 0, alpha = .2, 
          #screen = list(x = 90, y = 90, z = 90), 
          light = c(0,0,-1))
