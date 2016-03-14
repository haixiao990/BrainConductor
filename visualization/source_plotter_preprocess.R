plot.motion <- function(img, location = NA, threshold = 0, filename = NA){
  assert_that(length(dim(img))==4)
  
  dimen = dim(img)[1:3]
  time = dim(img)[4]
 
  if(is.na(location[1])) location = floor(dimen/2)  

  #red: all brain
  #blue: almost all brain
  #white: almost no brain
  #black: no brain
  col.vec = c(rgb(1,0,0), colorRampPalette(c("blue", "white"))(19) ,rgb(0,0,0))
  
  if(!is.na(filename)) png(filename, height=4, width=6, units="in", res=600)
  
  par(mar = rep(0.2,4), bg="black", mfrow = c(1,3))
  
  for(i in 1:3){
    if(i==1) imgtmp = img[location[1],,,]
    if(i==2) imgtmp = img[,location[2],,]
    if(i==3) imgtmp = img[,,location[3],]
    
    if(threshold>0) imgtmp[which(imgtmp<threshold)] = 0

    proj = apply(imgtmp, c(1,2), function(x){length(which(x==0))/length(x)})
    
    image(proj, col=col.vec, breaks = -1/19+c(1:22)*1/20, asp = dim(proj)[2]/dim(proj)[1],
          bty="n", xaxt="n", yaxt="n")
  }
  
  if(!is.na(filename)) dev.off()
  
  invisible()
}

plot.masking <- function(img1, img2, location = NA, filename = NA){
  assert_that(length(dim(img1))==3 & length(dim(img2))==3)

  if(is.na(location[1])) location = floor(dimen/2)

  if(!is.na(filename)) png(filename, height=4, width=6, units="in", res=600)

  par(mar = rep(0.2,4), bg="black", mfrow = c(1,3))

  for(i in 1:3){
    if(i==1) imgtmp1 = img1[location[1],,]; imgtmp2 = img2[location[1],,]
    if(i==2) imgtmp1 = img1[,location[2],]; imgtmp2 = img2[,location[2],]
    if(i==3) imgtmp1 = img1[,,location[3]]; imgtmp2 = img2[,,location[3]]

    imgcomb = array(NA, dim=c(dim(imgtmp1),2))
    imgcomb[,,1] = imgtmp1
    imgcomb[,,2] = imgtmp2

    #return true if both are larger than 0 or both equal to 0
    imgtmp = apply(imgcomb, c(1,2), function(x){
     (max(abs(x))>0) == (min(abs(x))>0)
    })

    image(imgtmp)
  }

  if(!is.na(filename)) dev.off()

  invisible()

}
