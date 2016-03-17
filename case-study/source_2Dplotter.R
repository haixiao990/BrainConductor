library(oro.nifti)
source("~/Brainconductor.git/Brainbase/R/conversion.R")

#plot.graph <- function(){
  #average the points in AAL to represent each parcel as a point
  #first convert all the indices into 3d coordinates
  #then average all the 3d coordinates
aal = readNIfTI("~/Brainconductor.git/data/AAL_MNI_2mm.nii")
aal = aal@.Data
dimen = dim(aal)
idx = unique(as.numeric(aal))[-1]

aal.mat = matrix(0, ncol = 3, nrow = length(idx))
for(i in 1:length(idx)){
  tmp = which(aal == idx[i])
  loc3d = sapply(1:length(tmp), function(x){.convert.2Dto3Dloc(tmp[x], dimen)})
  aal.mat[i,] = apply(t(loc3d), 2, mean)

  if(i %% floor(length(idx)/10) == 0) cat('*')
}

  #decide on a viewpoint perspective. make the first plot
  #for the first plot, plot the connections in order of the furthest away
  #  point in the perspective (sorted)
  #red lines = edge in control not in case, green means edge in case not in
  #  control, the thickness controls the magnitude
  #label only the 10 top nodes with the most degree

  #plot in the axial direction (3rd dimension)

#first sort the edges in med.graph
med.graph[lower.tri(med.graph)] = 0
edge.idx = which(med.graph != 0, arr.ind = T)
edge.val = apply(edge.idx, 1, function(x){med.graph[x[1],x[2]]})

#sort each pair first
for(i in 1:nrow(edge.idx)){
  val = edge.idx[i,]
  pos1 = aal.mat[val[1],]
  pos2 = aal.mat[val[2],]

  if(pos1[3] > pos2[3]) edge.idx[i,] = c(val[2], val[1])
}
#order the pairs
pos.vec = sapply(edge.idx[,1], function(x){ aal.mat[x,3] })

reordering = order(pos.vec, decreasing = F)
edge.idx = edge.idx[reordering, ]
edge.val = edge.val[reordering]

#par(mfrow = c(1,2))
png("~/DUMP_figures/aal_medgraph.png", height = 5, width = 5, res = 300, units = "in")
plot(NA, xlim = c(1, dimen[1]), ylim = c(1, dimen[2]), asp = T)

red = c(219, 51, 64)
green = c(35 ,139, 42)

for(i in 1:nrow(edge.idx)){
  pos1 = aal.mat[edge.idx[i,1],]
  pos2 = aal.mat[edge.idx[i,2],]
  if(edge.val[i]>0) col = green else col = red

  lines(x = c(pos1[1], pos2[1]), y = c(pos1[2], pos2[2]),
    col = rgb(col[1], col[2], col[3], 255*((abs(edge.val[i])-6)*.07+0.3), max = 255), 
    lwd = (abs(edge.val[i])-6)*0.5625+0.5)
}  
dev.off()

  #in the second plot, again in order or points furthest away,
  #plot a circle for the degree (let's do it in ... red again?)

#fk the ordering for now
deg = apply(med.graph, 2, function(x){length(which(x!=0))})

png("~/DUMP_figures/aal_medgraph_points.png", height = 5, width = 5, res = 300, units = "in")
plot(NA, xlim = c(1, dimen[1]), ylim = c(1, dimen[2]), asp = T)

#REQUIRES FOR THE LOWER TRI TO NOT BE ZERO'D OUT
for(i in 1:nrow(aal.mat)){
  points(aal.mat[i,1], aal.mat[i,2], pch = 16, cex = deg[i]*.21+0.1, 
    col = rgb(red[1], red[2], red[3], 255*(deg[i]*0.05+.5), max = 255))
}

dev.off()
#}
