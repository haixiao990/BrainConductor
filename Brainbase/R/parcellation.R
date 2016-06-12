fit.parcellation2template <- function(parcellation, template){
  assert_that(class(parcellation@data) == "BCoData")
  assert_that(class(template@data) == "BCoData")

  #find out the "zero.voxels" which are ones in
  # mni but not in aal
  idx.aal = which(aal != 0)
  idx.aal = idx.aal[idx.aal %in% mask] 
  idx.aalconvert = mapvalues(idx.aal, mask, 
   1:max(edge.mat), warn_missing = F)  
  #note: idx.aal are voxel locations in 3D, but idx.aalconvert
  # are values that go from 1:max(edge.mat)

 #form the base.line graph
  zero.voxels = which(!mask %in% idx.aal)
  n = max(edge.mat)
  g = .construct.graphBase(zero.voxels, edge.mat, n)
  
  #add in all the parcellations in aal
  aal.val = as.numeric(aal)[idx.aal]
  uniq.aal = unique(aal.val)

  old.comp = components(g)$no

  for(i in 1:length(uniq.aal)){
    idx.parcel = which(aal.val == uniq.aal[i])
    voxels.inParcel = idx.aalconvert[idx.parcel]


    if(enforce.connectivity){
      #simply add each edge to each other in a chain graph
      edges.toadd = matrix(0, nrow = length(voxels.inParcel) - 1, ncol = 2)
      edges.toadd[,1] = voxels.inParcel[1:(length(voxels.inParcel)-1)]
      edges.toadd[,2] = voxels.inParcel[2:length(voxels.inParcel)]

    } else {
      #find the relevant edges in edge.mat
      leftRight.bool = alply(edge.mat, 2, function(x){
        which(x %in% voxels.inParcel)
      })
    
      edges.inParcel = intersect(leftRight.bool[[1]], leftRight.bool[[2]])
      edges.toadd = edge.mat[edges.inParcel,]
    }  

    #add the selected edges into the graph g
    g = add.edges(g, t(edges.toadd))
  
    if(verbose){
      #check if the components went down the right amount
      new.comp = components(g)$no
      if(old.comp - new.comp != length(idx.parcel)-1){
        print(paste0("PARCEL: ", uniq.aal[i], " for i = ", i, " with supposed decrease in ",
         length(idx.parcel)-1, " but actual drop in ", old.comp - new.comp))
      }
      old.comp = new.comp

      if(i %% floor(length(uniq.aal)/10) == 0) cat('*')
    }
  }

  as.factor(components(g)$membership)  
}


.construct.graphBase <- function(zero.voxels, edge.mat, n, verbose = FALSE){
  assert_that(is.numeric(n) & length(n) == 1)
  assert_that(is.matrix(edge.mat) & is.numeric(edge.mat))#initialize the graph
 
  #initialize the graph
  g = graph.empty(n, directed = F)
  
  #determine which columns are all 0
  iter = 1

  #do this recursively: find all edge-pairs such that one edge
  #  is not in zero.voxel and one edge is. Add that edge and
  #  move the zero.voxel into the non-zero.voxel list.
  #  Continue until there are no more zero.voxels
  while (length(zero.voxels) > 0 & nrow(edge.mat) > 0) {
    bool.mat = apply(edge.mat, 2, function(x){x %in% zero.voxels})

    #find voxels between nonzero and zero
    sum.mat = apply(bool.mat, 1, sum)
    boundary.edgeidx = which(sum.mat == 1)  

    #now for the annoying part: you want to make sure you don't add
    # the same zero.voxel in twice
    boundary.edge = edge.mat[boundary.edgeidx,]
    idx = which(boundary.edge %in% zero.voxels)
    accounted.voxels = boundary.edge[idx]
    duplicated.zerovoxels = duplicated(accounted.voxels)
    #if(sum(duplicated.zerovoxels) > 0) {
      idx = idx[-which(duplicated.zerovoxels == TRUE)]
    #}
 
    #now remove the duplicated zero.voxels
    idx = idx %% nrow(boundary.edge)
    idx[idx == 0] = nrow(boundary.edge)
    boundary.edge = boundary.edge[idx,]

    #add edges to g
    g = add.edges(g, t(boundary.edge))

    #mark zero edges as nonzero
    zero.voxels = zero.voxels[!zero.voxels %in% 
     unique(as.vector(boundary.edge))]

    #remove extra edges
    edge.mat = edge.mat[-which(sum.mat == 0),, drop = FALSE]

    if(verbose) {
      print(iter)
      iter = iter + 1
      print(paste0("Length of zero.voxels: ", length(zero.voxels)))
      print(paste0("Edge count: ", ecount(g)))
      print(paste0("Dim of edges: ", nrow(edge.mat)))
    }
  }

  g
}


