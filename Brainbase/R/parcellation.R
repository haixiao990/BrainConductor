#WARNING: Make sure this requires igraph
#returns the partition (factor) not the NIdata
fit.parcellation2template <- function(parcellation, template,
 verbose = T){
  assert_that(class(parcellation@data) == "BCoData")
  assert_that(class(template@data) == "BCoData")

  if(class(parcellation@data) == "BCoData4D"){
    mat = get.matrix(parcellation, output2D = F)
    mask = which(mat != 0)
    partition = as.factor(mat[mask])
  } else {
    #WARNING: Does this work?
    mask = parcellation@data@mask
    partition = get.matrix(parcellation)[1,]
  }

  if(class(template@data) == "BCoData4D"){
    t.mat = get.matrix(template, output2D = F)
    t.mask = which(t.mat != 0)
  } else {
    t.mask = template@data@mask
  }

  dimen = dim(parcellation)
  t.dimen = dim(template)
  assert_that(all(dimen[1:3] == t.dimen[1:3]))

  if(length(mask) == length(t.mask) && all(sort(mask) == sort(t.mask))) 
    partition

  #WARNING: This is big function. we need 1) a verbose setting, and 2)
  #  ways to input potential shortcuts
  if(verbose) cat("Extracting neighbors from template\n")
  t.adj.list = .extract.neighbors(t.mask, dimen, verbose = verbose)
  t.edge.mat = .convert.adjList2edgeMat(adj.list)

  #this removes all the elements in mask not in t.mask
  mask = mask[mask %in% t.mask] 
  edge.idx = mapvalues(mask, t.mask, 1:max(t.edge.mat), 
   warn_missing = F)  
  #note: mask are voxel locations in 3D, but edge.idx
  # are values that go from 1:max(edge.mat)

  #form the base.line graph
  #find out the "zero.voxels" which are ones in t.mask but not in mask
  zero.voxels = which(!t.mask %in% mask)
  n = max(edge.mat)
  if(verbose) cat("Filling in holes in template not in parcellation\n")
  g = .construct.graphBase(zero.voxels, edge.mat, n, verbose = verbose)
  
  #add in all the parcellation values in parcellation
  partition = as.numeric(partition)[mask]
  uniq.partition = unique(partition)

  old.comp = components(g)$no

  if(verbose) cat("Transferring parcellation to template\n")
  for(i in 1:length(uniq.partition)){
    idx = which(partition == uniq.partition[i])
    voxels.inParcel = edge.idx[idx]

    edges.toadd = matrix(0, nrow = length(voxels.inParcel) - 1, ncol = 2)
    edges.toadd[,1] = voxels.inParcel[1:(length(voxels.inParcel)-1)]
    edges.toadd[,2] = voxels.inParcel[2:length(voxels.inParcel)]

    #add the selected edges into the graph g
    g = add.edges(g, t(edges.toadd))
  
  }

  as.factor(components(g)$membership)  
}

# convert adj.list into a matrix
.convert.adjList2edgeMat = function(adj.list) {
  v1 = rep(1:length(adj.list), times = lapply(adj.list, length))
  v2 = unlist(adj.list)

  assert_that(length(v1) == length(v2))

  #throw out potential duplicates
  #WARNING: check this
  not.dup = v1 >= v2
  v1 = v1[not.dup]
  v2 = v2[not.dup]
  
  cbind(v1, v2)
}

#WARNING: This could be coded in C?
# given the MNI standard template, find out all the neighbors
# return two things: the mask and list of neighbors
# the pattern dictates how neighbors are defined.
.extract.neighbors = function(mask, dimen, pattern = .cross.enumerate(), 
                             verbose = T) {
  assert_that(all(mask <= prod(dimen)))
 
  neighbor.list = list(mask)

  for(j in 1:length(mask)){
    #convert mask index into pixel-location
    loc = .convert.2Dto3Dloc(mask[j], dimen)
		
    #apply pattern
    neigh = t(apply(pattern,1,function(s, loc){s+loc}, loc=loc))

    #remove the invalid neighbors
    rmv = unique(c(which(neigh<=0,arr.ind=TRUE)[,1],
     which(neigh[,1]>dimen[1]), which(neigh[,2]>dimen[2]),
     which(neigh[,3]>dimen[3])))
    if(length(rmv)>0) neigh = neigh[-rmv,]

    #convert pattern back to idx and see if it's in mask
    idx = apply(neigh, 1, .convert.3Dto2Dloc, dimen=dimen)
    #see if the index number is in mask
    idx = idx[which(idx %in% mask)]
    #convert idx into a column number
    colidx = which(mask %in% idx)

    #make sure there were neighbors found
    assert_that(length(colidx) == length(idx))
    if(length(colidx)==0) {
      stop(paste0("ERROR AT INDEX ",j," WHERE NO NEIGHBORS FOUND!"))
    }
		
    neighbor.list[[j]] = colidx
    assert_that(max(colidx) <= length(mask))
    assert_that(min(colidx) > 0)

    if(verbose && length(mask)>10 & j %% floor(length(mask)/10)==0) cat('*')
  }

  assert_that(length(neighbor.list)==length(mask))
  list(neighbor.list = neighbor.list, mask = mask)
}


.cube.enumerate <- function(){
  vec = c( 0, 0, 1,   0, 1, 0,  1, 0, 0,
           0, 0,-1,   0,-1, 0, -1, 0, 0,
           0, 1, 1,   1, 0, 1,  1, 1, 0,
           0,-1, 1,  -1, 0, 1, -1, 1, 0,
           0, 1,-1,   1, 0,-1,  1,-1, 0,
           0,-1,-1,  -1, 0,-1, -1,-1, 0,
           1, 1, 1,  -1,-1,-1,
           1,-1,-1,  -1, 1,-1, -1,-1, 1,
           1, 1,-1,   1,-1, 1, -1, 1, 1)

  mat = matrix(vec,ncol=3,byrow=TRUE)
  mat
}

.cross.enumerate <- function(){
  vec = c( 0, 0, 1,   0, 1, 0,  1, 0, 0,
           0, 0,-1,   0,-1, 0, -1, 0, 0)

  mat = matrix(vec,ncol=3,byrow=TRUE)
  mat
}

.construct.graphBase <- function(zero.voxels, edge.mat, n, verbose = TRUE){
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


