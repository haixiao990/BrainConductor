library(huge)
library(mvtnorm)
library(pcaPP)
library(camel)
library(tawny)
library(doSNOW)
library(foreach)
library(parallel)

source('thres.R')
source('oraSel.R')

SMG <- function(n=50,d=50,graph="hub",T=3,n.sim=100,p.add=0.01, p.rmv=.05, n.lam=16, n.out = 0, method = 'glasso'){
  
  # Minimum lambda ratio for CLIME (found by experimental trial and error)
  min.ratio = 10/d
  
  # Names of methods correlations
  NamesCor = c("SMGKen","SMGPea", "NaiveKen.","NaivePea","AvgKen","AvgPea", "SMGLW", "AvgLW", "NaiveLW")
  
  # Generates data and corresponding precision
  L=huge.generator(n = n, d = d, graph = graph)
  
  # Get the precision structure associated with true median graph
  G_L=L$theta;
  
  # G for testing
  G = G_L; G[lower.tri(G)]=0; diag(G)=0;
  
  # Find the Hamming distance of the generated graph from 0
  # Known number of edges. Oracle information
  spa = sum(G)
  
  # Number of potential edges
  upS = d * (d-1) / 2; # Upperbound for sparscity
  
  # sparscity parameters for s
  testno=floor(c(seq(1,upS, length.out=100)))
  
  # Array false positive and false negatives
  FPR=FNR=array(0,dim=c(n.sim,length(testno),length(NamesCor)))
  
  # Run n.sim simulations
  for(sim in 1:n.sim){
    
    # Parallel stuff
    cl <- makeCluster(min(detectCores(),T))
    registerDoSNOW(cl)
    
    # For each patient
    results = foreach(i = 1:T, .packages=c('huge','mvtnorm','pcaPP','camel','tawny'))%dopar% {
      
      source('thres.R')
      source('perturbEdge.R')
      source('genOutlier.R')
      
      
      if(i <= n.out){
        perturb = genOutlier(G_L, p.rmv = .90)
      } else {
        # Add and remove edges
        perturb = perturbEdge(G_L, p.add = p.add, p.rmv = p.rmv)
      }
      
      Gt = perturb$Gt
      sigma = perturb$sigma
      
      truSpa = sum(Gt)
      
      mu=rep(0,d) # Mean is 0 for each dimension
      
      if(i <= n.out){
        z = rmvnorm(n*3,mu,as.matrix(sigma)) # Generate gaussian data for transform
      } else {
        z = rmvnorm(n,mu,as.matrix(sigma)) # Generate gaussian data for transform
      }     
      
      
      # Data observations are cube of the gaussian (to make nonparanormal)
      x = z^5
      
      # Calculate Kendall's tau matrix R (in the paper)
      ken=sin(pi/2*cor.fk(x))
      # Calculate Pearson correlation matrix
      pea=cor(x) 
      
      ################################ Ledoit Wolf Methods #####################################
      sig = cov.sample(x)
      lw = cov.shrink(sig)
      lw = solve(lw)
      
      # Set diagonal and lower triangle to be 0
      # Set diagonal and lower triangle to be 0
      diag(lw) = 0;lw[lower.tri(lw)]=0;
      
      # Estimate graph using lw
      thresLW = 0.001 * mean(abs(lw))
      G7_T = lw;
      ind7 = which(as.matrix(abs(lw)) > thresLW)
      G7_T[ind7] = 1
      G7_T[-ind7] = 0
      
      # Add onto edge count graph
      G7 = G7_T
      cat(i)
      
      result = list(G7_T=G7, x=x, ken=ken, pea = pea, lw=lw)
      
      return(result)
    }
    stopCluster(cl)
    
    cat('Patients Generated \n')
    
    # Aggregate Results from Parallelization
    cumKen = Reduce("+",lapply(results,"[[","ken"))
    cumPea = Reduce("+",lapply(results,"[[","pea"))
    cumLw  = Reduce("+",lapply(results,"[[","lw" ))
    G7     = Reduce("+",lapply(results,"[[","G7_T" ))
    wx     = Reduce(rbind,lapply(results,"[[","x" ))
    xList  = lapply(results,"[[","x" )
    
    # For each patient, find G1 and G2
    G1 = G2 = 0
    
    # Parallel stuff
    cl <- makeCluster(detectCores())
    registerDoSNOW(cl)
    cat('Patients: \n')
    for(i in 1:T){
      x = xList[[i]]
      
      ken = sin(pi/2*cor.fk(x))
      pea = cor(x)
      
      if(method == 'glasso'){
        CV1 = oraSel(n.lam = detectCores(), lambda.min.ratio=min.ratio, dat=x, cor = 'ken', method='glasso', ora = G)
        CV2 = oraSel(n.lam = detectCores(), lambda.min.ratio=min.ratio, dat=x, cor = 'pea', method='glasso', ora = G)
        
        G1=G1 + huge(ken,method="glasso",lambda=CV1$optLam,verbose=F)$path[[1]]
        G2=G2 + huge(pea, method='glasso', lambda=CV2$optLam, verbose=F)$path[[1]]

      } else if(method == 'clime'){
        CV1 = oraSel(n.lam = detectCores(), lambda.min.ratio=min.ratio, dat=x, cor = 'ken', method='clime', ora= G, S=ken)
        CV2 = oraSel(n.lam = detectCores(), lambda.min.ratio=min.ratio, dat=x, cor = 'pea', method='clime', ora= G, S=pea)
        
        G1=G1 + camel.tiger(ken,method="clime",lambda=CV1$optLam,verbose=F)$path[[1]]
        G2=G2 + camel.tiger(pea, method='clime', lambda=CV2$optLam, verbose=F)$path[[1]]
        
      } else{
        stop('Method must be clime or glasso')
      }
      
      cat(i,'\n')
    }
    
    
    #### MEDIAN GRAPH METHODS ####
    # For the make the lower triangle and diagonal 0
    G1[lower.tri(G1)]=0; G2[lower.tri(G2)]=0; G7[lower.tri(G7)]=0; 
    diag(G1)=diag(G2)=diag(G7)=0;
    # HF: G1 and G2 are cumulative graph estimates based on kendall or pearson  
    
    #### Naive Leboit Wolf ####
    sigAll = cov.sample(wx)
    lwAll = cov.shrink(sigAll) 
    lwAll = solve(lwAll)
    # Set diagonal and lower triangle to be 0
    diag(lwAll) = 0;lwAll[lower.tri(lwAll)]=0;
    
    
    ####  AVERAGE CORREL. METHODS ####
    # Get the average kendall and pearson across patients
    avgKen = cumKen / T
    avgPea = cumPea / T
    avgLw  = cumLw / T
    
    # Set diagonal and lower triangle to be 0
    diag(avgKen) = diag(avgPea) = diag(avgLw) = 0;
    avgKen[lower.tri(avgKen)]=0; avgPea[lower.tri(avgPea)]=0; avgLw[lower.tri(avgLw)]=0;
    
    #### NAIVE GRAPH METHODS ####
    # Fit the naive pearson and Kendall graphs
    #browser()
    # Print to output file
    cat(paste('\t\t\t\t\t\t\tNV  Started \n',sep=''))
    
    wKen = sin(pi/2*cor.fk(wx))
    wPea = cor(wx)
    
    if(method=='glasso'){
      CV3 = oraSel(n.lam = detectCores(), lambda.min.ratio=min.ratio, dat=wx, cor = 'ken', method='glasso', ora = G)
      CV4 = oraSel(n.lam = detectCores(), lambda.min.ratio=min.ratio, dat=wx, cor = 'pea', method='glasso', ora = G)
      
      
      G3=huge(wKen,method="glasso",lambda=CV3$optLam,verbose=F)$path[[1]]
      G4=huge(wPea, method='glasso', lambda=CV4$optLam, verbose=F)$path[[1]]

    } else if(method=='clime'){
      CV3 = oraSel(n.lam = detectCores(), lambda.min.ratio=min.ratio, dat=wx, cor = 'ken', method='clime', ora = G, S = wKen)
      CV4 = oraSel(n.lam = detectCores(), lambda.min.ratio=min.ratio, dat=wx, cor = 'pea', method='clime', ora = G, S = wPea)
      
      
      G3=camel.tiger(wKen, method="clime",lambda=CV3$optLam,verbose=F)$path[[1]]
      G4=camel.tiger(wPea, method='clime', lambda=CV4$optLam, verbose=F)$path[[1]]
    }
    
    G1 = as.matrix(G1)
    G2 = as.matrix(G2)
    G3 = as.matrix(G3)
    G4 = as.matrix(G4)
    G  = as.matrix(G)
    
    # Print to output file
    cat(paste('\t\t\t\t\t\t\tFP/FN  Started \n',sep=''))
    
    ############################# s Parameters ###############################################
    # Find Sparse Median graph for each s
    # HF: s is a tuning parameter representing the number of edges 
    # For each value of s
    F_i <- foreach(i = 1:length(testno)) %dopar% {
      source('thres.R')
      
      cutpoint=testno[i] # s
      
      FP_i = numeric(9);
      FN_i = numeric(9);
      
      # Find sparse median graph corresponding to s; Find the false positive and false negatives compared to the true graph
       G1t=thres(G1,cutpoint); FP_i[1]=length(which((G1t-G)>0)); FN_i[1]=length(which((G1t-G)<0))   
      G2t=thres(G2,cutpoint); FP_i[2]=length(which((G2t-G)>0)); FN_i[2]=length(which((G2t-G)<0))          
      G3t=thres(G3, cutpoint); FP_i[3]=length(which((G3t-G)>0)); FN_i[3]=length(which((G3t-G)<0))
      G4t=thres(G4, cutpoint); FP_i[4]=length(which((G4t-G)>0)); FN_i[4]=length(which((G4t-G)<0))
      G5t=thres(avgKen, cutpoint); FP_i[5]=length(which((G5t-G)>0)); FN_i[5]=length(which((G5t-G)<0))   
      G6t=thres(avgPea, cutpoint); FP_i[6]=length(which((G6t-G)>0)); FN_i[6]=length(which((G6t-G)<0))
      G7t=thres(G7,cutpoint); FP_i[7]=length(which((G7t-G)>0)); FN_i[7]=length(which((G7t-G)<0))
      G8t=thres(avgLw, cutpoint); FP_i[8]=length(which((G8t-G)>0)); FN_i[8]=length(which((G8t-G)<0))
      G9t=thres(lwAll, cutpoint); FP_i[9]=length(which((G9t-G)>0)); FN_i[9]=length(which((G9t-G)<0))
      return(list(FP_i = FP_i, FN_i = FN_i))
    }

    # Aggregate Results from Parallelization
    FP = Reduce("rbind",lapply(F_i,"[[","FP_i"))
    FN = Reduce("rbind",lapply(F_i,"[[","FN_i"))
    
    stopCluster(cl)
    
    # Print to output file
    cat(paste('\t\t\t\t\t\t\tFP/FN  Complete \n',sep=''))
    
    ##########################################################################################
    # Calculate the false positive and false negative rates for this simulation
    FPR[sim,,]=FP/(d*(d-1)/2-spa)
    FNR[sim,,]=FN/spa 
    cat('Sim: ', sim, 'Complete\n')
  }
  
  # Sort rates in ascending order and plot results
  a11=sort(apply(FPR[,,1],2,mean));a12=sort(1-apply(FNR[,,1],2,mean));a11=c(0,a11,1);a12=c(0,a12,max(a12))
  a21=sort(apply(FPR[,,2],2,mean));a22=sort(1-apply(FNR[,,2],2,mean));a21=c(0,a21,1);a22=c(0,a22,max(a22))
  a31=sort(apply(FPR[,,3],2,mean));a32=sort(1-apply(FNR[,,3],2,mean));a31=c(0,a31,1);a32=c(0,a32,max(a32))
  a41=sort(apply(FPR[,,4],2,mean));a42=sort(1-apply(FNR[,,4],2,mean));a41=c(0,a41,1);a42=c(0,a42,max(a42))
  a51=sort(apply(FPR[,,5],2,mean));a52=sort(1-apply(FNR[,,5],2,mean));a51=c(0,a51,1);a52=c(0,a52,max(a52))
  a61=sort(apply(FPR[,,6],2,mean));a62=sort(1-apply(FNR[,,6],2,mean));a61=c(0,a61,1);a62=c(0,a62,max(a62))
  a71=sort(apply(FPR[,,7],2,mean));a72=sort(1-apply(FNR[,,7],2,mean));a71=c(0,a71,1);a72=c(0,a72,max(a72))
  a81=sort(apply(FPR[,,8],2,mean));a82=sort(1-apply(FNR[,,8],2,mean));a81=c(0,a81,1);a82=c(0,a82,max(a82))
  a91=sort(apply(FPR[,,9],2,mean));a92=sort(1-apply(FNR[,,9],2,mean));a91=c(0,a91,1);a92=c(0,a92,max(a92))
  
  tmp1 = a11
  plot(a12~tmp1,type='l',lwd=3, xlab='FPR', ylab= 'TPR', main=graph,
       xlim=c(0,1), ylim=c(0,1))
  tmp2 = a21
  lines(tmp2, a22,type="l",lwd=2,lty=2, col=2)
  tmp3 = a31
  lines(tmp3, a32,type="l",lwd=2,lty=3, col=3)
  tmp4 = a41
  lines(tmp4, a42,type="l",lwd=2,lty=4, col=5)
  tmp5 = a51
  lines(tmp5, a52,type="l",lwd=2,lty=5, col=6)
  tmp6 = a61
  lines(tmp6, a62,type="l",lwd=2,lty=6, col=2)
  tmp7 = a71
  lines(tmp7, a72,type="l",lwd=2,lty=7, col=3)
  tmp8 = a81
  lines(tmp8, a82,type="l",lwd=2,lty=8, col=5)
  tmp9 = a91
  lines(tmp9, a92,type="l",lwd=2,lty=10, col=6)
  
  filename = paste('sim_s',n.sim,'_n',n,'_d',d,'_T', T, '_a',p.add, '_r',p.rmv,'_o', n.out,'_', graph, '.RData',sep='')
  
  save(list=c('a11', 'a21', 'a31', 'a41', 'a51', 'a61', 'a71', 'a81', 'a91',
              'a12', 'a22', 'a32', 'a42', 'a52', 'a62', 'a72', 'a82', 'a92'), file = filename)
}