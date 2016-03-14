context("Converting from 4D to 2D")

#make a dummy dataset that we'll consider
#construction: it's a 6x6x6x10 dataset. The last
# 10 is for time.
#Take a look at the 5x5x6 box: (called X)
# the elements X[,,1], X[,,5], X[,,6],
#              X[1,,], X[6,,],
#              X[,1,], X[,6,] are all 0 (empty)
# the elements X[2:3, 2:3, 2:4] are drawn from one distribution
# the elements X[4:5, 4:5, 2:4] are drawn from another
# the elements X[2:3, 4:5, 2:4] another, and X[4:5, 2:3, 2:4]

set.seed(10)
X = array(0, dim = c(6, 6, 6, 10))

#fill in the 4-dimension array X with vec so that 
#  every row along the 4th dimension are correlated
fill.in <- function(dim1, dim2, dim3, vec, noise = 0.05){
  X[dim1, dim2, dim3, ] <<- aperm(apply(X[dim1, dim2, dim3, ,drop=FALSE], 
   c(1:3), function(x){ vec + noise*rnorm(10)}), c(2,3,4,1))
}
#make sure the matrix is adding the way I think it is
vec = rnorm(10)
fill.in(2:3, 2:3, 2:4, vec, 0)
assert_that(all(X[2,2,2,] == X[3,3,4,]))
assert_that(all(X[2,2,2,] != 0))

#now fill in the actual matrix
vec = rnorm(10); fill.in(2:3, 2:3, 2:4, vec)
vec = rnorm(10); fill.in(4:5, 4:5, 2:4, vec)
vec = rnorm(10); fill.in(2:3, 4:5, 2:4, vec)
vec = rnorm(10); fill.in(4:5, 2:3, 2:4, vec)

test_that("Conversion of 4D to 2D works both ways (without template)", {
  res = .convert.4Dto2Dmat(X)

  expect_true(ncol(res$mat) == length(which(X[,,,1] != 0)))
  expect_true(ncol(res$mat) == length(res$mask))
  expect_true(nrow(res$mat) == dim(X)[4])

  X2 = .convert.2Dto4Dmat(res$mat, res$mask, dim(X)[1:3])

  expect_true(sum(abs(X-X2)) < 1e-10)
})


test_that("Conversion works both ways", {
  set.seed(10)
  box = array(0, dim=c(5,5,5))
  idx = sample(1:(5^3),20)
  box[idx] = 1
  dimen = dim(box)

  mask = which(box!=0)
  expect_true(all(sort(idx)==sort(mask)))

  for(i in 1:length(mask)){
    loc = .convert.2Dto3Dloc(mask[i], dimen)
    expect_true(box[loc[1], loc[2], loc[3]]==1)
  }

  for(i in 1:dimen[1]){
    for(j in 1:dimen[2]){
      for(k in 1:dimen[3]){
        if(box[i,j,k]==1){
          idx = .convert.3Dto2Dloc(c(i,j,k), dimen)
          expect_true(idx %in% mask)
        } 
      }
    }
  }
})

