files = dir("~/Brainconductor.git/Brainbase/R", full.names = T)

setwd("~/Brainconductor.git/R-examples")
package.skeleton(name = "Brainbase", code_files = files)
