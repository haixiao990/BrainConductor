source("source_header.R")
setwd("/home/smile/klsix/fmri_script_test/20160118_CPACtest/alt+z2")
plot.outlineAll(func.path = "func2mni.nii.gz",
 anat.path = "anat2mni.nii.gz", MNI.path = "MNI152_T1_2mm_brain.nii.gz",
 file.prefix = "50002")


######################

source("source_header.R")
setwd("/home/smile/klsix/fmri_script_test/20151201_test/50003")
plot.outlineAll(func.path = "func2mni.nii.gz", 
 anat.path = "anat2mni.nii.gz", MNI.path = "MNI152_T1_2mm_brain_symmetric.nii.gz",
 file.prefix = "50003") 


######################


setwd("/home/smile/klsix/fmri_script_test/20151201_test/50002")
anat <- readNIfTI("anat2mni.nii.gz")

func <- readNIfTI("func2mni.nii.gz")

dat = readNIfTI("MNI152_T1_2mm_brain_symmetric.nii.gz")
dat = dat@.Data

func.before = readNIfTI("func_before.nii.gz")
func.before = func.before@.Data

setwd("/home/smile/klsix/COBRA.git/BrainConductor.plotter")

anat = anat@.Data
func = func@.Data

meanFunc = compute.meanFunctional(func)

#####
#determine the average value in func that is not 0
## this will be for using a similar analysis on Preprocessed CP
mean(func[func!=0])

#ventricles
#BCview(anat, window.size = 50, pos = c(35, 44, 44))
#BCview(func, window.size = 50, pos = c(35, 44, 44))

plot_anatfunc(anat, meanFunc, location = c(35, 44, 44), 
 filename = "~/DUMP/50002_ventricles_20151225.png", 
 transparency = 0.5, direction.panel = FALSE)

#smooth
BCview(func, window.size = 50, pos = c(35, 44, 44))
BCview(func.before, window.size = 50, pos = c(38, 37, 23))

#motion
plot.motion(func, location = c(35, 44, 44),
 filename = "~/DUMP/50002_motion_20151225.png")

plot.motion(func.before, location = c(35, 44, 20), threshold = 100,
 filename = "~/DUMP/50002_motion_before_20151225.png")

#overlap
plot.overlap(anat, dat, 
 filename = "~/DUMP/50002_anat_to_MNI2mm_sagittal_20151225.png")
plot.overlap(anat, dat, 
 	filename = "~/DUMP/50002_anat_to_MNI2mm_axial_20151225.png", 
 view = "axial")

#compute mean functional
plot.overlap(meanFunc, dat,
 filename = "~/DUMP/50002_meanFunc_to_MNI2mm_sagittal_20151225.png")
plot.overlap(meanFunc, dat,
 filename = "~/DUMP/50002_meanFunc_to_MNI2mm_axial_20151225.png",
 view = "axial")

plot.overlap(meanFunc, anat,
 filename = "~/DUMP/50002_meanFunc_to_anat_sagittal_20151225.png")
plot.overlap(meanFunc, anat,
 filename = "~/DUMP/50002_meanFunc_to_anat_axial_20151225.png",
 view = "axial")

#for reference, plot onto itself
plot.overlap(dat, dat,
 filename = "~/DUMP/MNI2mm_sagittal_20151225.png")
plot.overlap(dat, dat,
 filename = "~/DUMP/MNI2mm_axial_20151225.png",
 view = "axial")

####################################################3
#take a look at the overlap for PCP
func = readNIfTI("/home/smile/klsix/fmri_script_test/Preprocessed_Connectome_outputs/Pitt_0050002_func_preproc.nii.gz")
dat = readNIfTI("/home/smile/klsix/mridata/CPAC_image_resource/symmetric/MNI152_T1_3mm_brain_symmetric.nii.gz")
mask = readNIfTI("/home/smile/klsix/fmri_script_test/Preprocessed_Connectome_outputs/Pitt_0050002_func_mask.nii.gz")
meanFunc = readNIfTI("/home/smile/klsix/fmri_script_test/Preprocessed_Connectome_outputs/Pitt_0050002_func_mean.nii.gz")

func = func@.Data
dat = dat@.Data
mask = mask@.Data
meanFunc = meanFunc@.Data

#WARNING: I'm not sure why i can't compute the meanFunc myself... it fails?
meanFunc[mask==0] = 0
#meanFunc[mask==1] = meanFunc[mask==1]+100

##recenter the voxels inside the brain-mask to have a mean
### of 9000
#for(i in 1:dim(func)[4]){
#  func.slice = func[,,,i]
#  func.slice[mask==1] = func.slice[mask==1]+700
#  func.slice[mask==0] = 0
#  func[,,,i] = func.slice
#}
#
#meanFunc = compute.meanFunctional(func)

plot.overlap(meanFunc, dat,
 filename = "~/DUMP/MNI3mm_PCP_50002_sagittal_20151226.png")

plot.overlap(meanFunc, dat,
 filename = "~/DUMP/MNI3mm_PCP_50002_axial_20151225.png",
 view = "axial")

