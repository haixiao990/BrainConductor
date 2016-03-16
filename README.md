BrainConductor Repository
=============================
Parallel Project to Bioconductor

#TO DO (Top Priority)
- Finalized NIdata file format. 
 -- S3 or S4? Pass by reference or pass by value?
 -- Current idea 5 slots: 1) Phenotype information and Subject/Scan ID, 2) Scan Information (i.e. the
   file typically found in a NIfTI header, 3) Neuroimaging data, 4) Additional notes that can
   be added by the user, 5) Hash ID (to ensure each subject has a unique ID)
 -- We show store information about what transformation/mask was used somewhere... Also, if 
   we convert 4D to 2D matrices, we need to store the voxel's spatial locations somewhere too...
- Hash table that makes sure each NIdata object has a unique ID. Is it by subject or by scan?
- Function to read in Phenotype information from a CSV to the NIdata

#Folder Hiearchy of Repository
##BrainCoSetup
R package for Brainconductor installation

##Brainbase
R package for input/output of NIdata

##Brainstat
R package for the statistical analysis 

##Templates
R package for typical datasets for our Brainconductor package

##Templates-scripts
Collection of R functions used to make the data files for Templates
(These might actually be part of Templates in the future?)

##Paper
Our latex paper and figures

##dicm2nii\_matlab\_func
Matlab function to convert DICOM to NIfTI format

##OO-example
Small example to illustrate pass by value and pass by reference

##Data
Datasets in .nii.gz form that we would like to include in our packets eventually

#Packages Needed
The following are the needed packges throughout all our repositories
 - assertthat, hash, huge, oro.nifti, plyr, R.matlab, utils
