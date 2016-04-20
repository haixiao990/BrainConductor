nam = read.csv("slotnames.csv")
nam = levels(nam[,1])
nam = nam[-1]

x = ""
conn = file("nifti2nidata.txt")
for(i in 1:length(nam)){
  #x = paste0(x, "  scaninfo@", nam[i], " = dat@", nam[i], "\n")
  x = paste0(x, "  dat@", nam[i], " = scaninfo@", nam[i], "\n")
}  

writeLines(x, conn)
close(conn)
