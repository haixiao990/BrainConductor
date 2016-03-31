BCoFSL <- function(func.call, conversion.list = NULL, output.file = NULL){

  if(!is.null(converstion.list){
    #write to directory
    for(i in 1:length(converstion.list)){
      BCoWrite(eval(parse(text = converstion.list[i])), filename.prefix = names(conversion.list)[i])
    }
  }

  #run FSL command
  eval(parse(text = func.call))

  if(!is.null(output.file)){
    #read the output
    BCoRead(output.file)
  }
}


