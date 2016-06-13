#add time slices
#add a way to zoom in and out
.get.hotkeys <- function(){
  hotkeys.num = 12
  hotkeys = numeric(hotkeys.num)
  actions = list(12)
  
  count = 1
  
  hotkeys[count] = "Down"
  actions[[count]] = function(param){param$pos[2] = 
   param$pos[2] - param$multiplier; param}
  count = count + 1
  
  hotkeys[count] = "Up"
  actions[[count]] = function(param){param$pos[2] = 
   param$pos[2] + param$multiplier; param}
  count = count + 1
  
  hotkeys[count] = "Left"
  actions[[count]] = function(param){param$pos[1] = 
   param$pos[1] - param$multiplier; param}
  count = count + 1
  
  hotkeys[count] = "Right"
  actions[[count]] = function(param){param$pos[1] = 
   param$pos[1] + param$multiplier; param}
  count = count + 1
  
  hotkeys[count] = "."
  actions[[count]] = function(param){param$pos[3] = 
   param$pos[3] - param$multiplier; param}
  count = count + 1
  
  hotkeys[count] = "/"
  actions[[count]] = function(param){param$pos[3] = 
   param$pos[3] + param$multiplier; param}
  count = count + 1
  
  hotkeys[count] = "a"
  actions[[count]] = function(param){param$multiplier = 
   param$multiplier + 10; param}
  count = count + 1
  
  hotkeys[count] = "s"
  actions[[count]] = function(param){param$multiplier = 
   max(param$multiplier - 10, 1); param}
  count = count + 1
  
  hotkeys[count] = "="
  actions[[count]] = function(param){param$window.size = 
   param$window.size + 25; param}
  count = count + 1
  
  hotkeys[count] = "-"
  actions[[count]] = function(param){param$window.size = 
   sapply(param$window.size, function(x){max(x-25, 10)}); param}
  count = count + 1
  
  #alert: some strange bug when using this
  hotkeys[count] = "Q"
  actions[[count]] = function(param){break()}
  count = count + 1
  
  list(hotkeys = hotkeys, actions = actions)
} 

.readkeygraph <- function(prompt)
{
  getGraphicsEvent(prompt = prompt, onMouseDown = NULL, onMouseMove = NULL,
    onMouseUp = NULL, onKeybd = onKeybd)
  Sys.sleep(0.01)
  
  keyPressed
}

.onKeybd <- function(key)
{
  if(!(key %in% .get.hotkeys()$hotkeys)) return()
  
  keyPressed <<- key
}

BCoview <- function(obj, window.size = NA, pos = NA){
  assert_that(is.numeric(window.size) | is.na(window.size))
  assert_that(is.na(pos[1]) | ((length(pos)==3 | length(pos)==4) 
   & all(is.numeric(pos))))  

  assert_that(class(obj@data) == "BCoDataFull")
  if(class(obj@data) == "BCoData4D") {
    dat = .convert.2Dto4Dmat(obj@data)
  } else dat = obj@data@mat

  dimen = dim(dat)
  if(length(window.size)==1 & !is.na(window.size)) 
   window.size = rep(window.size, 3)
  
  #temporary
  #WARNING: need a way to move through time as well
  if(length(dim(dat))==4) dat = dat[,,,1]

  #temporary change: for UNIX system
  #WARNING: check this
  X11(type="Xlib")
  #win.graph()
  
  if(is.na(pos[1])) pos = floor(dimen/2)
  
  message("[Use Arrow Keys to move the Point]")
  flush.console()
  
  param = list(pos = pos, window.size = window.size, multiplier = 1)
  
  while(TRUE){
    BCoView.static(dat, location = param$pos, window.size = param$window.size)
    keyPressed = .readkeygraph(paste0("Point Currently at: ",
     param$pos[1], ", ", param$pos[2], ", ", param$pos[3]))
    
    hotkeys = .get.hotkeys()
    idx = which(keyPressed == hotkeys$hotkeys)
    param = hotkeys$actions[[idx]](param)
    
    param$pos = sapply(1:3, function(i){if(param$pos[i]<0) param$pos[i] = 0; 
     if(param$pos[i]>dimen[i]) param$pos[i] = dimen[i]; param$pos[i]})
  }
  
  invisible()
}

