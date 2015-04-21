# utility function to convert alist() construction with <- tagged linear model to regular ~ variety
flist_untag <- function(flist) {
  for ( i in 1:length(flist) ) {
    if ( class(flist[[i]])=="<-" ) {
      # tagged formula, so convert to ~ formula expression
      flist[[i]][[1]] <- as.name("~")
    }
    # eval required to convert from class language to class formula
    flist[[i]] <- eval(flist[[i]])
  }
  as.list(flist)
}