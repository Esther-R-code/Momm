PSE_U<- function(PSElist, catecount, w_c, pathcount, dor){
  if(dor=="D"){PSEu<- PSElist[[1]][pathcount,paste0("Outcome=",w_c[catecount])]
  }else if(dor=="R"){PSEu<- PSElist[[2]][pathcount,paste0("Outcome=",w_c[catecount])]}
  else{cat("The parameter 'dor' must be D or R, for idenfity which scale be used.")}
  return(PSEu)
}