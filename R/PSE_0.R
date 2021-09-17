#'This function will separate out the PSE of the orginal data from the output of function 'Ord_mediation_analysis_pal'.
#'This subfunction is used in sensitivityanalysis.R
#'@param tiile The PSE for the orginal data
#'@param PSElist the output of function 'Ord_mediation_analysis_pal' with orginal data
#'@param catecount for counting the categories of outcome that user have interesting in
#'@param w_c category of Y that user have instercting in
#'@param pathcount fot counting the pathway
#'@param dor must be "D" or "R", for idenfity "difference" or "ratio" scale to be used
#'@export PSE_0

PSE_0<- function(PSElist, catecount, w_c, pathcount, dor){
  if(dor=="D"){PSEo<- PSElist[8+(pathcount-1)*8,paste0("Outcome=",w_c[catecount])]
  }else if(dor=="R"){PSEo<- PSElist[10+(pathcount-1)*8,paste0("Outcome=",w_c[catecount])]}
  else{cat("The parameter 'dor' must be D or R, for idenfity which scale be used.")}
  return(PSEo)
}
