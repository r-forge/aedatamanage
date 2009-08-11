# Functions for probe - gen connection 
#
# History
# 17.03.2009 : 
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################
#
#TODO überarbeiten!
#
###### probeset -> gen ######### 
#get.genes <- function(IDs=c(""))
#{
#	require(hgu133a.db)
#	
#	result <- 1:length(IDs)
#	xx <- as.list(hgu133aSYMBOL)
#   	for(i in 1:length(IDs)){
#		if(is.na(xx[[IDs[i]]])==TRUE)
#			result[i] <- IDs[i]
#    	else
#			result[i] <- xx[[IDs[i]]]
#    } 
#	return(result)
#} 
#
######## gen -> probeset ########
#get.probesets <- function(name="")
#{
#	require(hgu133a.db)
#	
#	xx <- as.list(hgu133aSYMBOL)
#	result <- NULL
#	
#	for(i in name){
#		for( ding in seq(1,length(xx))){
#			if(as.character(xx[ding])==i)
#				result <- c(result,xx[ding])
#		}
#	}
#	return(result)
#} 
