# Internal-Function to correct AnnotationDataFrame
#
# History
# 16.02.2009 : 
#
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

.correctADF <- function(adf)
{
	require(affy)
	
	# save all FactorValue objects as factor
	objects <- grep("FactorValue", names(pData(adf)) )
	for(i in names(pData(adf))[objects] ){
		if( !is.factor(pData(adf)[[i]]) )
			pData(adf)[[i]] <- as.factor(pData(adf)[[i]])
	}
	
	# save all Characteristics objects as characters
	objects <- grep("Characteristics", names(pData(adf)) )
	for(i in names(pData(adf))[objects] ){
		if( !is.character(pData(adf)[[i]]) )
			pData(adf)[[i]] <- as.character(pData(adf)[[i]])
	}

	return(adf)	
}

