# Functions to correct (Batch Effect) data in the directory structure.
#
# History
# 17.02.2009 : 
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

##############################################################
# Correct for Batch-Effect
###########################
correctBatch <- function(path=getwd(), ...)
{
	batch <- dapply(FUN_correctBatch, path=path, 
			pattern="_eset.Rdata", recursive=FALSE, 
			...)
}

FUN_correctBatch <- function(file, reprocess=FALSE, saveold=FALSE)
{
	require(affy)
	
	# Load and prepare data
	load(file)
	cat("\nCorrect", info$study_name, ":\n")
	
	if( !file.exists(sub("_eset.Rdata" ,"_esetNoBatchCorrect.Rdata",file)) || reprocess==T ){		
		position <- regexpr("-raw-cel-", pData(eSet)$Array.Data.File) 
		groups <- substr(pData(eSet)$Array.Data.File, 1, position-1)
		sample_info <- cbind(pData(eSet)$Array.Data.File, pData(eSet)$Sample.Name, groups )
		colnames(sample_info) <- c("array", "sample", "Batch")
		
		# Correct batch-effect with ComBat
		exprs(eSet) <- ComBat( exprs(eSet), sample_info )
		
		# Save old file
		if(saveold){
			file_old <- sub("_eset.Rdata" ,"_esetNoBatchCorrect.Rdata",file)
			file.copy(file, file_old)
		}
		
		# Save
		info$timestamp <- date()
		save(eSet, info, file=file)
		return(TRUE)
	} else{
		cat("\tBatch correct data exist!\n")
		return(NULL)
	}	
}
