# Functions for nonspecific filtering in the directory structure.
#
# History
# 17.03.2009 : 
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

##############################################################
# Filtering experiments 
###########################
filterExperiments <- function(path=getwd(), ...)
{
	experiments <- dapply(path=path, FUN_filterExperiments, pattern="_eset.Rdata", 
			..., recursive=TRUE)
}

FUN_filterExperiments <- function(esetfile, 
		reprocess=FALSE, ...)
{
	require(affy)
	require(genefilter)
	#load data
	load(esetfile)
	cat("\nFilter", info$study_name, ":\n")
	
	#filter
	filter <- nsFilter(eSet, ...)
	eSet <- filter$eset
	
	cat("\t", sum(unlist(filter$filter.log)), "probes removed.")
	
	# Save
	info$timestamp <- date()
	filename <- esetfile
	save(eSet, info, file=filename)
	return(TRUE)
}
