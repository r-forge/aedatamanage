# Function to check for duplicate CEL files
# 
# Author: schmidb
###############################################################################

checkForDuplicates <- function(path, 
		tmpdir=tempdir(), verbose=getOption("verbose"))
{
	require(affy)
	
	# extract arrays
	if(verbose) cat("Extract data\n")
	zipfiles <- list.files(path=path, pattern=".raw.zip", recursive=TRUE, full.names=T)
	info <- list(study_name="checkForDuplicates")
	datadir <- .unzip(zipfiles, info, to=tmpdir)
	
	#run md5 check
	celfiles <- list.celfiles(datadir, full.names=T)
	return( .checkForDuplicatesMD5sum(celfiles) )
}

.checkForDuplicatesMD5sum <- function(celfiles, verbose=getOption("verbose"))
{
	# package for md5sum check
	require(tools)
	
	#calculate md5 sum
	if(verbose) cat("Calculate md5 sums\n")
	md <- md5sum(celfiles)
	
	#Output
	dupl <- which(duplicated(md))
	if(verbose && length(dupl)!=0){
		cat("\t",length(dupl)," Duplicates:\n")
		for(i in dupl)	
			cat("\t",basename(names(md[i])), " = ", basename(names(md[which(md[i]==md)[1]])),"\n")
	}
	
	return(basename(names(md[dupl])))
}