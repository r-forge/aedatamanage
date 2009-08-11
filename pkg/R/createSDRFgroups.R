# Function to create sdrf file for groups
#
# History
# 17.02.2009 : 
#
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

createSDRFgroups <- function(path=getwd(), ...)
{
	groups <- dapply(FUN_write_AnnotatedDataFrame, path=path, 
			pattern="_eset.Rdata", recursive=FALSE, ...)
}

FUN_write_AnnotatedDataFrame <- function(file, 
		reprocess=FALSE, remove=c('Comment..Derived.ArrayExpress.FTP.file.',
				'Derived.Array.Data.Matrix.File',
				'Comment..Derived.ArrayExpress.Data.Retrieval.URI.',
				'Comment..ArrayExpress.FTP.file.',
				'Comment..ArrayExpress.Data.Retrieval.URI.',
				'Comment..Array.Design.URI.',						
				'Sample.Name',
				'Scan.Name',
				'Protocol.REF',
				'Extract.Name',
				'Material.Type',
				'Term.Source.REF',	
				'LabeledExtract.Name',	
				'Label'))
{
	require(affy)
	
	#Load
	load(file)
	if( !exists("info") )
	{
		info <- list()
		warning("info object missing in data file: ", file)
	}
	if( !exists("eSet") )
	{
		eSet <- new("ExpressionSet")
		warning("eSet object missing in data file: ", file)
	}
	
	cat("\nCreate sdrf.c for", info$study_name, ":\n")
	
	filename <- paste(info$directory, .Platform$file.sep, info$study_name, ".sdrf.c.txt", sep="")
	if( !file.exists(filename) || reprocess==T ){
		#extract adf file
		adf <- pData(eSet)
		
		#remove columns
		if( !is.null(remove) )
			for( i in remove )
				if( any(colnames(adf)==i) ) 
					adf <- adf[,-which(colnames(adf)==i)]
				
		#write file
		write.AnnotatedDataFrame(adf, file=filename)
		return(TRUE)
	} else{
		cat("\tsdrf.c file exists!\n")
		return(NULL)
	}
}

write.AnnotatedDataFrame <- function(adf, file="", 
		sep = "\t", row.names = FALSE, quote=FALSE,
		col.names = TRUE)
{ 
	#add column source name
	#cols <- colnames(adf)
	#adf <- cbind(rownames(adf),adf)
	#colnames(adf) <- c("Source Name", cols)
	write.table(adf, file=file, sep=sep, quote=quote, row.names=row.names, col.names=col.names)
}
	