# Quality assessment for compelte data together
#
# History
# 20.04.2009 : 
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

qaComplete <- function(path=getwd(), 
		qaFUN=boxplot, inputQaFun=c("affybatch", "listCELfiles"),
		tmpdir=tempdir(),
		reprocess=FALSE, file_add="_qa.Rdata", 
		...)
{
	# Load
	require(affy)
	# get info
	info <- .info(path)
	info$zipfile_name <- info$study_name <- "complete"
	info$directory <- path
	info$group_name <- ""
	cat("\nQA", info$study_name, ":\n")	
	
	if( !file.exists(paste(info$directory, .Platform$file.sep, info$study_name, file_add, sep="")) || reprocess==T ){		
		# Extract zip files
		zipfiles <- list.files(path=path, pattern=".raw.zip", recursive=TRUE, full.names=T)
		datadir <- .unzip(zipfiles, info, to=tmpdir)
		
		# Create annotatet data frames
		adf <- adf_create( list.files(path=path, pattern=".sdrf.txt", recursive=TRUE, full.names=T) )
				
		# Quality Asessment
		bad_arrays <- .qa(datadir, adf, qaFUN=qaFUN, inputQaFun=inputQaFun)
		
		# Clean
		cat("\tRemove extracted data on master\n")
		system(paste("rm -R ", datadir,"/* > /dev/null", sep="" ))
		
		# Save
		info$timestamp <- date()
		filename <- paste( info$study_name, file_add, sep="")
		save(bad_arrays, info, file=paste(info$directory, filename, sep=.Platform$file.sep))
		return(bad_arrays)
	} else{
		cat("\tQA data exist!\n")
		return(NULL)
	}
}


##########################################################
# Function for qa
##############################
.qa <- function(datadir, adf, 
		qaFUN=boxplot, inputQaFun=c("affybatch", "listCELfiles"),  
		arraydesign="A-AFFY-33", ...)
{
	cat("\tQA\n")
	
	inputQaFun <- match.arg(inputQaFun)
	
	# read celfiles
	celfiles <- list.celfiles(datadir, full.names=T)
	if(!all(basename(celfiles) %in% adf$Array.Data.File)){
		celfiles <- celfiles; adf <- adf
		stop("Wrong CEL files in any Experiment!\n", 
				basename(celfiles)[ which(!(basename(celfiles) %in% adf$Array.Data.File))] )
	}
	
	# remove wrong arrays, only use A-AFFY-33 data
	wrong_array_design <- which(adf$Array.Design.REF!=arraydesign)
	if(length(wrong_array_design)!=0){
		unlink(paste(datadir,adf$Array.Data.File[wrong_array_design],sep="/"))
		adf <- adf[-wrong_array_design,]
	}
	cat("\t",length(wrong_array_design)," Arrays of wrong Chip Types deleted.\n")
	
	# read celfiles
	celfiles <- list.celfiles(datadir, full.names=T)
	if(!all(basename(celfiles) %in% adf$Array.Data.File)){
		celfiles <<- celfiles; adf <<- adf
		stop("Wrong CEL files after corrections!",
				basename(celfiles)[ which(!(basename(celfiles) %in% adf$Array.Data.File))] )
	}
	
	if( inputQaFun=="affybatch"){
		# create affyBatch
		ab <- ReadAffy(filenames=celfiles)
		# run function
		bad_arrays <- qaFUN(ab, ...)
	} else {
		# run function
		bad_arrays <- qaFUN(celfiles, ...)		
	}
	
	return(bad_arrays)
}


#######################################################
# Exampe QA fun
qaFUNaffyPara <- function(celfiles, ...)
{
	require(affyPara)
	#distribute files
	cat('\t\tDistribute Data\n')
	celfiles <- distributeFiles(celfiles, to =  sub("tmp", "usr1/tmp", tempdir()),
			protocol = "RCP", hierarchicallyDist = FALSE, 
			master=FALSE, delExistTo=FALSE,
			full.names=TRUE)
	# run function
	cat('\t\tBoxplot QA\n')
	bp <- boxplotPara(celfiles$CELfiles, plot=FALSE)
	cat('\t\tMAplot QA\n')
	map <- MAplotPara(celfiles$CELfiles, level=2, plot=FALSE)
	boxMAParaSummary <- affyPara:::summaryM1M2Para(bp, map, level=3)
	
	# remove files
	cat('\t\tRemove distributed Data\n')
	removeDistributedFiles(celfiles$to, master=TRUE)
	
	return( rownames(boxMAParaSummary)[rowSums(boxMAParaSummary) > 2] )
}



