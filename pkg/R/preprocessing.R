# Functions to preprocess data in the directory structure.
#
# History
# 16.02.2009 : 
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

##############################################################
# Preprocess single studies 
###########################
preprocessExperiments <- function(path=getwd(), 
		preprocessFUN=rma, inputPreprocessFun=c("affybatch", "listCELfiles"), 
		tmpdir=tempdir(), reprocess=FALSE, ...)
{
	experiments <- dapply(path=path, .FUN_preprocessExperiments, 
			preprocessFUN=preprocessFUN, inputPreprocessFun=inputPreprocessFun, 
			tmpdir=tmpdir, reprocess=reprocess, ..., 
			pattern=".raw.zip", recursive=TRUE)
}

.FUN_preprocessExperiments <- function(zipfile, 
		preprocessFUN=rma, inputPreprocessFun=c("affybatch", "listCELfiles"),
		tmpdir=tempdir(),
		reprocess=FALSE, file_add="_eset.Rdata", 
		...)
{
	require(affy)
	# get info
	info <- .info(zipfile)
	cat("\nProcess", info$study_name, ":\n")

	if( !file.exists(gsub(".raw.zip", file_add, zipfile)) || reprocess==T ){		
		# Extract zip files
		datadir <- .unzip(zipfile, info, to=tmpdir)

		# Create annotatet data frames
		adf <- adf_create( gsub(".raw.zip", ".sdrf.txt", zipfile) )
		
		# Preprocess
		eSet <- .preprocess(datadir, adf, 
				preprocessFUN=preprocessFUN, inputPreprocessFun=inputPreprocessFun, ...)
		
		# Clean
		cat("\tRemove extracted data\n")
		system(paste("rm -R ", datadir," > /dev/null", sep="" ))
		
		# Save
		info$timestamp <- date()
		filename <- gsub(".raw.zip", file_add, zipfile)
		save(eSet, info, file=filename)
		return(TRUE)
	} else{
		cat("\tPreprocessed data exist!\n")
		return(NULL)
	}
}


##############################################################
# Preprocess groups 
###########################
preprocessGroups <- function(path=getwd(), 
		preprocessFUN=rma, inputPreprocessFun=c("affybatch", "listCELfiles"),
		tmpdir=tempdir(),
		complete=TRUE, reprocess=FALSE,
		...)
{
	groups <- dapply(path=path, .FUN_preprocessGroups, 
			preprocessFUN=preprocessFUN, inputPreprocessFun= inputPreprocessFun,
			tmpdir=tmpdir,
			reprocess=reprocess, ..., recursive=FALSE)
	if(complete)
		complete <- preprocessComplete(path, preprocessFUN=preprocessFUN, 
				tmpdir=tmpdir,
				inputPreprocessFun=inputPreprocessFun, reprocess=reprocess, ...)
}

.FUN_preprocessGroups <- function(path, 
		preprocessFUN=rma, inputPreprocessFun=c("affybatch", "listCELfiles"), 
		tmpdir=tempdir(),
		file_add="_eset.Rdata",	reprocess=FALSE,
		...)
{
	if( file.info(path)$isdir ){
		
		require(affy)
		# get info
		info <- .info(path)
		info$group_name <- ""
		cat("\nProcess", info$study_name, ":\n")
		
		if( !file.exists(paste(info$directory, .Platform$file.sep, info$study_name, file_add, sep="")) || reprocess==T ){
			# Extract zip files
			zipfiles <- list.files(path=path, pattern=".raw.zip", recursive=FALSE, full.names=T)
			if(length(zipfiles)!=0) #check for empty group
			{
				datadir <- .unzip(zipfiles, info, to=tmpdir)
			
				# Create annotatet data frames
				adf <- adf_create( list.files(path=path, pattern='.sdrf.txt', recursive=FALSE, full.names=T) )

				# Preprocess
				eSet <- .preprocess(datadir, adf, 
						preprocessFUN=preprocessFUN, inputPreprocessFun=inputPreprocessFun, ...)
				
				# Clean
				cat("\tRemove extracted data\n")
				system(paste("rm -R ", datadir," > /dev/null", sep="" ))
		
				# Save
				info$timestamp <- date()
				filename <- paste( info$study_name, file_add, sep="")
				save(eSet, info, file=paste(info$directory, filename, sep=.Platform$file.sep))
				return(TRUE)
			} else{
				cat("\tNo experiments in group!\n")
				return(NULL)
			}
		} else{
			cat("\tPreprocessed data exist!\n")
			return(NULL)
		}
	}
}

##############################################################
# Preprocess compelte data together
##############################
preprocessComplete <- function(path=getwd(), 
		preprocessFUN=rma, inputPreprocessFun=c("affybatch", "listCELfiles"),
		tmpdir=tempdir(),
		reprocess=FALSE, file_add="_eset.Rdata", 
		...)
{
	# Load
	require(affy)
	# get info
	info <- .info(path)
	info$zipfile_name <- info$study_name <- "complete"
	info$directory <- path
	info$group_name <- ""
	cat("\nProcess", info$study_name, ":\n")	
	
	if( !file.exists(paste(info$directory, .Platform$file.sep, info$study_name, file_add, sep="")) || reprocess==T ){		
		# Extract zip files
		zipfiles <- list.files(path=path, pattern=".raw.zip", recursive=TRUE, full.names=T)
		datadir <- .unzip(zipfiles, info, to=tmpdir)
		
		# Create annotatet data frames
		adf <- adf_create( list.files(path=path, pattern=".sdrf.txt", recursive=TRUE, full.names=T) )
		
		# Preprocess
		eSet <- .preprocess(datadir, adf, 
				preprocessFUN=preprocessFUN, inputPreprocessFun=inputPreprocessFun, ...)
				
		# Clean
		cat("\tRemove extracted data\n")
		system(paste("rm -R ", datadir," > /dev/null", sep="" ))
		
		# Save
		info$timestamp <- date()
		filename <- paste( info$study_name, file_add, sep="")
		save(eSet, info, file=paste(info$directory, filename, sep=.Platform$file.sep))
		return(TRUE)
	} else{
		cat("\tPreprocessed data exist!\n")
		return(NULL)
	}	
}

##################################################################################
##################################################################################
# Internal function
##################################################################################

##########################################
# Function to read infos about experiment
##########################################
.info <- function(zipfile)
{
	info <- list()
	info$zipfile_name <- basename(zipfile)
	info$study_name <- sub(".raw.zip", "", info$zipfile_name)
	info$directory <- dirname(zipfile)
	info$group_name <- basename(info$directory)
	return(info)
}

##########################################
# Function to extract zip files
##########################################
# Extract zip files
.unzip <- function(zipfiles, info, to=tempdir())
{
	require(ArrayExpress)
	require(multicore)
	cat("\tExtract: ")
	datadir <- paste(to, info$study_name, sep=.Platform$file.sep)
	dir.create(datadir, recursive=TRUE)
	
	lapply(zipfiles, function(i, datadir){
				cat(".")
				#ArrayExpress:::extract.zip(i, extract_path=datadir)
				unzip(i, exdir=datadir)
			}, datadir)

	cat("\n")
	return(datadir)
}

##############################
# Function to create adf object
##############################
adf_create <- function(sdrffiles, verbose=getOption("verbose"))
{
	if(verbose) cat("\tCreate AnnotatedDataFrame: ")
	adf <- new("AnnotatedDataFrame")
	for( i in sdrffiles){
		if(verbose) cat(".")
		#read annotation file
		sdrf2file <- sub(".sdrf.txt", ".sdrf2.txt", i)
		if(file.exists(sdrf2file))
			adf_tmp <- read.AnnotatedDataFrame.AE(sdrf2file)
		else
			adf_tmp <- read.AnnotatedDataFrame.AE(i)
		#add information about entity and experiment
		sdrffile_name <- basename(i)
		study_name <- sub(".sdrf.txt", "", sdrffile_name)
		directory <- dirname(i)
		group_name <- basename(directory)
		adf_tmp$FactorValue..experiment. <- study_name
		adf_tmp$FactorValue..entity. <- group_name
		#correct ADF 
		adf_tmp <- .correctADF(adf_tmp)
		#combine adf 
		test <- try( adf <- combine(adf, adf_tmp) )
		if (inherits(test, "try-error"))
			stop("Error with ",group_name, ":", study_name,"!\n")
	}
	cat("\n")
	return(adf)
}

##############################
# Function to preprocess data
##############################
.preprocess <- function(datadir, adf, 
		preprocessFUN=preprocessFUN, inputPreprocessFun=c("affybatch", "listCELfiles"),  
		arraydesign="A-AFFY-33", bad_arrays=NULL, ...)
{
	cat("\tPreprocess\n")
	
	inputPreprocessFun <- match.arg(inputPreprocessFun)
	
	# read celfiles
	celfiles <- list.celfiles(datadir, full.names=T)
	if(!all(basename(celfiles) %in% adf$Array.Data.File)){
		celfiles <<- celfiles; adf <<- adf
		stop("Wrong CEL files in any Experiment!\n", 
				basename(celfiles)[ which(!(basename(celfiles) %in% adf$Array.Data.File))] )
	}
	
	# remove bad arrays
	id <-  which( adf$Array.Data.File %in% bad_arrays)
	if(length(id) != 0){
		unlink(paste(datadir,bad_arrays,sep="/"))
		adf <- adf[-id,]
		cat("\t",length(id)," Arrays of bad Quality deleted.\n")
	}
		
	
	# remove wrong arrays, only use A-AFFY-33 data
	wrong_array_design <- which(adf$Array.Design.REF!=arraydesign)
	if(length(wrong_array_design)!=0){
		unlink(paste(datadir,adf$Array.Data.File[wrong_array_design],sep="/"))
		adf <- adf[-wrong_array_design,]
		cat("\t",length(wrong_array_design)," Arrays of wrong Chip Types deleted.\n")
	}
	
	# Remove Duplicates
	dupl <- .checkForDuplicatesMD5sum(list.celfiles(datadir, full.names=T))
	if(length(dupl) !=0){
		unlink(paste(datadir,dupl,sep="/"))
		adf <- adf[-which(adf$Array.Data.File %in% dupl ),]
		cat("\t",length(dupl)," duplicated Arrays deleted.\n")
	}
	
	# read celfiles
	celfiles <- list.celfiles(datadir, full.names=T)
	if(!all(basename(celfiles) %in% adf$Array.Data.File)){
		celfiles <<- celfiles; adf <<- adf
		stop("Wrong CEL files after corrections!",
				basename(celfiles)[ which(!(basename(celfiles) %in% adf$Array.Data.File))] )
	}
	
	if( inputPreprocessFun=="affybatch"){
		# create affyBatch
		ab <- ReadAffy(filenames=celfiles)
		# run function
		eSet <- preprocessFUN(ab, ...)
	} else {
		# run function
		eSet <- preprocessFUN(celfiles, ...)		
	}
	
	# add pheno data
	phenoData(eSet) <- adf
	return(eSet)
}

#############################################
# Function to Read Annotation file correctly
#############################################
read.AnnotatedDataFrame.AE <- function(filename, path, row.names = NULL, 
		blank.lines.skip = TRUE, fill = TRUE, varMetadata.char = "$", ...)
{
	adf <- read.AnnotatedDataFrame(filename, path=path, row.names = row.names, 
			blank.lines.skip = blank.lines.skip, fill = fill, varMetadata.char = varMetadata.char, ...)
	
	#remove empty lines
	emptylines = which(sapply(seq_len(nrow(pData(adf))), function(i)
						all(pData(adf)[i,]=="",na.rm=T)))
	if(length(emptylines) != 0)
		pData(adf) = pData(adf)[-emptylines,]
	
	#add row.names
	sampleNames(adf) <- adf$Array.Data.File
	
	return(adf)	
}

#############################################
# Function for rma para preprocessing
#############################################
preprocessFUNrmaPara <- function(celfiles, ...)
{
	require(affyPara)
	#distribute files
	celfiles <- distributeFiles(celfiles, to =  sub("tmp", "usr1/tmp", tempdir()),
			protocol = "RCP", hierarchicallyDist = FALSE, 
			master=FALSE, delExistTo=FALSE,
			full.names=TRUE)
	# run function
	eSet <- rmaPara(celfiles$CELfiles, ...)
	# remove files
	removeDistributedFiles(celfiles$to, master=FALSE)
	
	return(eSet)
}
