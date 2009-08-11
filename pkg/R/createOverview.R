# Function to create overview table of experiment data in data structure
#
# History
# 16.02.2009 : 
#
# Example Code:
# dat <- create_overview()
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

create_overview <- function(path=getwd(), 
		printtype="no", printfilename=paste(basename(getwd()), printtype, sep="."  ),
		chipType="A-AFFY-33", maxDesc=400,
		verbose=getOption("verbose"))
{
	# read files
	idffiles <- list.files(path=path, pattern='.idf.txt', recursive=TRUE, full.names=T)
	sdrffiles <- list.files(path=path, pattern='.sdrf.txt', recursive=TRUE, full.names=T)
	
	# check files
	if( length(idffiles)!=length(sdrffiles))
		stop("Missing idf or sdrf file!")
	
	# create table structure
	info <- matrix(nrow=length(idffiles), ncol=6)
	idffile <- basename(idffiles)
	study_name <- sub(".idf.txt", "", idffile)
	rownames(info) <- study_name
	colnames(info) <- c("ID", "Entity", "Title", "Description", "Arrays", "PubMed ID") 
	
	# Read infos
	for(file in idffiles)
	{
		idffile <- basename(file)
		study_name <- sub(".idf.txt", "", idffile)
		directory <- dirname(file)
		group_name <- basename(directory)
		
		if(verbose) cat(study_name, "-", group_name, ":\n")
		
		#Read idffile	
		idffile = scan(file,character(),sep="\n")
		idf.data = list()
		for(g in idffile) { 
			e = unlist(strsplit(g,"\t"))
			key=e[1] 
			if(length(e)>1)
				values=e[2:length(e)]
			else
				values=NA
			idf.data[[key]]=values
		}
		
		#Read sdrffile
		adf <- read.AnnotatedDataFrame.AE(paste(study_name, ".sdrf.txt", sep=""),
				path = directory)
		assays <- dim(adf)[1]	
	#	if( !"FactorValue..cancerous." %in% varLabels(adf))
	#		warning("No FactorValue..cancerous")
		
		assays_chipType <- sum(adf$Array.Design.REF==chipType)	
#		assays_canc <- sum(adf$FactorValue..cancerous.==1 & adf$Array.Design.REF==chipType)		
			
		#Create object
		if( nchar(idf.data$`Experiment Description`) > maxDesc ){
			desc <- substr(idf.data$`Experiment Description`, 0, maxDesc)
			desc <- paste(desc, "...")
		} else
			desc <- idf.data$`Experiment Description`
		info[study_name,] <- c(study_name, group_name, idf.data$`Investigation Title`,
			#	encoded_text_to_latex(desc) , paste(assays_canc, assays_chipType, assays, sep="/"), idf.data$`PubMed ID`)
				encoded_text_to_latex(desc) , paste(assays_chipType, assays, sep="/"), idf.data$`PubMed ID`)
	}
	
	# output
	if(printtype=="html" || printtype=="latex")
	{
		require(xtable)
		xinfo <- xtable(info)
		print(xinfo, type=printtype, file=paste(path,printfilename,sep="/"), include.rownames=FALSE)
	} else
		return(info)
}
