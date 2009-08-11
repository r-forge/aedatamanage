# Function to create statistic table of experiments in data structure
#
# History
# 04.03.2009 : 
#
# Example Code:
# dat <- create_statistic()
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

create_statistic <- function(path=getwd(),
		criteria=list( ChipType=function(adf) sum( adf$Array.Design.REF == "A-AFFY-33" ) ),
		verbose=getOption("verbose"))
{
	#read dirs	
	dirs <- file.info(list.files(path, full.name=T))$isdir
	dirs <- list.files(path, full.name=T)[dirs]
	
	#create output matrix
	erg <- matrix(0,nrow=length(dirs), ncol=2+length(criteria))
	rownames(erg) <- dirs
	colnames(erg) <- c("Experiments","Arrays",names(criteria))
	
	#fill output matrix for every directory
	for( i in dirs){
		# list experiments in dir
		experiments <- list.files(i,pattern=".sdrf.txt", full.names=T)
		nr_experiments <- length(experiments)
		erg[i,1] <- nr_experiments
	
		# count arrays in experiments in dir
		assays <- assays_chipType <- assays_canc <- 0
		for(j in experiments){
			# read Annotation file
			adf <- read.AnnotatedDataFrame.AE(j)
			erg[i,2] <- erg[i,2] + dim(adf)[1]
			
			# for further criteria
			for(k in 1:length(criteria))
				erg[i, 2+k] <- erg[i, 2+k] + sum( criteria[[k]](adf) )
		}	
	}
	rownames(erg) <- basename(dirs) 
	
	#return matrix as table
	return( as.table(erg) )
}