# Functions to create data structure
#
# History
# 16.02.2009 : 
#
# Example Code:
# createDataStruct(data= list( breast=c("E-TABM-43", "E-GEOD-1561"), prostate=c("E-GEOD-2443") ) )
#
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

createDataStruct <- function(path = getwd(), data = list(), name="ArrayExpressData" )
{
	require(ArrayExpress)
	
	#create main directory and switch path
	setwd(path)
	if( !file.exists(name))
		dir.create(name, recursive=TRUE)
	else cat("data structure exists!\n")
	main_path <- paste(path, name, sep="/")
	setwd(main_path)
	
	#subdirectories
	group_names <- names(data)
	for(i in group_names)
	{
		cat("Group: ",i, "\n")
		#create sub-directory and switch path
		if( !file.exists(i))
			dir.create(i)
		else cat("group structure exists!\n")
		new_path <- paste(main_path, i, sep="/")
		
		#get data
		for(j in data[[i]]){
			cat("\t", j, "\n")
			if( !file.exists(paste(new_path,"/",j,".raw.zip",sep="")) 
					|| !file.exists(paste(new_path,"/",j,".idf.txt",sep=""))
					|| !file.exists(paste(new_path,"/",j,".sdrf.txt",sep="")) )
				getAE(j, path=new_path, type="raw", extract=FALSE)
			else cat("\texperiment exists!\n")
		}
	}
	
	#Check
	zipfiles <- basename( list.files(paste(path,name,sep="/"), pattern=".raw.zip", recursive=TRUE) )
	zipfiles <- sub(".raw.zip","", zipfiles)	
	data <- unlist(data)
	if( !all(zipfiles %in% data) )
		warning("There are to much raw.zip files:", zipfiles[which(zipfiles %in% data == FALSE)])
	if( !all(data %in% zipfiles) )
		warning("There are missing raw.zip files:", data[which(data %in% zipfiles == FALSE)])
	return(main_path)
}
