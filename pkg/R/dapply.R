# Function to run function FUN to data in directory
#
# History
# 16.02.2009 : 
#
# Example Code:
# dapply(function(x){ print(file.info(x))})
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

dapply <- function(FUN, path=getwd(), pattern=NULL, recursive=TRUE, ...)
{
	# list files
	files <- list.files(path=path, pattern=pattern, recursive=recursive, full.names=T)
	# run apply
	return( lapply(files, FUN, ...) )	
}
