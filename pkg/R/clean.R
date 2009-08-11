# Function to clean data structure
#
# History
# 16.02.2009 : 
#
# Example Code:
# clean()
# 
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

clean <- function(path=getwd(), 
		pattern=c(".Rdata", "sdrf.c.txt", ".pdf", ".html", ".cel", "~") )
{
	#remove data
	for(i in pattern)
	{
		files <- list.files(path=path, pattern=i, recursive=TRUE, full.names=T)
		file.remove(files)
	}
}
