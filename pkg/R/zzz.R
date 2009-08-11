# zzz.R
# 
# functions for initialize and clean up.
#
# History:
# 16.02.2009 : Version 0.1
#
# Copyright (C) 2009 : Markus Schmidberger <schmidb@ibe.med.uni-muenchen.de>
###############################################################################

.onAttach <- function(lib, pkg) {
	if(interactive() && .Platform$OS.type == "windows" && .Platform$GUI == "Rgui"){
		addVigs2WinMenu("ArrayExpressDataManage")
	}
}
