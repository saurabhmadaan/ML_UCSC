# TODO: How to Install a New Package
# 
# Author: PatriciaHoffman
###############################################################################

help.start()
install.packages("vcd")
help(package="vcd")
library(vcd)
help(Arthritis)
Arthritis
example(Arthritis)
q()
#
#install.packages(pkgs, lib, repos, contriburl, method, available, destdir, dependencies, type, configure.args, configure.vars, clean, Ncpus, ...)
#- package:utils
install.packages("rJava")
#help(package="rJava")
install.packages("XML")
#


#To install a .zip file as a package in R
#
#
#add the file to these addresses on my computer
#C:\Program Files\R\R-2.11.1-x64\library
#and
#C:\Program Files (x86)\R\R-2.11.1\library 
#
#then use command 
#install.packages("package name")
#library(package name)




# you can save an object from the work space and read it in again later
#Description  save  http://127.0.0.1:30048/library/base/html/save.html

#save writes an external representation of R objects to the specified file. The objects can be read back from the file at a later date by using the function load (or data in some cases).
#
#save.image() is just a short-cut for ‘save my current workspace’, i.e., save(list = ls(all=TRUE), file = ".RData"). It is also what happens with q("yes").
#Usage
#
#save(..., list = character(0L),
#		file = stop("'file' must be specified"),
#		ascii = FALSE, version = NULL, envir = parent.frame(),
#		compress = !ascii, compression_level,
#		eval.promises = TRUE, precheck = TRUE)
#
#save.image(file = ".RData", version = NULL, ascii = FALSE,
#		compress = !ascii, safe = TRUE)
#
#Arguments
#... 	the names of the objects to be saved (as symbols or character strings).
#list 	A character vector containing the names of objects to be saved.
#file 	a connection or the name of the file where the data will be saved. Must be a file name for workspace format version 1.


#Description  for load http://127.0.0.1:30048/library/base/html/load.html
#
#Reload datasets written with the function save.
#Usage
#
#load(file, envir = parent.frame())
#
#Arguments
#file 	a (readable binary) connection or a character string giving the name of the file to load.
#envir 	the environment where the data should be loaded.
#
#
#Description   data   http://127.0.0.1:30048/library/utils/html/data.html
#
#Loads specified data sets, or list the available data sets.
#Usage
#
#data(..., list = character(0), package = NULL, lib.loc = NULL,
#		verbose = getOption("verbose"), envir = .GlobalEnv)
#
#Arguments
#... 	a sequence of names or literal character strings.
#list 	a character vector.
#package 	a character vector giving the package(s) to look in for data sets, or NULL.
#
#By default, all packages in the search path are used, then the ‘data’ subdirectory (if present) of the current working directory.
#lib.loc 	a character vector of directory names of R libraries, or NULL. The default value of NULL corresponds to all libraries currently known.
#verbose 	a logical. If TRUE, additional diagnostics are printed.
#envir 	the environment where the data should be loaded.
