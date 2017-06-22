install.packages("R.matlab")
install.packages("devtools")
devtools::install_github("schmidtchristoph/reach/reach")
#library(reach)
library(matlabr)
#library(R.matlab)
library(smt)

#runMatlabFct( "[] = ExportTrajDia()" )
run_matlab_script("ExportTrajDia1.m")

file = 'test.txt'
trackll= .readDiatrack(file)
trackll
