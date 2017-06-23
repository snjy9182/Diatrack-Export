install.packages("R.matlab")
install.packages("devtools")
devtools::install_github("schmidtchristoph/reach/reach")
#library(reach)
library(matlabr)
#library(R.matlab)
library(smt)
start.time = Sys.time()
#runMatlabFct( "[] = ExportTrajDia()" )
run_matlab_script("ExportTrajDia.m")
file = 'test.txt'
trackll2= .readDiatrack(file)
end.time = Sys.time();
time.taken = end.time - start.time