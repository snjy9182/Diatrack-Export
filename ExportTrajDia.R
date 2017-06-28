install.packages("R.matlab")
install.packages("devtools")
devtools::install_github("schmidtchristoph/reach/reach")
#library(reach)
library(matlabr)
#library(R.matlab)
library(smt)
start.time = Sys.time()
run_matlab_script("/Users/sunjayyoo/Dropbox/Work/Diatrack\ Export/ExportTrajDia.m")
file = '/Users/sunjayyoo/Dropbox/Work/Diatrack\ Export/test.txt'
trackll2= .readDiatrack(file)
end.time = Sys.time();
time.taken = end.time - start.time