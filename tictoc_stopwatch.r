#Simple stopwatch

proc.time()


#tictoc package: tic starts the watch, toc stops it

install.packages("tictoc")
library(tictoc)

tic()
2+2
Sys.sleep(1)
toc()