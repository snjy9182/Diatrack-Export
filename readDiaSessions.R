#Install packages and dependencies
install.packages("R.matlab")
library(R.matlab)


#Load in file
file <- '/Users/sunjayyoo/Dropbox/Work/Diatrack\ Export/HTZ1Halo_fr10ms_120mW_14.mat'
data <- readMat(file)$tracks
backup <- data
temp <- data[1][[1]][[1]][[7]]
data[1][[1]][[1]][[7]] <- data[1][[1]][[1]][[6]]
data[1][[1]][[1]][[6]] <- temp
#data[FRAME][[1]][[1]][[ROWVARIABLE]][[VALUE]]

#Start of algorithm
startIndex = 0;
startFrame = 1;
trajectories = 1;
track.list = list();
repeat{
    repeat{
        if (startIndex == 0 || startIndex < ncol(data[[startFrame]][[1]][[1]])){
            startIndex = startIndex + 1;
        } else {
            startFrame = startFrame + 1;
            startIndex = 1;
        }
        if (startFrame > length(data)){
            break
        } else if (length(data[startFrame][[1]][[1]][[1]]) == 0){
            startFrame = startFrame + 1;
            startIndex = 1;
        } else if (data[startFrame][[1]][[1]][[6]][[startIndex]] == 0)
            break
    }
    if (startFrame > length(data))
        break
    frame = startFrame;
    index = startIndex;
    track <- data.frame(x = numeric(), y = numeric(), z = integer());
    repeat{
        track <- rbind(track, data.frame(x = data[frame][[1]][[1]][[2]][[index]], y = data[frame][[1]][[1]][[1]][[index]], z= data[frame][[1]][[1]][[3]][[index]]))
        if (data[frame][[1]][[1]][[7]][[index]] != 0){
            index = data[frame][[1]][[1]][[7]][[index]];
            frame = frame + 1;
        } else
            break
    }
    track.list[[trajectories]] = track
    trajectories = trajectories + 1
}
