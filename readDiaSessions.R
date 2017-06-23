#Install packages and dependencies
install.packages("R.matlab")
library(R.matlab)

#readDiaSessions function definition
readDiaSessions = function(file, interact = FALSE, timer = FALSE){
    
    # interactively open window
    if (interact == TRUE) {
        file=file.choose();
    }
    if (timer == TRUE) {
        start.time = Sys.time()
    }
    
    file.name = basename(file);
    cat("\nReading Diatrack session file: ",file.name,"...\n");
    
    #Pre-process data
    data <- readMat(file)$tracks;
    #backup <- data
    temp <- data[1][[1]][[1]][[7]];
    data[1][[1]][[1]][[7]] <- data[1][[1]][[1]][[6]];
    data[1][[1]][[1]][[6]] <- temp;
    #data[FRAME][[1]][[1]][[ROWVARIABLE]][[VALUE]]
    
    #Start of algorithm
    startIndex = 0;
    startFrame = 1;
    trajectoryIndex = 1;
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
                break;
            } else if (length(data[startFrame][[1]][[1]][[1]]) == 0){
                startFrame = startFrame + 1;
                startIndex = 1;
            } else if (data[startFrame][[1]][[1]][[6]][[startIndex]] == 0)
                break;
        }
        if (startFrame > length(data))
            break;
        frame = startFrame;
        index = startIndex;
        track <- data.frame(x = numeric(), y = numeric(), z = integer());
        repeat{
            RefinedCooX = round(data[frame][[1]][[1]][[2]][[index]], 2);
            RefinedCooY = round(data[frame][[1]][[1]][[1]][[index]], 2);
            RefinedCooZ = round(data[frame][[1]][[1]][[3]][[index]], digits = 1);
            track <- rbind(track, data.frame(x = RefinedCooX, y = RefinedCooY, z = RefinedCooZ));
            if (data[frame][[1]][[1]][[7]][[index]] != 0){
                index = data[frame][[1]][[1]][[7]][[index]];
                frame = frame + 1;
            } else
                break;
        }
        track.list[[trajectoryIndex]] <- track;
        trajectoryIndex = trajectoryIndex + 1;
    }
    if (timer == TRUE) {
        end.time = Sys.time();
        time.taken = end.time - start.time;
        cat("\nDuration: ");
        cat(time.taken);
        cat(" mins\n");
    }
    cat("Session file read.\n")
    return(track.list);
} 

