#Install packages and dependencies
install.packages("R.matlab")
library(R.matlab)

#readDiaSessions function definition
readDiaSessions = function(file, interact = FALSE, timer = FALSE){
    
    #Interactively open window
    if (interact == TRUE) {
        file=file.choose();
    }
    
    #Start timer
    if (timer == TRUE) {
        start.time = Sys.time()
    }
    
    #Display starter text
    file.name = basename(file);
    cat("\nReading Diatrack session file: ",file.name,"...\n");
    
    #Pre-process data
    #Successor and predecessor rows of first frame switched for consistency
    #(Unsure why Diatrack reverses the ordering of these two rows for the first frame)
    data <- readMat(file)$tracks;
    temp <- data[1][[1]][[1]][[7]]; 
    data[1][[1]][[1]][[7]] <- data[1][[1]][[1]][[6]];
    data[1][[1]][[1]][[6]] <- temp;
    
    #Data structure of data:
    #data[FRAME][[1]][[1]][[ROWVARIABLE]][[VALUE]] 
    
    #Instantiate indexing variables and the track list
    startIndex = 0;
    startFrame = 1;
    trajectoryIndex = 1;
    track.list = list();
    
    #Loop for each trajectory track to be saved into track list
    repeat{
        
        #Loop until the next particle without a predecessor is found
        repeat{
            
            #Basic iteration through indexes and then through frames
            if (startIndex == 0 || startIndex < ncol(data[[startFrame]][[1]][[1]])){
                startIndex = startIndex + 1;
            } else {
                startFrame = startFrame + 1;
                startIndex = 1;
            }
            
            #Check at each iteration
            if (startFrame > length(data)){ #Break at end frame
                break;
            } else if (length(data[startFrame][[1]][[1]][[1]]) == 0){ #Iterate to next frame at empty frames
                startFrame = startFrame + 1;
                startIndex = 1;
            } else if (data[startFrame][[1]][[1]][[6]][[startIndex]] == 0) { #Break if particle is found
                break;
            }
            #Do nothing and iterate to next indexed particle if no particle is found
        }
        
        #Break track loop at end frame
        if (startFrame > length(data)){
            break;
        }
        
        #Instantiate initial frame and index coordinates into looping frame and index coordinates
        frame = startFrame;
        index = startIndex;
        
        #Create temporary track to insert into track list
        track <- data.frame(x = numeric(), y = numeric(), z = integer());
        
        #Loop through every instance the particle exists and break once it no longer has successors
        repeat{
            RefinedCooX = round(data[frame][[1]][[1]][[2]][[index]], 2);
            RefinedCooY = round(data[frame][[1]][[1]][[1]][[index]], 2);
            RefinedCooZ = round(data[frame][[1]][[1]][[3]][[index]], digits = 1);
            track <- rbind(track, data.frame(x = RefinedCooX, y = RefinedCooY, z = RefinedCooZ));
            if (data[frame][[1]][[1]][[7]][[index]] != 0) {
                index = data[frame][[1]][[1]][[7]][[index]];
                frame = frame + 1;
            } else
                break;
        }
        
        #Append temporary track for particle into track list and iterate to the next trajectory
        track.list[[trajectoryIndex]] <- track;
        trajectoryIndex = trajectoryIndex + 1;
    }
    
    #Display stop timer
    if (timer == TRUE) {
        end.time = Sys.time();
        time.taken = end.time - start.time;
        cat("\nDuration: ");
        cat(time.taken);
        cat(" mins\n");
    }
    
    #Confirmation text and return track list
    cat("Session file read.\n")
    return(track.list);
} 

