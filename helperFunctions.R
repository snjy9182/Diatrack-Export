##------------------------------------------------------------------------------
## getStartFrame
# returns starting frame of a track/trajectory (using its name) at a given index for a track list

#PARAMETERS: 
#track.list = named track list output
#Note: Last five characters of the original file name without extension (cannot contain ".")
#index = index of the track in the track list (track number)

##'@export getStartFrame
getStartFrame = function(track.list, index){
    return(as.numeric(substr(names(track.list[index]), 
                             gregexpr(pattern = '\\.', names(track.list[index]))[[1]][1]+1, 
                             gregexpr(pattern = '\\.', names(track.list[index]))[[1]][2]-1)));
}
