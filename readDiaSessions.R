


startIndex = 0;
startFrame = 1;
trajectories = 1;
#create dataframe with XYZ indexed by trajectories
repeat{
    repeat{
        if (startIndex == 0 || startIndex < maxIndex)
            startIndex++;
        else {
            startFrame++;
            startIndex = 1;
        }
        if (predecessor == 0 || startFrame > maxFrame)
            break;
    }
    if (startFrame > maxFrame)
        break;
    frame = startFrame;
    index = startIndex;
    repeat{
        #add RefinedCooYXZ to dataframe
        if (successor != 0){
            index = successor;
            frame++;
        } else
            break;
    }
}

return dataframe;