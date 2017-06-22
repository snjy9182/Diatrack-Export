## .readDiatrack
## a function to read one diatrack txt file and returns a list of tracks

.readDiatrack=function(file, interact=F,ab.track=F){

    # interactively open window
    if (interact==T) {
        file=file.choose()
    }

    file.name=basename(file)
    cat("\nReading Diatrack file: ",file.name,"...\n")

    ## skip the first 'comment line'
    data=read.table(file=file, header=F, skip=1)

    ## read in frame number line (for future use)
    frame.num=data[1,]

    ## remove frame number line for computation
    data=data[-1,]

    ## process the data
    # store coordinates of track in track.list
    track.list=list()
    # store absolute coordinates of track for comparison plots
    ab.track.list=list()
    # store num.tracks.per.file
    num.tracks.per.file=c()

    # select 3 column at a time
    # can use frame number to do this, but this way makes the program more
    # robust with little to non decrease in efficiency

    for (i in 1:(dim(data)[2]/3)) {

        #i=2

        triple=i*3
        track=dplyr::select(data,(triple-3+1):triple)
        colnames(track)=c("x","y","z")
        track=dplyr::filter(track,x!=0,y!=0)

        # the [[]] is important, otherwise only x is included
        track.list[[i]]=track

        # store num.tracks.per.file
        num.tracks.per.file[i]=dim(track)[1]


        ## preprocess to fix coordinates from 0 to max
        ## absolute value of trajectory movement

        abTrack=data.frame(x=track$x-min(track$x),
                            y=track$y-min(track$y))
        ab.track.list[[i]]=abTrack

    }

    ## name the tracks

    # frame.id
    frame.num.mx=matrix(frame.num,ncol=3,nrow=length(frame.num)/3,byrow=T)
    frame.id=unlist(frame.num.mx[,1])

    # duration


    # duration=table(frame.id)
    duration=num.tracks.per.file

    # file.id
    file.subname=substr(file.name,
           start=nchar(file.name)-8,
           stop=nchar(file.name)-4)

    file.id=rep(file.subname,length(duration))

    # indexPerFile
    indexPerFile=seq(from=1,to=length(duration))

    ## trackID=fileID.frameID.duration.indexPerFile
    track.name=paste(file.id,frame.id,duration,indexPerFile,sep=".")

    # name the track
    names(track.list)=track.name
    names(ab.track.list)=track.name

    if (ab.track==T) return(ab.track.list) else return(track.list)

}