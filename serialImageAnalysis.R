#### DOWNLOAD FOLDER OF FILES FROM GOOGLE TEAM DRIVE ############# ----
teamDriveDownload <- function(team.drive.name, folder.name){
    if(!require("googledrive")) install.packages("googledrive")
    library(googledrive)
    q.search <- paste0("name contains '",folder.name,"'")
    folder.id <- as.data.frame(
        drive_find(team_drive = team.drive.name, q=q.search)$drive_resource,
        stringsAsFactors = F)$id
    folder.files <- drive_ls(as_id(folder.id))
    drive_download(as_id(folder.files$id[1]))
    lapply(folder.files$id, function(x) drive_download(as_id(x),overwrite = T))
}

countFiles <- function(team.drive.name,folder.name){
    q.search <- paste0("name contains '",folder.name,"'")
    folder.id <- as.data.frame(
        drive_find(team_drive = team.drive.name, q=q.search)$drive_resource,
        stringsAsFactors = F)$id
    folder.files <- drive_ls(as_id(folder.id))
    length(folder.files$name)
}

#### PROCESSING IMAGES AFTER DOWNLOADING ############# ----

#function to drop rows & columns in data frame
dropRowsCols <- function(img, drop.rows=NA, drop.cols=NA){
    #drop.rows and drop.cols is list of row or col numbers to be dropped form dataframe
    ifelse(test = !is.na(drop.rows),
        img <- img[-c(drop.rows),],
        img <- img
    )
    ifelse(test = !is.na(drop.cols),
        img <- img[ ,-c(drop.cols)],
        img <- img
    )
    t(img)
}

binning.jpeg  <- function(
    dir, bin.length.sec=3600, frame.interval.sec = 10,
    dir.out="bins", drop.rows=NA, drop.cols=NA, height.px=100) {
    # load package, add download if not there
    if (!"EBImage" %in% rownames(installed.packages())) {
        source("http://bioconductor.org/biocLite.R")
        biocLite("EBImage")
    }
    library(EBImage)
    
    #make list of file paths and make dirs
    files  <- list.files(path = dir, pattern = "*.jpg", full.names = F)
    dir.create("csv",showWarnings = FALSE)
    dir.create("delta",showWarnings = FALSE)
    dir.create(dir.out,showWarnings = FALSE)
    
    print("----------   Converting JPEGs to CSVs   ----------")
    p <- i  <- 0
    for (file in files) {
        img <- as.array(
            resize(
                dropRowsCols(
                    channel(readImage(paste0(dir,file),
                                      type = "jpeg"),"gray"
                    ),
                    drop.rows=drop.rows,drop.cols=drop.cols
                ), h = height.px
            )
        )
        write.csv(img,file=paste0("csv/",file))
        i  <- i + 1
        p  <- (i/length(files))*100
        print(paste0(dir,file," - ",round(p,2),"%"))
    }
    print(" ")
    print("----------   Generating Differences Matrices    ----------")
    delta <- files[-1]
    baseline  <- files[-length(files)]
    p <- i  <- 0
    for(i in 1:length(delta)){
        array  <- read.csv(paste0("csv/",delta[i])) - read.csv(paste0("csv/",baseline[i]))
        write.table(x=abs(array), file = paste0("delta/",delta[i]),sep = ",")
        p  <- (i/length(delta))*100
        print(paste0(delta[i]," ",round(p,2)," %"))
    }
    #bin.length.sec  <- 3600   # function should input this value (default 1hr)
    #frame.interval.sec <- 10 # function shoud input this value (default 10 sec)
    frames.per.bin <- bin.length.sec/frame.interval.sec # function should input this value 36000=sec in hr
    bin.number  <- 1:round(length(delta)/frames.per.bin)
    if ((length(delta) %% 2) == 1) {
        write.table(x=data.frame(
            matrix(
                0,
                nrow=nrow(read.csv(paste0("delta/",delta[1]))),
                ncol=ncol(read.csv(paste0("delta/",delta[1])))
            )
        ), file = paste0("delta/0.jpg"),sep = ",")
    }
    #bin.number.char <-  sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin.number)
    print("----------   Adding Matrices and Saving     ----------")
    print(paste0("bin - start - finish"))
    delta <- list.files("delta/",pattern = "*.jpg", full.names = FALSE)
    for (bin in bin.number) {
        start  <- as.numeric((bin-1)*frames.per.bin+1)
        finish <- bin*frames.per.bin
        array  <- data.frame(matrix(
            0,
            nrow=nrow(read.csv(paste0("delta/",delta[1]))),
            ncol=ncol(read.csv(paste0("delta/",delta[1])))
        ))
        for (number in start:finish){
            array <- array + read.csv(paste0("delta/",delta[number]))
        }
        write.table(x=array, file = paste0(
            dir.out,"/","bin-",sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin),
            ".frame-",start,"to",finish,".secs",bin.length.sec,".raw.csv"),
            sep = ",",row.names = F,col.names = F)
        write.table(x=scale(array), file = paste0(
            dir.out,"/","bin-",sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin),
            ".frame-",start,"to",finish,".secs",bin.length.sec,".zscore.csv"),
            sep = ",",row.names = F,col.names = F)
        print(paste0(" ",bin," -   ",start,"  -   ",finish))
    }
}

binning.png  <- function(
    dir, bin.length.sec=3600, frame.interval.sec = 10,
    dir.out="bins", drop.rows=NA, drop.cols=NA, height.px=100) {
    # load package, add download if not there
    if (!"EBImage" %in% rownames(installed.packages())) {
        source("http://bioconductor.org/biocLite.R")
        biocLite("EBImage")
    }
    library(EBImage)
    
    #make list of file paths and make dirs
    files  <- list.files(path = dir, pattern = "*.png", full.names = F)
    dir.create("csv",showWarnings = FALSE)
    dir.create("delta",showWarnings = FALSE)
    dir.create(dir.out,showWarnings = FALSE)
    
    print("----------   Converting PNGs to CSVs   ----------")
    p <- i  <- 0
    for (file in files) {
        img <- as.array(
            resize(
                dropRowsCols(
                    channel(readImage(paste0(dir,file),
                                      type = "png"),"gray"
                    ),
                    drop.rows=drop.rows,drop.cols=drop.cols
                ), h = height.px
            )
        )
        write.csv(img,file=paste0("csv/",file))
        i  <- i + 1
        p  <- (i/length(files))*100
        print(paste0(dir,file," - ",round(p,2),"%"))
    }
    print(" ")
    print("----------   Generating Differences Matrices    ----------")
    delta <- files[-1]
    baseline  <- files[-length(files)]
    p <- i  <- 0
    for(i in 1:length(delta)){
        array  <- read.csv(paste0("csv/",delta[i])) - read.csv(paste0("csv/",baseline[i]))
        write.table(x=abs(array), file = paste0("delta/",delta[i]),sep = ",")
        p  <- (i/length(delta))*100
        print(paste0(delta[i]," ",round(p,2)," %"))
    }
    #bin.length.sec  <- 3600   # function should input this value (default 1hr)
    #frame.interval.sec <- 10 # function shoud input this value (default 10 sec)
    frames.per.bin <- bin.length.sec/frame.interval.sec # function should input this value 36000=sec in hr
    bin.number  <- 1:round(length(delta)/frames.per.bin)
    if ((length(delta) %% 2) == 1) {
        write.table(x=data.frame(
            matrix(
                0,
                nrow=nrow(read.csv(paste0("delta/",delta[1]))),
                ncol=ncol(read.csv(paste0("delta/",delta[1])))
            )
        ), file = paste0("delta/0.png"),sep = ",")
    }
    #bin.number.char <-  sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin.number)
    print("----------   Adding Matrices and Saving     ----------")
    print(paste0("bin - start - finish"))
    delta <- list.files("delta/",pattern = "*.png", full.names = FALSE)
    for (bin in bin.number) {
        start  <- as.numeric((bin-1)*frames.per.bin+1)
        finish <- bin*frames.per.bin
        array  <- data.frame(matrix(
            0,
            nrow=nrow(read.csv(paste0("delta/",delta[1]))),
            ncol=ncol(read.csv(paste0("delta/",delta[1])))
        ))
        for (number in start:finish){
            array <- array + read.csv(paste0("delta/",delta[number]))
        }
        write.table(x=array, file = paste0(
            dir.out,"/","bin-",sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin),
            ".frame-",start,"to",finish,".secs",bin.length.sec,".raw.csv"),
            sep = ",",row.names = F,col.names = F)
        write.table(x=scale(array), file = paste0(
            dir.out,"/","bin-",sprintf(paste0("%0",max(nchar(bin.number)),"d"), bin),
            ".frame-",start,"to",finish,".secs",bin.length.sec,".zscore.csv"),
            sep = ",",row.names = F,col.names = F)
        print(paste0(" ",bin," -   ",start,"  -   ",finish))
    }
}

filterNoise <- function(dir, pattern = "raw.csv", filter.radius = 1,out.dir="bins.filtered/" ){
    files <- list.files(path = dir,pattern = pattern,full.names = T)
    dir.create(path = out.dir)
    img <- NULL
    for (file in files) {
        img <- as.array(medianFilter(
            data.matrix(read.csv(file,header = F,sep = ","))
            ,size = filter.radius)
        )
        write.table(
            img,
            paste0(out.dir,sub(pattern = dir,replacement = "",file)),
            col.names = F, row.names = F,sep = ","
        )
    }
}

#### MAKE DATAFRAME SUMMERIZING CHANGE FROM FRAME TO FRAME ############# ----

# sum rows or cols of data frame
sumRowsOrCols <- function(img, bin.cols = TRUE, drop.rows=NA, drop.cols=NA){
    ifelse(
        bin.cols == TRUE,
        bin.cols <- 1,
        bin.cols <- 2
    )
    apply(
        X = dropRowsCols(
            img = data.matrix(img), drop.rows = drop.rows, drop.cols = drop.cols
        ),
        MARGIN = bin.cols,
        FUN = sum
    )
}

# sums of AoIs in a single image
AoIs <- function(img, numAoIs){
    len <- length(img)
    size <- round(len/numAoIs,0)
    i1 <- NULL
    i2 <- NULL
    sapply(
        1:numAoIs,
        FUN =  function(x){
            sum(img[sum((x-1)*size+1):(x*size)],na.rm = TRUE)
        },simplify = TRUE
    )
}

## NOT WORKING ##
seriesAoIs <- function( 
    dir, pattern = ".raw.csv", numAoIs, bin.cols = TRUE,
    drop.rows=NA, drop.cols=NA, bin.names=c("left","right"), time.intervals=NA
) {
    array.of.sums <- NULL
    images <- list.files(path = dir,pattern = pattern,full.names = T)
    for (image in images) {
        img <- read.csv(
            dropRowsCols(image,drop.rows = drop.rows,drop.cols = drop.cols), 
            header = F,sep = ","
        )
        bin.sums <- AoIs(
            sumRowsOrCols(
                img = img,
                bin.cols = bin.cols),
            numAoIs = numAoIs
        )
        array.of.sums <- rbind.data.frame(array.of.sums,t(bin.sums))
    }
    ifelse(
        is.na(time.intervals),
        rownames(array.of.sums) <- 1:nrow(array.of.sums),
        rownames(array.of.sums) <- time.intervals
    )
    array.of.sums <- rbind.data.frame(bins=c(bin.names),array.of.sums)
    t(array.of.sums)
}

#### MAKE VISUALIZATIONS FROM ANALYZED SERIAL IMAGES ############# ----

makeHeatmaps <- function(dir, bin.type = "zscore",file.prefix="heatmap.",
    my.colors = c("black","purple","blue", "red","orange","yellow")
){
    # load package, add download if not there
    if (!"gplots" %in% rownames(installed.packages())) {
        install.packages("gplots")
    }
    library(gplots)
    if (!"RColorBrewer" %in% rownames(installed.packages())) {
        install.packages("RColorBrewer")
    }
    library(RColorBrewer)
    
    dir <- paste0(dir,"/")
    my.colors <- colorRampPalette(my.colors)(n = 299)
    files <- list.files(path = dir,pattern = paste0("*.",bin.type,".csv"),full.names = T)
    pdf(file = paste0(file.prefix,bin.type,".pdf"),width = 11,height = 8.5)
    lapply(
        files,
        function(x) {
            heatmap.2(
                
                data.matrix(read.csv(x,header = F)[,-1]),
                main = sub(dir,"",sub(paste0(".",bin.type,".csv"),"",x)), # heat map title
                density.info="density",  # turns off density plot inside color legend
                trace="none",         # turns off trace lines inside the heat map
                margins =c(12,9),     # widens margins around plot
                col=my.colors,       # use on color palette defined earlier
                # breaks=col_breaks,    # enable color transition at specified limits
                dendrogram="none",     # only draw a row dendrogram
                na.color="black",
                Rowv = FALSE,
                Colv=FALSE
            )            # turn off column clustering
        }
    )
    dev.off()
}

makeGraph <- function(
    df.of.sums, pdf.name="graphs.pdf"
){
    pdf(file = pdf.name,height = 11,width = 8.5)
    par()
    #make total movement graph----
    colSums(df.of.sums[,-1])
    #make movement in each bin graph----
    #make fraction of movement in each bin graph----
    dev.off()
}

###### TO DO #########
# need to make functions to make graphs of activity automaticlly
