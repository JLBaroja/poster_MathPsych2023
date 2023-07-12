to_hex <- function(int,digits=2){
    str <- as.character(as.hexmode(int))
    if(nchar(str)<digits){
        str <- paste('0',str,sep='')
    }
    return(str)
}

nice_scatter <- function(x,y,
                        color='#0044ee',
                        xlimz=NULL,
                        ylimz=NULL,
                        n_mids=40,
                        new_plot=T,
                        style='polygon',
                        point_scale=4,
                        raw_data=F,
												...){
    if(is.null(xlimz)){
        xlimz <- c(min(x),max(x))}
    if(is.null(ylimz)){
        ylimz <- c(min(y),max(y))}
  #  n_mids <- 30
    
    if(new_plot){
        plot(NULL,xlim=xlimz,ylim=ylimz,ann=F,...)
        if(raw_data){
            points(x,y,col='#cccccc')}
        }
    
    count_grid <- array(dim=c(n_mids,n_mids))
    x_brks <- seq(xlimz[1],xlimz[2],length.out = n_mids+1)
    y_brks <- seq(ylimz[1],ylimz[2],length.out = n_mids+1)
    x_width <- x_brks[2]-x_brks[1]
    y_width <- y_brks[2]-y_brks[1]
    x_mids <- x_brks[1:(length(x_brks)-1)]+x_width/2
    y_mids <- y_brks[1:(length(y_brks)-1)]+y_width/2
    for(i in 1:n_mids){
        for(j in 1:n_mids){
            # Counting
            count_grid[i,j] <- sum(x>=x_brks[i]
                                   &x<x_brks[(i+1)]
                                   &y>=y_brks[j]
                                   &y<y_brks[(j+1)])
        }
    }
    for(i in 1:n_mids){
        for(j in 1:n_mids){
            # Plotting
            alpha <- round(count_grid[i,j]*(255/max(count_grid)))
            # Squares of different sizes
            if(style=='points'){
                points(x_mids[i],y_mids[j],cex=point_scale*count_grid[i,j]/max(count_grid),
                       col=color,
                       pch=22,bg=paste(color,to_hex(alpha),sep=''))}
            # Polygons of different shades
            if(style=='polygon'){
                polygon(x=c(x_mids[i]+rep(c(-1,1),each=2)*x_width/2),
                        y=c(y_mids[j]+c(-1,1,1,-1)*y_width/2),
                        border=paste(color,to_hex(alpha),sep=''),
                        #border=NA,
                        col=paste(color,to_hex(alpha),sep=''))}
        }
    }
}

