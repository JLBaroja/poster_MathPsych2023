source('plotting_functions.R')
library('grImport')

# Load pigeon data and model results (obtained with 'jags_pigeons.R')
pigeons <- read.csv('pigeon_data.csv')
load('posteriors_pigeons.RData')

bird_colors <- c('#1b9e77','#e65201','#6146ca','#b12060','#68affc','#4749dc')


bird_data_postdct <- function(brd,type='simple'){
	# Plots data and postdiction for a single bird
	# 'brd' is a NUMBER between 1:6
	
	# Info extraction 
	b_dta <- subset(pigeons,bird_num==brd)
	alpha <- nds_pigeons$alpha[,brd]
	beta <- nds_pigeons$beta[,brd]
	
	n_smpl <- 1000
	x <- seq(-3,3,length.out=50)
	x_plt <- NULL
	y_plt <- NULL
	for(i in 1:n_smpl){
	  x <- rnorm(n=20,mean=0,sd=1.5)
	  #x <- runif(n=20,min=-3,max=3)
		y <- alpha[i]+beta[i]*x
		x_plt <- append(x_plt,x)
		y_plt <- append(y_plt,y)
	}

	# Plotting
	if(type=='simple'){
		plot(NULL,xlim=c(-3,3),ylim=c(-3,3))
	points(log(b_dta$n_reinf_right/b_dta$n_reinf_left),
		log(b_dta$n_resp_right/b_dta$n_resp_left),
		pch=21,bg='#ffffff',lwd=2,cex=2)
	}
	if(type=='complex'){
	nice_scatter(x_plt,y_plt,	
		xlimz=c(-3,3),ylimz=c(-3,3),color=bird_colors[brd],axes=F)
	box()
	points(log(b_dta$n_reinf_right/b_dta$n_reinf_left),
		log(b_dta$n_resp_right/b_dta$n_resp_left),
		pch=21,bg='#ffffff',lwd=2,cex=2)
	par(cex.axis=1.25,tck=-0.02,mgp=c(3,0.75,0))
	axis(1);axis(2,las=1)
	abline(0,1,lty='dashed',lwd=2.5,col='#ee0000')
	}
}



pigeons_data_plot <- function(x_cntr,y_cntr,wdth,hght,m1,m2){
	left_lim <- x_cntr-wdth/2+m2
	bottom_lim <- y_cntr-hght/2+m1
	right_lim <- x_cntr+wdth/2
	top_lim <- y_cntr+hght/2
	x_centers <- seq(left_lim,right_lim,length.out=7)[c(2,4,6)]
	y_centers <- seq(bottom_lim,top_lim,length.out=5)[c(2,4)]
	x_centers <- rep(x_centers,each=2)
	y_centers <- rep(sort(y_centers,decreasing=T),3)
	for(bn in 1:6){
		new_plot(which_point='center_center',
			c(x_centers[bn],y_centers[bn]),
			width=0.8*abs(right_lim-left_lim)/3,
			height=0.8*abs(top_lim-bottom_lim)/2,
			paste('pn',bn))
		bird_data_postdct(bn,type='complex')
	}
	# X label
	new_plot(which_point='left_top',
		c(left_lim,bottom_lim),
		width=wdth-m2,height=m1)
	text_plot()
	text(0,0,expression(paste(log,'(',W[right],'/',W[left],')')),cex=3,family='CMU Serif')
	# Y label
	new_plot(which_point='right_bottom',
		c(left_lim,bottom_lim),
		width=m2,height=hght-m1)
	text_plot()
	text(0,0,expression(paste(log,'(',B[right],'/',B[left],')')),cex=3,family='CMU Serif',srt=90)
	return(list(left_lim=left_lim,bottom_lim=bottom_lim,
		right_lim=right_lim,top_lim=top_lim,
		x_centers=x_centers,y_centers=y_centers))
}

#try(dev.off())
#x11(width=15.5,height=10.5)
#layout(matrix(1:6,ncol=3))
#pigeons_data_plot()
