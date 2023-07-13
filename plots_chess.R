source('plotting_functions.R')
#library('grImport')

# Load pigeon data and model results (obtained with 'jags_chess.R')
chess <- read.csv('queens_gambit_black.csv')
load('posteriors_chess_elo.RData')
nds_chess_elo <- nds_chess
load('posteriors_chess_experience.RData')
nds_chess_experience <- nds_chess
rm('nds_chess')

bird_colors <- c('#1b9e77','#4be263','#6146ca','#8731c2','#68affc','#4749dc')


chess_data_postdct <- function(grp,grouping='elo',type='simple'){
	# Plots data and postdiction for a single bird
	# 'grp' is a NUMBER between 1:4
	
	# Info extraction 
	if(grouping=='elo'){
		c_dta <- subset(chess,elo_group==grp)
		nds_chess <- nds_chess_elo
	}
	else if(grouping=='experience'){
		c_dta <- subset(chess,experience_group==grp)
		nds_chess <- nds_chess_experience
	}
	alpha <- nds_chess$alpha[,grp]
	beta <- nds_chess$beta[,grp]

	# Posterior predictive	
	n_smpl <- 1000
	x <- seq(-4,4,length.out=50)
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
		plot(NULL,xlim=c(-4,4),ylim=c(-4,4))
	points(log(c_dta$n_wins_QGA/c_dta$n_wins_QGD),
		log(c_dta$n_QGA/c_dta$n_QGD),
		pch=21,bg='#ffffff',lwd=2,cex=2)
	}
	if(type=='complex'){
	nice_scatter(x_plt,y_plt,	
		xlimz=c(-4,4),ylimz=c(-4,4),color=bird_colors[grp],axes=F)
	box()
	points(log(c_dta$n_wins_QGA/c_dta$n_wins_QGD),
		log(c_dta$n_QGA/c_dta$n_QGD),
		pch=21,bg='#33333366',lwd=2,cex=1)
	par(cex.axis=1.25,tck=-0.02,mgp=c(3,0.75,0))
	axis(1);axis(2,las=1)
	abline(0,1,lty='dashed',lwd=2.5,col='#ee0000')
	}
}



chess_data_plot <- function(x_cntr,y_cntr,wdth,hght,m1,m2,...){
	left_lim <- x_cntr-wdth/2+m2
	bottom_lim <- y_cntr-hght/2+m1
	right_lim <- x_cntr+wdth/2
	top_lim <- y_cntr+hght/2
	x_centers <- seq(left_lim,right_lim,length.out=7)[c(2,4,6)]
	y_centers <- seq(bottom_lim,top_lim,length.out=5)[c(2,4)]
	x_centers <- rep(x_centers,each=2)
	y_centers <- rep(sort(y_centers,decreasing=T),3)
	for(bn in 1:4){
		new_plot(which_point='center_center',
			c(x_centers[bn],y_centers[bn]),
			width=0.8*abs(right_lim-left_lim)/3,
			height=0.8*abs(top_lim-bottom_lim)/2,
			paste('pn',bn))
		chess_data_postdct(bn,type='complex',...)
	}
	# X label
	new_plot(which_point='left_top',
		c(left_lim,bottom_lim),
		width=wdth-m2,height=m1)
	text_plot()
	text(0,0,expression(paste(log,'(',W[accepted],'/',W[declined],')')),cex=3,family='CMU Serif')
	# Y label
	new_plot(which_point='right_bottom',
		c(left_lim,bottom_lim),
		width=m2,height=hght-m1)
	text_plot()
	text(0,0,expression(paste(log,'(',B[accepted],'/',B[declined],')')),cex=3,family='CMU Serif',srt=90)
	return(list(left_lim=left_lim,bottom_lim=bottom_lim,
		right_lim=right_lim,top_lim=top_lim,
		x_centers=x_centers,y_centers=y_centers))
}



#bird_joints <- function(grp,type='simple'){
#	# Plots data and postdiction for a single bird
#	# 'grp' is a NUMBER between 1:6
#	
#	# Info extraction 
#	alpha <- nds_chess$alpha[,grp]
#	beta <- nds_chess$beta[,grp]
#
#	# Posterior predictive	
#	n_smpl <- 300
#  indx <- sample(n_smpl,n_smpl)
#	# Plotting
#	if(type=='simple'){
#		plot(NULL,xlim=c(-2,2),ylim=c(-1,3))
#		points(alpha[indx],beta[indx],	
#			pch=21,bg='#ffffff',lwd=2,cex=2)
#	}
#	if(type=='complex'){
#	nice_scatter(alpha,beta,	
#		style='points',n_mids=40,point_scale=2,
#		xlimz=c(-2,2),ylimz=c(-1,3),color=bird_colors[grp],axes=F)
#	box()
#	par(cex.axis=1.25,tck=-0.02,mgp=c(3,0.75,0))
#	axis(1);axis(2,las=1)
#	abline(h=1,lty='dashed',lwd=2.5,col='#ee0000')
#	abline(v=0,lty='dashed',lwd=2.5,col='#ee0000')
#	}
#}


#chess_joints_plot <- function(x_cntr,y_cntr,wdth,hght,m1,m2){
#	left_lim <- x_cntr-wdth/2+m2
#	bottom_lim <- y_cntr-hght/2+m1
#	right_lim <- x_cntr+wdth/2
#	top_lim <- y_cntr+hght/2
#	x_centers <- seq(left_lim,right_lim,length.out=7)[c(2,4,6)]
#	y_centers <- seq(bottom_lim,top_lim,length.out=5)[c(2,4)]
#	x_centers <- rep(x_centers,each=2)
#	y_centers <- rep(sort(y_centers,decreasing=T),3)
#	for(bn in 1:6){
#		new_plot(which_point='center_center',
#			c(x_centers[bn],y_centers[bn]),
#			width=0.8*abs(right_lim-left_lim)/3,
#			height=0.8*abs(top_lim-bottom_lim)/2,
#			paste('pn',bn))
#		bird_joints(bn,type='complex')
#	}
#	# X label
#	new_plot(which_point='left_top',
#		c(left_lim,bottom_lim),
#		width=wdth-m2,height=m1)
#	text_plot()
#	text(0,0,'bias',cex=3,family='CMU Serif')
#	# Y label
#	new_plot(which_point='right_bottom',
#		c(left_lim,bottom_lim),
#		width=m2,height=hght-m1)
#	text_plot()
#	text(0,0,'sensitivity',cex=3,family='CMU Serif',srt=90)
#	return(list(left_lim=left_lim,bottom_lim=bottom_lim,
#		right_lim=right_lim,top_lim=top_lim,
#		x_centers=x_centers,y_centers=y_centers))
#}


chess_marginals_plot <- function(node,...){
    if(node=='alpha'){
        marginal = nds_chess$alpha
				prior = nds_chess$alpha_prior
        label = expression(alpha)
				xlimz = c(-2,2)
				line_at = 0
    }
    else if(node=='beta'){
        marginal = nds_chess$beta
				prior = nds_chess$beta_prior
        label = expression(beta)
				xlimz = c(-1,3)
				line_at = 1
    }
    plot(NULL,xlim=xlimz,...,axes=F,ann=F)
		par(cex.axis=1.5)
		axis(1)
		mtext(label,1,cex=2,line=3)
    hist(prior,breaks=20,plot=F)->ht
    lines(ht$mids,ht$density,lwd=2,lty='dotted')
		for(grp in 1:6){
    	hist(marginal[,grp],breaks=40,plot=F)->ht
    	lines(ht$mids,ht$density,lwd=4,col=bird_colors[grp])
    }
		abline(v=line_at,lwd=2,lty='dashed',col='#ee0000')
}


#try(dev.off())
#x11(width=15.5,height=10.5)
#layout(matrix(1:6,ncol=3))
#chess_data_plot()
