source('plotting_functions.R')
#library('grImport')

# Load pigeon data and model results (obtained with 'jags_chess.R')
chess <- read.csv('queens_gambit_black.csv')
load('posteriors_chess_elo.RData')
nds_chess_elo <- nds_chess
load('posteriors_chess_experience.RData')
nds_chess_experience <- nds_chess
rm('nds_chess')

palette_elo <- c('#e66101','#fdb863','#b2abd2','#5e3c99')
#palette_elo <- c('#b3cde3','#8c96c6','#8856a7','#810f7c')
palette_experience <- c('#a6611a','#dfc27d','#80cdc1','#018571')
#palette_elo <- c('#b3cde3','#8c96c6','#8856a7','#810f7c')
#palette_elo <- rep('#00EE00',4)
#palette_experience <- rep('#00ee00',4)

chess_data_postdct <- function(grp,grouping,type='simple'){
	# Plots data and postdiction for a single bird
	# 'grp' is a NUMBER between 1:4
	
	# Info extraction 
	if(grouping=='elo'){
		c_dta <- subset(chess,elo_group==grp)
		nds_chess <- nds_chess_elo
		palette_chess <- palette_elo
	}
	else if(grouping=='experience'){
		c_dta <- subset(chess,experience_group==grp)
		nds_chess <- nds_chess_experience
		palette_chess <- palette_experience
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
		xlimz=c(-4,4),ylimz=c(-4,4),color=palette_chess[grp],axes=F)
	box()
	points(log(c_dta$n_wins_QGA/c_dta$n_wins_QGD),
		log(c_dta$n_QGA/c_dta$n_QGD),
		#pch=21,bg='#ffffff66',lwd=2,cex=1.25)
		pch=16,lwd=2,cex=1)
	par(cex.axis=1.5,tck=-0.02,mgp=c(3,0.75,0))
	axis(1);axis(2,las=1)
	abline(0,1,lty='dashed',lwd=2.5,col='#ee0000')
	text(3.5,3.5,'matching',cex=1,srt=45,adj=c(1,-0.5),col='#ee0000')
	}
}



chess_data_plot <- function(x_cntr,y_cntr,wdth,hght,m1,m2,grouping){
	left_lim <- x_cntr-wdth/2+m2
	bottom_lim <- y_cntr-hght/2+m1
	right_lim <- x_cntr+wdth/2
	top_lim <- y_cntr+hght/2
	x_centers <- seq(left_lim,right_lim,length.out=7)[c(2,4,6)]
	y_centers <- seq(bottom_lim,top_lim,length.out=5)[c(2,4)]
	x_centers <- rep(x_centers,each=2)
	y_centers <- rep(sort(y_centers,decreasing=T),3)
  plt_labs <- NULL
	if(grouping=='elo'){
		palette_ch <- palette_elo
		for(i in 1:4){
			qt <- c(min(chess$avg_elo[chess$elo_group==i]),max(chess$avg_elo[chess$elo_group==i]))
			plabel <- paste('ELO between\n',qt[1],'and',qt[2],collapse='')
			plt_labs <- append(plt_labs,plabel)
		}
	}
	else if(grouping=='experience'){
		palette_ch <- palette_experience
		for(i in 1:4){
			qt <- c(min(chess$total_QG[chess$experience_group==i]),max(chess$total_QG[chess$experience_group==i]))
			plabel <- paste('Between\n',qt[1],'and',qt[2],'\nQG games',collapse='')
			plt_labs <- append(plt_labs,plabel)
		}
	}
	for(bn in 1:6){
	new_plot(which_point='center_center',
			c(x_centers[bn],y_centers[bn]),
			width=0.8*abs(right_lim-left_lim)/3,
			height=0.8*abs(top_lim-bottom_lim)/2,
			paste('pn',bn))
		if(bn<=4){
			chess_data_postdct(bn,type='complex',grouping=grouping)
			text(-2.5,3.75,plt_labs[bn],col=palette_ch[bn],font=2,adj=c(0.5,1))
		}
		else if(bn==5){
		new_plot(which_point='center_center',
				c(x_centers[bn],y_centers[bn]),
				width=0.8*abs(right_lim-left_lim)/3,
				height=0.6*abs(top_lim-bottom_lim)/2,
				paste('pn',bn))
			if(grouping=='elo'){ylimz=c(0,5)}
			else if(grouping=='experience'){ylimz=c(0,8)}
			chess_marginals_plot('alpha',grouping=grouping,ylimz=ylimz)
		}
		else if(bn==6){
		new_plot(which_point='center_center',
				c(x_centers[bn],y_centers[bn]),
				width=0.8*abs(right_lim-left_lim)/3,
				height=0.6*abs(top_lim-bottom_lim)/2,
				paste('pn',bn))
			if(grouping=='elo'){ylimz=c(0,5.5)}
			else if(grouping=='experience'){ylimz=c(0,5)}
			chess_marginals_plot('beta',grouping=grouping,ylimz=ylimz)
		}
	}
	

	# X label
	new_plot(which_point='left_top',
		c(left_lim,bottom_lim),
		width=(wdth-m2)*2/3,height=m1)
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


chess_marginals_plot <- function(node,grouping,ylimz=c(0,6)){
		if(grouping=='elo'){
			nds_chess=nds_chess_elo
			palette_chess=palette_elo
		}	
		if(grouping=='experience'){
			nds_chess=nds_chess_experience
			palette_chess=palette_experience
		}	
    if(node=='alpha'){
        marginal = nds_chess$alpha
				prior = nds_chess$alpha_prior
        #label = expression(alpha)
        label = 'Bias'
				xlimz = c(-1,1)
				line_at = 0
				text_left = 'bias towards\ngambit DECLINED'
				text_right =	'bias towards\ngambit ACCEPTED'
    }
    else if(node=='beta'){
        marginal = nds_chess$beta
				prior = nds_chess$beta_prior
        #label = expression(beta)
        label = 'Sensitivity'
				xlimz = c(0,2)
				line_at = 1
				text_left = 'undermatching'
				text_right = 'overmatching'
    }
    plot(NULL,xlim=xlimz,ylim=ylimz,axes=F,ann=F)
		par(cex.axis=1.5)
		axis(1)
		text(xlimz[1]+.5,ylimz[2]*.9,text_left,col='#ee0000aa')
		text(xlimz[2]-.5,ylimz[2]*.9,text_right,col='#ee0000aa')
		mtext(label,1,cex=2,line=3)
    hist(prior,breaks=20,plot=F)->ht
    lines(ht$mids,ht$density,lwd=2,lty='dotted')
		for(grp in 1:4){
    	hist(marginal[,grp],breaks=40,plot=F)->ht
    	lines(ht$mids,ht$density,lwd=4,col=palette_chess[grp])
    }
		abline(v=line_at,lwd=2,lty='dashed',col='#ee0000')
}

