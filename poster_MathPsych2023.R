rm(list=ls())
source('hpmp.R')
source('plots_pigeons.R')
source('plots_chess.R')
library('extrafont')
library('grImport')
library('png')
library('grDevices')

file_name <- 'poster_MathPsych2023.pdf'
start_poster(file_name,
	poster_width=33.1, # A0 inches
	poster_height=46.8, # A0 inches
	margins=c(0.9,1.05,0.9,1.05)) # inches

 
header_height <- 3.5
# UCI Seal
new_plot(which_point='left_top',c(-15.5,22.5),
	width=header_height,height=header_height,
	'UC Logo')
uci_logo <- readPicture('uci_seal.ps.xml')
#plot(0)
grid.picture(uci_logo,
	plt_x_coord(-15.5+header_height/2),
	plt_y_coord(22.5-header_height/2),exp=4)

# Title
new_plot(which_point='left_top',c(-15.5+header_height,22.5),
	width=31-header_height,height=header_height,
	'Poster Title')
#plot(0)
text_plot(axes=F)
text(0,0,'A Bayesian Graphical Model for Matching Behaviors',adj=c(0.5,-.5),cex=5.5,family='CMU Serif',font=2)
text(0,0,'56th Annual Meeting of the Society for Mathematical Psychology, University of Amsterdam, July 18-22, 2023.',adj=c(0.5,-4.5),cex=2.5,family='CMU Serif',font=3)
text(0,0,'Luis Baroja & Joachim Vandekerckhove',adj=c(0.5,1.25),cex=4.5,family='CMU Serif')
text(0,0,'University of California, Irvine. Department of Cognitive Sciences.',adj=c(0.5,4),cex=3,family='CMU Serif',font=3)



# The Matching Law
new_plot(which_point='center_top',c(0,18),
	width=31,height=5,
	'The Matching Law')





text_1 <- readPNG('text_model_1.png')
# Get the image dimensions
img_width <- dim(text_1)[2]
img_height <- dim(text_1)[1]

# Calculate the image position within the plot
xleft <- 0
xright <- img_width
ybottom <- 0
ytop <- img_height

new_plot(which_point='center_top',c(0,18),
	width=9,height=6,
	'The Matching Law')
# Draw the image on the plot
#plot(0,xlim=c(xleft,xright),ylim=c(ybottom,ytop))
#rasterImage(text_1, xleft, ybottom, xright, ytop)



# VI and VR schedules
y_pos_schedules <- 13
new_plot(which_point='left_top',c(0.5,y_pos_schedules),
	width=15,height=5,
	'Response-based matching')
text_plot()
string <- 'Response-based matching'
text_wrapped(0,.8,string,font=2,cex=4)
string <- 'In response-based alternatives, each response has a fixed probability of being rewarded, indepented of time or previous responses and rewards.'
text_wrapped(0,0.4,string,cex=2.5,wdth=80,font=3)

new_plot(which_point='right_top',c(-0.5,y_pos_schedules),
	width=15,height=5,
	'Time-based matching')
text_plot()
string <- 'Time-based matching'
text_wrapped(0,.8,string,font=2,cex=4)
string <- 'In time-based alternatives there is a fixed probability per second of a reward being baited. The next response after baiting collects the reward.'
text_wrapped(0,0.4,string,cex=2.5,wdth=80,font=3)



# VI and VR schedules
y_pos_schedules <- 8
new_plot(which_point='right_top',c(-0.5,y_pos_schedules),
	width=15,height=5,
	'Pigeons dataset')
text_plot()
string <- 'Pigeon Experiment'
text_wrapped(0,.8,string,font=2,cex=3)
string <- c('6 pigeons responded in two time-based alternatives simultaneously available for around 130 days.',
	'One alternative was richer than the other (i.e., the baiting probability per second was higher in one alternative).',
'The target data are the total number of responses to and rewards from each alternative in each experimental session.')
string <- 'Six pigeons responded in two time-based alternatives simultaneously available for approximately 130 daily sessions. In each session one alternative was more rewarding than the other, indicated by a higher baiting probability per second, although which alternative was richer changed across days. The target data include the total number of responses and rewards obtained from each alternative in each experimental session.'
text_wrapped(0,0,string,cex=2.25,adj=0.5,wdth=80)

new_plot(which_point='left_top',c(0.5,y_pos_schedules),
	width=15,height=5,
	'Chess dataset')
text_plot()
string <- 'Chess Dataset'
text_wrapped(0,.8,string,font=2,cex=3)
string <- c('We analized the decisions of the Player with the Black pieces against the Queen\'s Gambit.',
	'Downloaded the Lichess dataset (over 4 billion total games!) and filtered games featuring the Queen\'s Gambit.',
'Black has two main alternatives against the Gambit: either Accept it, or Decline it.')
string <- 'We analyzed the decisions made by the player controlling the Black pieces in response to the Queen\'s Gambit. To conduct this analysis, we downloaded the Lichess dataset, which contains over 4 billion total games, and filtered for games that featured the Queen\'s Gambit. When facing the Gambit, Black has two options: accepting (QGA) it or declining (QGD) it. The target data per player include the number of each of those decisions and the corresponding won games.'
text_wrapped(0,0,string,cex=2.25,adj=0.5,wdth=80)




if(T){
# Pigeon data and post pred
yc <- 3
new_plot(which_point='right_top',c(-0.5,yc),
	width=15,height=10,
	'Pigeon data')
pigeons_data_plot(-8,yc-5,15,10,1,1)

# Pigeon joint posteriors
yc <- -7
new_plot(which_point='right_top',c(-0.5,yc),
	width=15,height=10,
	'Pigeon joints')
pigeons_joints_plot(-8,yc-5,15,10,1,1)

# Pigeon marginal posteriors
# Alpha
new_plot(which_point='center_center',c(-11.5,-15.5),
	width=5,height=4,
	'Alpha marginals')
pigeons_marginals_plot('alpha',ylim=c(0,.9))
# Beta
new_plot(which_point='center_center',c(-4.5,-15.5),
	width=5,height=4,
	'Alpha marginals')
#pigeons_marginals_plot('beta',ylim=c(0,1.1))


# Chess data and post pred
yc <- 3
new_plot(which_point='left_top',c(0.5,yc),
	width=15,height=10,
	'Chess data')
chess_data_plot(8,yc-5,15,10,1,1)

# Chess data and t pred
yc <- -7
new_plot(which_point='left_top',c(0.5,yc),
	width=15,height=10,
	'Chess data')
chess_data_plot(8,yc-5,15,10,1,1,grouping='experience')
}

# Bayesian software validation
#new_plot(which_point='center_center',c(-5,0),
#	width=31,height=3,
#	'Bayesian software validation')

embedFonts(file_name)
end_poster(F,F)
#end_poster(T,T)
#embed_fonts(file_name)

