rm(list=ls())
source('hpmp.R')
source('plots_pigeons.R')
source('plots_chess.R')
library('extrafont')
library('grImport')

file_name <- 'poster_MathPsych2023.pdf'
start_poster(file_name,
	poster_width=33.1, # A0 inches
	poster_height=46.8, # A0 inches
	margins=c(0.9,1.05,0.9,1.05)) # inches

# HEADER
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


# VI and VR schedules
y_pos_schedules <- 13
new_plot(which_point='left_top',c(0.5,y_pos_schedules),
	width=15,height=5,
	'Response-based matching')
new_plot(which_point='right_top',c(-0.5,y_pos_schedules),
	width=15,height=5,
	'Time-based matching')

# Pigeon data and post pred
new_plot(which_point='right_top',c(-0.5,7),
	width=15,height=10,
	'Pigeon data')
pigeons_data_plot(-8,2,15,10,1,1)

# Pigeon joint posteriors
new_plot(which_point='right_top',c(-0.5,-3),
	width=15,height=10,
	'Pigeon joints')
pigeons_joints_plot(-8,-8,15,10,1,1)

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
pigeons_marginals_plot('beta',ylim=c(0,1.1))


# Chess data and post pred
new_plot(which_point='left_top',c(0.5,7),
	width=15,height=10,
	'Chess data')
chess_data_plot(8,2,15,10,1,1)

# Chess data and t pred
new_plot(which_point='left_top',c(0.5,-3),
	width=15,height=10,
	'Chess data')
chess_data_plot(8,-8,15,10,1,1,grouping='experience')


# Bayesian software validation
#new_plot(which_point='center_center',c(-5,0),
#	width=31,height=3,
#	'Bayesian software validation')

end_poster(F,F)
#end_poster(T,T)
#embed_fonts(file_name)
