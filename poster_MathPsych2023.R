rm(list=ls())
source('hpmp.R')
library('extrafont')
library('grImport')

file_name <- 'poster_MathPsych2023.pdf'
start_poster(file_name,
	poster_width=33.1, # A0 inches
	poster_height=46.8, # A0 inches
	margins=c(0.9,1.05,0.9,1.05)) # inches

# HEADER
header_height <- 3.5

new_plot(which_point='left_top',c(-15.5,22.5),
	width=header_height,height=header_height,
	'UC Logo')
uci_logo <- readPicture('uci_seal.ps.xml')
#plot(0)
grid.picture(uci_logo,
	plt_x_coord(-15.5+header_height/2),
	plt_y_coord(22.5-header_height/2),exp=4)

new_plot(which_point='left_top',c(-15.5+header_height,22.5),
	width=31-header_height,height=header_height,
	'Poster Title')
#plot(0)
text_plot(axes=F)
text(0,0,'A Bayesian Graphical Model for Matching Law Behavior',adj=c(0.5,-.5),cex=5.5,family='CMU Serif',font=2)
text(0,0,'56th Annual Meeting of the Society for Mathematical Psychology, University of Amsterdam, July 18-22, 2023',adj=c(0.5,-4.5),cex=2.5,family='CMU Serif',font=3)
text(0,0,'Luis Baroja & Joachim Vandekerckhove',adj=c(0.5,1.25),cex=4.5,family='CMU Serif')
text(0,0,'University of California, Irvine',adj=c(0.5,4),cex=3,family='CMU Serif',font=3)

end_poster(F,F)
#embed_fonts(file_name)
