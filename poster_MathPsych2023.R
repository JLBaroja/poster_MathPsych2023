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
text(0,0,'A Bayesian Graphical Model for Matching Law Behavior',adj=c(0.5,-.5),cex=5.5,font=2)
text(0,0,'56th Annual Meeting of the Society for Mathematical Psychology, University of Amsterdam, July 18-22, 2023.',adj=c(0.5,-4.5),cex=2.5,font=3)
text(0,0,'Luis Baroja & Joachim Vandekerckhove',adj=c(0.5,1.25),cex=4.25)
text(0,0,'University of California, Irvine. Department of Cognitive Sciences.',adj=c(0.5,4),cex=2.75,font=3)


# The Matching Law
square(which_point='left_top',coordinates=c(-15.5,18.5),
	width=31,height=6.5)
square(which_point='center_center',coordinates=c(0,18.5),
	width=6.25,height=1,text='The Matching Law',cex.text=4,font.text=2,line=0)
yc <- 18
insert_png('model_1.png',c(-10.25,yc),wdth=9.5)
insert_png('model_2.png',c(0,yc),wdth=9.5)
insert_png('chess_jl.png',c(10.25,yc),wdth=10.25)


# VI and VR schedules
y_pos_schedules <- 10.75
square(which_point='left_top',c(0.5,y_pos_schedules+0.5),
	width=15,height=5.25)
square(which_point='center_center',coordinates=c(7.75,y_pos_schedules+0.5),
	width=7,height=1,text='Response-based matching',cex.text=3,font.text=2,line=0)
new_plot(which_point='left_top',c(0.5,y_pos_schedules),
	width=15,height=1,
	'Response-based matching')
text_plot()
string <- 'In response-based alternatives each response has a fixed probability of being rewarded, independent of time or previous responses and rewards.'
text_wrapped(0,0,string,cex=2.5,wdth=80,font=3)

square(which_point='right_top',c(-0.5,y_pos_schedules+0.5),
	width=15,height=5.25)
square(which_point='center_center',coordinates=c(-7.75,y_pos_schedules+0.5),
	width=5.75,height=1,text='Time-based matching',cex.text=3,font.text=2,line=0)
new_plot(which_point='right_top',c(-0.5,y_pos_schedules),
	width=15,height=1,
	'Time-based matching')
text_plot()
string <- 'In time-based alternatives there is a fixed probability per second of a reward being baited. The next response after baiting collects the reward.'
text_wrapped(0,0,string,cex=2.5,wdth=80,font=3)


# VI and VR schedules
y_pos_schedules <- 9.75
new_plot(which_point='right_top',c(-0.5,y_pos_schedules),
	width=15,height=4.25,
	'Pigeons dataset')
text_plot()
string <- 'Pigeon Experiment'
text_wrapped(0,.75,string,font=2,cex=2.5)
string <- 'Six pigeons responded in two time-based alternatives simultaneously available for approximately 130 daily sessions. In each session one alternative was more rewarding than the other, indicated by a higher baiting probability per second, although which alternative was richer changed across days. The target data include the total number of responses and rewards obtained from each alternative in each experimental session.'
text_wrapped(0,0,string,cex=2.25,adj=0.5,wdth=80)

new_plot(which_point='left_top',c(0.5,y_pos_schedules),
	width=15,height=4.25,
	'Chess dataset')
text_plot()
string <- 'Chess Dataset'
text_wrapped(0,.75,string,font=2,cex=2.5)
string <- 'We analyzed the decisions made by the player controlling the Black pieces in response to the Queen\'s Gambit (QG). For this analysis, we worked with the lichess.org dataset, which contains over 4 billion games, and filtered for openings that featured the QG (only one month is analized below). When facing the QG, Black has two options: accepting it or declining it. The target data per player are the number of each of those decisions and the corresponding victories.'
text_wrapped(0,0,string,cex=2.25,adj=0.5,wdth=85)




if(T){

# Pigeon data and post pred
yc <- 5.5
square(which_point='right_top',c(-0.5,yc),
	width=15,height=10,line=3)
square(which_point='left_center',c(-15,yc),
	width=6,height=.8,line=0,text='Pigeon data and posterior predictive',
	cex.text=2,font.text=2)
pigeons_data_plot(-8,yc-5,15,10,1,1)

# Pigeon joint posteriors
yc <- -5
square(which_point='right_top',c(-0.5,yc),
	width=15,height=10,line=3)
square(which_point='left_center',c(-15,yc),
	width=4.75,height=.8,line=0,text='Joint posterior distributions',
	cex.text=2,font.text=2)
pigeons_joints_plot(-8,yc-5,15,10,1,1)
}

if(T){
# Chess data and post pred
yc <- 5.5
square(which_point='left_top',c(0.5,yc),
	width=15,height=10,line=3)
square(which_point='right_center',c(15,yc),
	width=5.25,height=.8,line=0,text='Grouping by player level (ELO)',
	cex.text=2,font.text=2)
chess_data_plot(8,yc-5,15,10,1,1,grouping='elo')

yc <- -5
square(which_point='left_top',c(0.5,yc),
	width=15,height=10,line=3)
square(which_point='right_center',c(15,yc),
	width=5.25,height=.8,line=0,text='Grouping by player experience',
	cex.text=2,font.text=2)
chess_data_plot(8,yc-5,15,10,1,1,grouping='experience')
}



yc <- -15.75
# Pigeons summary
square(which_point='left_top',c(-15.5,yc),
	width=9,height=5.5,line=4)
square(which_point='left_center',c(-15,yc),
	width=4,height=.8,line=0,text='Pigeons summary',
	cex.text=2.5,font.text=2)
new_plot(which_point='left_top',c(-15.5,yc),
	width=9,height=5.5)
text_plot()
tx_wdth <- 55
tx_cex <- 2
string <- 'The distribution of pigeon responses among the two alternatives is influenced by the distribution of rewards obtained from them, albeit high individual variability and systematic deviations from the matching equilibrium.'
text_wrapped(0,.45,string,cex=tx_cex,adj=0.5,wdth=tx_wdth)
string <- 'Most pigeons clearly undermatch, with only one individual showing sensitivity consistent with strict matching. This may reflect a potentially over-abundant environment.'
text_wrapped(0,-.25,string,cex=tx_cex,adj=0.5,wdth=tx_wdth)
string <- 'However, all birds are unbiased and show no systematic preference for any alternative.'
text_wrapped(0,-.75,string,cex=tx_cex,adj=0.5,wdth=tx_wdth)


# Chess summary
square(which_point='right_top',c(15.5,yc),
	width=9,height=5.5,line=4)
square(which_point='right_center',c(15,yc),
	width=3.5,height=.8,line=0,text='Chess summary',
	cex.text=2.5,font.text=2)
new_plot(which_point='right_top',c(15.5,yc),
	width=9,height=5.5)
text_plot()
tx_wdth <- 55
tx_cex <- 2
string <- 'The matching relationship reasonably describes choices by Black against the QG, although certain deviations from the equilibrium appear systematic.'
text_wrapped(0,.6,string,cex=tx_cex,adj=0.5,wdth=tx_wdth)
string <- 'Players of all levels are oversensitive to each won game. Moreover, lower rated players are unbiased, but higher rated players prefer declining the gambit beyond the matching prediction.'
text_wrapped(0,0.0250,string,cex=tx_cex,adj=0.5,wdth=tx_wdth)
string <- 'Inexperienced and highly experienced players are also biased towards declining, although over-sensitivity to each won game systematically diminishes as experience against the gambit accumulates.'
text_wrapped(0,-.6,string,cex=tx_cex,adj=0.5,wdth=tx_wdth)

# General conclusions
square(which_point='center_top',c(0,yc),
	width=11.5,height=4,line=4)
square(which_point='center_center',c(0,yc),
	width=5.25,height=.8,line=0,text='General Conclusions',
	cex.text=3,font.text=2)
new_plot(which_point='center_top',c(0,yc),
	width=11.5,height=4)
text_plot()
string <- 'The matching equilibrium is a robust phenomenon that arises under different types of environmental constrains.'
text_wrapped(0,.6,string,cex=2,adj=0.5,wdth=75)
string <- 'However, there is considerable variability across individuals, and certain systematic relationships between the matching parameters and other variables appear evident.'
text_wrapped(0,0.05,string,cex=2,adj=0.5,wdth=75)
string <- 'Future steps will develop hiearchical extensions and explanatory modeling to better characterize the relationship between matching parameters and different covariates.'
text_wrapped(0,-.6,string,cex=2,adj=0.5,wdth=75)

# Funding
square(which_point='center_bottom',c(0,-21.25),
	width=11.5,height=1,line=4)
square(which_point='center_center',c(0,-20.25),
	width=2,height=.8,line=0,text='Funding',
	cex.text=2.5,font.text=2)
new_plot(which_point='center_bottom',c(0,-21.25),
	width=11.5,height=1)
text_plot()
string <- 'This work was supported by NSF grants #1850849 and #2051186.'
text_wrapped(0,-.1,string,cex=2,wdth=80)

# URL
square(which_point='center_bottom',c(0,-22.5),
	width=31,height=.75,box.lty='32')
new_plot(which_point='center_bottom',c(0,-22.5),
	width=31,height=.75)
text_plot(axes=F)
text(0,0,'https://github.com/JLBaroja/poster_MathPsych2023',font=2,cex=2)
text(-.95,0,'jbaroja@uci.edu',adj=0,font=2,cex=2)
text(.95,0,'https://cidlab.com',adj=1,font=2,cex=2)

embedFonts(file_name)
end_poster(F,F)
#end_poster(T,F)
#embed_fonts(file_name)

