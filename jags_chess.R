rm(list=ls())
library('R2jags')

chess <- read.csv('queens_gambit_black.csv')

for(gr in c('experience','elo')){
	if(gr=='experience'){
		groups <- chess$experience_group
	}
	else if(gr=='elo'){
		groups <- chess$elo_group
	}
	label_posteriors <- paste('posteriors_chess_',gr,'.RData',sep='')

	priors <- list(mean_alpha = 0.0,
	               sd_alpha   = 1/sqrt(.1),
	               mean_beta  = 0.0,
	               sd_beta    = 1/sqrt(.1),
	               shape_tau  = 2.0,
	               rate_tau   = 0.5)
	
	observed <- list(
	    Br               = chess$n_QGA,
	    Bl               = chess$n_QGD,
	    Wr               = chess$n_wins_QGA,
	    Wl               = chess$n_wins_QGD,
	    #sessions         = sessions,
	    groups            = groups,
	    mean_alpha_prior = priors$mean_alpha,
	    sd_alpha_prior   = priors$sd_alpha,
	    mean_beta_prior  = priors$mean_beta,
	    sd_beta_prior    = priors$sd_beta,
	    shape_tau_prior  = priors$shape_tau,
	    rate_tau_prior   = priors$rate_tau,
	    n_obs            = length(groups),
	    n_groups          = length(unique(groups))
	)
	unobserved <- c('alpha', 'beta','tau',
		'alpha_prior','beta_prior')#,#'tau_prior',
		#'lambda_Br', 'lambda_Bl',
		#'Br_post','Bl_post')
	write(
	    'model{
	
			# Add hierarchical
	
			
	         alpha_prior ~ dnorm(mean_alpha_prior, pow(sd_alpha_prior, -2))T(-3,3)
	         beta_prior  ~ dnorm(mean_beta_prior , pow(sd_beta_prior , -2))T(-3,3)
	         #tau_prior   ~ dgamma(shape_tau_prior, rate_tau_prior)T(0.01,)
	    for(b in 1:n_groups){
	         alpha[b] ~ dnorm(mean_alpha_prior, pow(sd_alpha_prior, -2))T(-3,3)
	         beta[b]  ~ dnorm(mean_beta_prior , pow(sd_beta_prior , -2))T(-3,3)
	         tau[b]   ~ dgamma(shape_tau_prior, rate_tau_prior)T(0.01,)
	    }
	
	    for(i in 1:n_obs){
	             lambda_Br[i] ~ dlnorm( alpha[groups[i]]/2 + beta[groups[i]] * log(Wr[i]/Wl[i])/2, tau[groups[i]])
	             lambda_Bl[i] ~ dlnorm(-alpha[groups[i]]/2 - beta[groups[i]] * log(Wr[i]/Wl[i])/2, tau[groups[i]])
	             Br[i] ~ dpois(lambda_Br[i])
	             Bl[i] ~ dpois(lambda_Bl[i])
	    # Posterior predictive
	             Br_post[i] ~ dpois(lambda_Br[i])
	             Bl_post[i] ~ dpois(lambda_Bl[i])
	         }
	
	    }','matching_chess.bug')
	bayes_chess <- jags(
		data = observed,
		parameters.to.save = unobserved,
		model.file = 'matching_chess.bug',
		n.chains=3,n.iter=6000,n.thin=3,n.burnin=1000)
	unlink('matching_chess.bug')
	print(head(sort(bayes_chess$BUGSoutput$summary[,'Rhat'],decreasing=T),5))
	print(head(sort(bayes_chess$BUGSoutput$summary[,'n.eff'],decreasing=F),5))
	nds_chess <- bayes_chess$BUGSoutput$sims.list
#	save(nds_chess,file='posteriors_chess.RData')
	save(nds_chess,file=label_posteriors)
}
