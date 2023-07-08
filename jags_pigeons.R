library('R2jags')
pigeons <- read.csv('pigeon_data.csv')

# Eliminate 0s (problematic for logs of ratios)
zero_count_rows <-which(pigeons$n_reinf_right==0
                          |pigeons$n_reinf_left==0
                          |pigeons$n_resp_right==0
                          |pigeons$n_resp_left==0)
pigeons <- pigeons[-zero_count_rows,]

# Stable sessions only
pigeons <- pigeons[!pigeons$dynamic_env,]

# Panel-data, reformating sessions and birds to numeric values
sessions <- NA
for(i in 1:nrow(pigeons)){
   sessions[i] <- as.numeric(strsplit(pigeons$session[i],split='s')[[1]][2])
}
birds <- as.numeric(as.factor(pigeons$bird))

priors <- list(mean_alpha = 0.0,
               sd_alpha   = 1/sqrt(.1),
               mean_beta  = 0.0,
               sd_beta    = 1/sqrt(.1),
               shape_tau  = 2.0,
               rate_tau   = 0.5)

observed <- list(
    Br               = pigeons$n_resp_right,
    Bl               = pigeons$n_resp_left,
    Wr               = pigeons$n_reinf_right,
    Wl               = pigeons$n_reinf_left,
    #sessions         = sessions,
    birds            = birds,
    mean_alpha_prior = priors$mean_alpha,
    sd_alpha_prior   = priors$sd_alpha,
    mean_beta_prior  = priors$mean_beta,
    sd_beta_prior    = priors$sd_beta,
    shape_tau_prior  = priors$shape_tau,
    rate_tau_prior   = priors$rate_tau,
    n_obs            = length(sessions),
    n_birds          = length(unique(birds))
)
unobserved <- c('alpha', 'beta', 'tau', 'lambda_Br', 'lambda_Bl','log_B_post','Br_post','Bl_post')
write(
    'model{

		# Add hierarchical

    for(b in 1:n_birds){
         alpha[b] ~ dnorm(mean_alpha_prior, pow(sd_alpha_prior, -2))T(-2,2)
         beta[b]  ~ dnorm(mean_beta_prior , pow(sd_beta_prior , -2))T(-2,2)
         tau[b]   ~ dgamma(shape_tau_prior, rate_tau_prior)T(0.01,)
    }

    for(i in 1:n_obs){
             lambda_Br[i] ~ dlnorm( alpha[birds[i]]/2 + beta[birds[i]] * log(Wr[i])/2, tau[birds[i]])
             lambda_Bl[i] ~ dlnorm(-alpha[birds[i]]/2 - beta[birds[i]] * log(Wl[i])/2, tau[birds[i]])
             Br[i] ~ dpois(lambda_Br[i])
             Bl[i] ~ dpois(lambda_Bl[i])
    # Posterior predictive
             Br_post[i] ~ dpois(lambda_Br[i])
             Bl_post[i] ~ dpois(lambda_Bl[i])
             #Br_post[i,birds[i]] ~ dpois(lambda_Br[i])
             #Bl_post[i,birds[i]] ~ dpois(lambda_Bl[i])
           #  log_B_post[i] <- log(Br_post[i,birds[i]]/Bl_post[i,birsd[i]])
         }

    }','matching_pigeons.bug')
bayes_pigeons <- jags(
	data = observed,
	parameters.to.save = unobserved,
	model.file = 'matching_pigeons.bug')
unlink('matching_pigeons.bug')
print(head(sort(bayes_pigeons$BUGSoutput$summary[,'Rhat'],decreasing=T),5))
print(head(sort(bayes_pigeons$BUGSoutput$summary[,'n.eff'],decreasing=F),5))
nds_pigeons <- bayes_pigeons$BUGSoutput$sims.list
save(nds_pigeons,file='posteriors_pigeons.RData')
