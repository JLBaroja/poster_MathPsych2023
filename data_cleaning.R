rm(list=ls())
pigeons <- read.csv('matching_by_session.csv')

# Eliminate 0s (problematic for logs of ratios)
zero_count_rows <-which(pigeons$n_reinf_right==0
                          |pigeons$n_reinf_left==0
                          |pigeons$n_resp_right==0
                          |pigeons$n_resp_left==0)
pigeons <- pigeons[-zero_count_rows,]

# Stable sessions only
pigeons <- pigeons[!pigeons$dynamic_env,]

birds <- as.numeric(as.factor(pigeons$bird))
pigeons$bird_num <- birds
write.csv(pigeons,file='pigeon_data.csv',row.names=F)
