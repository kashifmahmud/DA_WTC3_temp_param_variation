# # Setting lower and upper bounds of the prior parameter pdf, and starting point of the chain
# Try wide priors
#-------------------------------------------------------------------------------------
param.Y <- matrix(c(0.2,0.3,0.4) , nrow=1, ncol=3, byrow=T)
param.af <- matrix(c(0,0.25,0.4) , nrow=1, ncol=3, byrow=T)
param.as <- matrix(c(0.4,0.6,0.8) , nrow=1, ncol=3, byrow=T)
param.sf <- matrix(c(0,0.001,0.002) , nrow=1, ncol=3, byrow=T)
param.sr <- matrix(c(0,0.01,0.02) , nrow=1, ncol=3, byrow=T)

# param.Y = matrix(c(0.2,0.3,0.4) , nrow=1, ncol=3, byrow=T)
# param.af = matrix(c(0,0.2,1) , nrow=1, ncol=3, byrow=T)
# param.as = matrix(c(0,0.5,1) , nrow=1, ncol=3, byrow=T)
# param.sf = matrix(c(0,0.01,0.02) , nrow=1, ncol=3, byrow=T)
# param.sr = matrix(c(0,0.01,0.02) , nrow=1, ncol=3, byrow=T)

# # initialize 'sf' prior differently for grouped treatments
# if (v[[1]]==1 || v[[1]]==2 || v[[1]]==3) {
#   param.sf <- matrix(c(0,0.01,0.02) , nrow=1, ncol=3, byrow=T) # Group 1
# } else if (v[[1]]==4 || v[[1]]==5 || v[[1]]==6) {
#   param.sf <- matrix(c(0,0.005,0.01) , nrow=1, ncol=3, byrow=T) # Group 2
# } else if (v[[1]]==7) {
#   param.sf <- matrix(c(0,0.0025,0.005) , nrow=1, ncol=3, byrow=T) # Free seedling
# }

# # initialize 'sf' prior differently for grouped treatments
# if (v[[1]]==1 || v[[1]]==2 || v[[1]]==3) {
#   param.sf <- matrix(c(0,0.0125,0.025) , nrow=1, ncol=3, byrow=T) # Group 1
# } else if (v[[1]]==4 || v[[1]]==5 || v[[1]]==6) {
#   param.sf <- matrix(c(0,0.0075,0.015) , nrow=1, ncol=3, byrow=T) # Group 2
# } else if (v[[1]]==7) {
#   param.sf <- matrix(c(0,0.005,0.01) , nrow=1, ncol=3, byrow=T) # Free seedling
# }

# if (length(vol.group)==1) {
#   param.sf <- matrix(c(0,0.01,0.02) , nrow=1, ncol=3, byrow=T) # 1 Group
# } else if (length(vol.group)==2) {
#   if (v1==1) {
#     param.sf <- matrix(c(0,0.01,0.02) , nrow=1, ncol=3, byrow=T) # Group 1
#   } else if (v1==2) {
#     param.sf <- matrix(c(0,0.0025,0.005) , nrow=1, ncol=3, byrow=T) # Group 2
#   }
# } else if (length(vol.group)==3) {
#   if (v1==1) {
#     param.sf <- matrix(c(0,0.01,0.02) , nrow=1, ncol=3, byrow=T) # Group 1
#   } else if (v1==2) {
#     param.sf <- matrix(c(0,0.005,0.01) , nrow=1, ncol=3, byrow=T) # Group 2
#   } else if (v1==3) {
#     param.sf <- matrix(c(0,0.0025,0.005) , nrow=1, ncol=3, byrow=T) # Free seedling
#   }
# } else if (length(vol.group)>3) {
#   if (v[[1]]==1 || v[[1]]==2 || v[[1]]==3) {
#     param.sf <- matrix(c(0,0.01,0.02) , nrow=1, ncol=3, byrow=T) # Group 1
#   } else if (v[[1]]==4 || v[[1]]==5 || v[[1]]==6) {
#     param.sf <- matrix(c(0,0.005,0.01) , nrow=1, ncol=3, byrow=T) # Group 2
#   } else if (v[[1]]==7) {
#     param.sf <- matrix(c(0,0.0025,0.005) , nrow=1, ncol=3, byrow=T) # Free seedling
#   }
# }

#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
if (no.param > 1) {
  param.Y <- rbind(param.Y, c(-(param.Y[3]-param.Y[1])/2/nrow(data), 0, (param.Y[3]-param.Y[1])/2/nrow(data)))
  param.af1 <- param.af2 <- rbind(param.af, c(-(param.af[3]-param.af[1])/nrow(data), 0, (param.af[3]-param.af[1])/nrow(data)))
  param.as1 <- param.as2 <- rbind(param.as, c(-(param.as[3]-param.as[1])/2/nrow(data), 0, (param.as[3]-param.as[1])/2/nrow(data)))
  param.sf1 <- param.sf2 <- rbind(param.sf, c(-(param.sf[3]-param.sf[1])/nrow(data), 0, (param.sf[3]-param.sf[1])/nrow(data)))
  param.sr1 <- param.sr2 <- rbind(param.sr, c(-(param.sr[3]-param.sr[1])/nrow(data), 0, (param.sr[3]-param.sr[1])/nrow(data)))
}

if (no.param > 2) {
  param.Y <- rbind(param.Y, c((param.Y[1,1]-param.Y[1,3]-param.Y[2,3]*nrow(data))/(2*nrow(data)^2), 0, (param.Y[1,3]-param.Y[1,1]-param.Y[2,1]*nrow(data))/(2*nrow(data)^2)))
  param.af1 <- param.af2 <- rbind(param.af, c((param.af[1,1]-param.af[1,3]-param.af[2,3]*nrow(data))/(2*nrow(data)^2), 0, (param.af[1,3]-param.af[1,1]-param.af[2,1]*nrow(data))/(nrow(data)^2)))
  param.as1 <- param.as2 <- rbind(param.as, c((param.as[1,1]-param.as[1,3]-param.as[2,3]*nrow(data))/(2*nrow(data)^2), 0, (param.as[1,3]-param.as[1,1]-param.as[2,1]*nrow(data))/(2*nrow(data)^2)))
  param.sf1 <- param.sf2 <- rbind(param.sf, c((param.sf[1,1]-param.sf[1,3]-param.sf[2,3]*nrow(data))/(2*nrow(data)^2), 0, (param.sf[1,3]-param.sf[1,1]-param.sf[2,1]*nrow(data))/(nrow(data)^2)))
  param.sr1 <- param.sr2 <- rbind(param.sr, c((param.sr[1,1]-param.sr[1,3]-param.sr[2,3]*nrow(data))/(2*nrow(data)^2), 0, (param.sr[1,3]-param.sr[1,1]-param.sr[2,1]*nrow(data))/(nrow(data)^2)))
}
if (no.param > 3) {
  param.Y <- rbind(param.Y, c((param.Y[1,1]-param.Y[1,3]-param.Y[2,3]*nrow(data)-param.Y[3,3]*(nrow(data)^2))/(2*nrow(data)^3), 0, (param.Y[1,3]-param.Y[1,1]-param.Y[2,1]*nrow(data)-param.Y[3,1]*(nrow(data)^2))/(2*nrow(data)^3)))
  param.af1 <- param.af2 <- rbind(param.af, c((param.af[1,1]-param.af[1,3]-param.af[2,3]*nrow(data)-param.af[3,3]*(nrow(data)^2))/(2*nrow(data)^3), 0, (param.af[1,3]-param.af[1,1]-param.af[2,1]*nrow(data)-param.af[3,1]*(nrow(data)^2))/(2*nrow(data)^3)))
  param.as1 <- param.as2 <- rbind(param.as, c((param.as[1,1]-param.as[1,3]-param.as[2,3]*nrow(data)-param.as[3,3]*(nrow(data)^2))/(2*nrow(data)^3), 0, (param.as[1,3]-param.as[1,1]-param.as[2,1]*nrow(data)-param.as[3,1]*(nrow(data)^2))/(2*nrow(data)^3)))
  param.sf1 <- param.sf2 <- rbind(param.sf, c((param.sf[1,1]-param.sf[1,3]-param.sf[2,3]*nrow(data)-param.sf[3,3]*(nrow(data)^2))/(2*nrow(data)^3), 0, (param.sf[1,3]-param.sf[1,1]-param.sf[2,1]*nrow(data)-param.sf[3,1]*(nrow(data)^2))/(2*nrow(data)^3)))
  param.sr1 <- param.sr2 <- rbind(param.sr, c((param.sr[1,1]-param.sr[1,3]-param.sr[2,3]*nrow(data)-param.sr[3,3]*(nrow(data)^2))/(2*nrow(data)^3), 0, (param.sr[1,3]-param.sr[1,1]-param.sr[2,1]*nrow(data)-param.sr[3,1]*(nrow(data)^2))/(2*nrow(data)^3)))
}
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
if (with.storage==F) {
  param = data.frame(param.Y,param.af1,param.as1,param.sf1,param.sr1,param.af2,param.as2,param.sf2,param.sr2)
  names(param) <- c("Y_min","Y","Y_max","af1_min","af1","af1_max","as1_min","as1","as1_max","sf1_min","sf1","sf1_max","sr1_min","sr1","sr1_max",
                    "af2_min","af2","af2_max","as2_min","as2","as2_max","sf2_min","sf2","sf2_max","sr2_min","sr2","sr2_max")
  pMinima <- param[ ,c("Y_min","af1_min","as1_min","sf1_min","sr1_min","af2_min","as2_min","sf2_min","sr2_min")]
  pMaxima <- param[ ,c("Y_max","af1_max","as1_max","sf1_max","sr1_max","af2_max","as2_max","sf2_max","sr2_max")]
  pValues <- param[ ,c("Y","af1","as1","sf1","sr1","af2","as2","sf2","sr2")] # Starting point of the chain
} else { # (with.storage==T) 
  # param.k <- matrix(c(0,0.25,1) , nrow=1, ncol=3, byrow=T)
  param.k <- matrix(c(0,0.15,0.3) , nrow=1, ncol=3, byrow=T)
  
  if (no.param > 1) {
    param.k <- rbind(param.k, c(-(param.k[3]-param.k[1])/nrow(data), 0, (param.k[3]-param.k[1])/nrow(data)))
  } 
  if (no.param > 2) {
    param.k <- rbind(param.k, c((param.k[1,1]-param.k[1,3]-param.k[2,3]*nrow(data))/(nrow(data)^2), 0, (param.k[1,3]-param.k[1,1]-param.k[2,1]*nrow(data))/(nrow(data)^2)))
  }
  if (no.param > 3) {
    param.k <- rbind(param.k, c((param.k[1,1]-param.k[1,3]-param.k[2,3]*nrow(data)-param.k[3,3]*(nrow(data)^2))/(2*nrow(data)^3), 0, (param.k[1,3]-param.k[1,1]-param.k[2,1]*nrow(data)-param.k[3,1]*(nrow(data)^2))/(2*nrow(data)^3)))
  }
  param = data.frame(param.k,param.Y,param.af1,param.as1,param.sf1,param.sr1,param.af2,param.as2,param.sf2,param.sr2)
  names(param) <- c("k_min","k","k_max","Y_min","Y","Y_max","af1_min","af1","af1_max","as1_min","as1","as1_max","sf1_min","sf1","sf1_max","sr1_min","sr1","sr1_max",
                    "af2_min","af2","af2_max","as2_min","as2","as2_max","sf2_min","sf2","sf2_max","sr2_min","sr2","sr2_max")
  pMinima <- param[ ,c("k_min","Y_min","af1_min","as1_min","sf1_min","sr1_min","af2_min","as2_min","sf2_min","sr2_min")]
  pMaxima <- param[ ,c("k_max","Y_max","af1_max","as1_max","sf1_max","sr1_max","af2_max","as2_max","sf2_max","sr2_max")]
  pValues <- param[ ,c("k","Y","af1","as1","sf1","sr1","af2","as2","sf2","sr2")] # Starting point of the chain
}
pChain <- matrix(0, nrow=chainLength, ncol = no.param*no.var+1) # Initialising the chain
#-------------------------------------------------------------------------------------

