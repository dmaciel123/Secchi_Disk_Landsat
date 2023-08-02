## Secchi models for Landsat data ###
## All cases should work for Landsat 5, 7, 8 and 9. 
## Bands used will be blue, green, red and nir


## Secchi model from Jiang et al. (2019)
secchi_jian2019 = function(kd, bbp, rrs, abso) {
  
  sechi_est = NULL
  wv = NULL
  rrs_x = rrs
  kd_lee = kd
  
  
  for(i in 1:nrow(kd_lee)) {
    
    data = data.frame(kd = t(kd_lee[i,]),
                      rrs = t(rrs_x[i,]),
                      abs = t(abso[i,]),
                      bb = t(bbp[i,]),
                      wave = c(1:4))
      

    names(data) <- c("kd", "rrs","ABS", "bb", "wave")
    
    min_kd <- data %>% filter(min(data$kd, na.rm =T) == kd)
    
    u = min_kd$bb/(min_kd$ABS+min_kd$bb)
    
    ratio = 1.04*(1+5.4*u)^0.5/(1/(1-(sin(30)^2)/1.34^2)^0.5)
    
    result = 1/((1+ratio)*min_kd$kd)*log(abs(0.14-(min_kd$rrs))/0.013)
    
    print(i)
    
    if(length(result) > 0) {
        sechi_est[i] <- result
    
        wv[i] = min_kd$wave
    }  else { 
      
      sechi_est[i] <- NA
      
      wv[i] = NA
      
      }
  }
  
  
  return(sechi_est)
  
}

#Secchi Model from Lee et al. (2015)
secchi_lee2015 = function(kd, bbp, rrs, abso) {
  
  sechi_est = NULL
  wv = NULL
  rrs_x = rrs
  kd_lee = kd
  
  
  for(i in 1:nrow(kd_lee)) {
    
    
    data = data.frame(kd = t(kd_lee[i,]),
                      rrs = t(rrs_x[i,]),
                      abs = t(abso[i,]),
                      bb = t(bbp[i,]),
                      wave = c(1:4))
    
    
    names(data) <- c("kd", "rrs","ABS", "bb", "wave")
    
    min_kd <- data %>% filter(min(data$kd, na.rm =T) == kd)
    
    result <- 1/(2.5*min_kd$kd)*log(abs(0.14-(min_kd$rrs))/0.013)
    WAVE = min_kd$wave
    
    print(i)
    if(length(result) > 0) {
      sechi_est[i] <- result
      
      wv[i] = WAVE
    }  else { 
      
      sechi_est[i] <- NA
      
      wv[i] = NA
      
    }

  }
  
  results = list(SECCHI = sechi_est,
                 Wave = wv)
  
  return(results)
  
}

### IOPS #####

#Maciel et al. 2020 function 

iops_maciel2020 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  
  #Empirical estimative of absorption at 560 nm (idea from Liu et al. (2019) - doi:10.3390/rs11192226)
  
  
  a560 <- NULL
  
  for(i in 1:length(rrs_a[2,])) {
    
    
    if(rrs_a[5,i] < 0.000002) {
      
      a560[i] = 0.062 + 1.3 * (rrs_a[2,i]/(rrs_a[4,i]+rrs_a[5,i]))^-2.1 
    }
    
    else{    
      
      a560[i] = 0.062 + 1.3 * (rrs_a[2,i]/(rrs_a[4,i]+rrs_a[5,i]))^-2.1
      
    }
  }
  
  #Estimatinga anw680 - Wang et al
  
  #a560 = 0.9398*(rrs_a[3,]/rrs_a[2,])^2 + 0.865 * rrs_a[3,]/rrs_a[2,] - 0.0852
  
  #Coeficiens of g0p (l) and g1p (m) taken from Lin et al. (2018) - doi 10.1364/oe.26.00a157
  #here we use coefficients for teta s = 15°, teta v = 40° and azimute = 130
  
  j = 0.0614
  k = 0.0524
  l = 0.0425
  m = 0.1408
  
  w = water.abs.bb[2,3]
  
  r = rrs_a[2,]
  
  
  #Here i consider that bbw is zero
  bbp.l0 = ((-2*r*a560 + m*a560 + l*a560 - a560*sqrt(l^2+m^2+2*l*m+4*r))/(2*(r-m-1-l)))
  
  
  p1 = ((-2*r*a560 + m*a560 + l*a560 + k*w+j*w))
  
  p2 = sqrt(l^2*a560^2+m^2*a560^2+2*l*m*a560^2+4*r*a560^2-2*k*m*w*a560-2*m*j*w*a560-2*k*l*w*a560-2*l*j*w*a560-4*k*w*a560-4*j*w*a560+k^2*w^2+2*k*j*w^2+j^2*w^2+4*r*w^2-4*m*w^2-4*w^2-4*l*w^2)
  
  p3 = 2*(r-m-1-l)
  
  bbp.l0 = (p1-p2)/p3
  
  
  
  #B = log(u[350,]/u[380,])
  
  #slope.bb <- 2.5*(1-1.30*exp(-0.9*rrs.sub[275,]/rrs.sub[155,]))
  #slope.bb_TURBID <- -372.99*B^2+37.286*B + 0.84
  
  slp.qaa.mod = 2.5*(1-1.3*exp(-1.5*rrs_a[3,]/rrs_a[2,]))
  
  
  bbp <- matrix(nrow = 8, ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp.l0*(560/wave[i])^slp.qaa.mod
    
    
  }
  
  
  #step 6
  
  #abs = (1-u)*(water.abs.bb$bb+bbp)/u
  
  w = water.abs.bb[,3]
  
  abs = (-2*rrs_a*bbp+m*bbp+l*bbp+bbp*sqrt(l^2+m^2+2*l*m+4*rrs_a))/(2*rrs_a)
  
  p1a = -2*rrs_a*bbp+m*bbp+l*bbp+k*w+j*w
  
  p2a = sqrt(l^2*bbp^2+m^2*bbp^2+2*l*m*bbp^2+4*rrs_a*bbp^2-2*k*m*w*bbp-2*m*j*w*bbp-2*k*l*w*bbp-2*l*j*w*bbp-4*k*w*bbp-4*j*w*bbp+k^2*w^2+2*k*j*w^2+j^2*w^2+4*rrs_a*w^2-4*m*w^2-4*w^2-4*l*w^2)
  
  p3a = 2*rrs_a
  
  abs = (p1a + p2a)/p3a
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}



iops_shi_wang <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs/(0.52+1.7*rrs)
  
  
  u = (-0.089 + sqrt(0.089^2+4*0.0125*rrs_a))/(2*0.125)
  
  bb_lambda_745 = water.abs.bb$Abs[5]*u[5,]/(1-u[5,])
  bb_lambda_862 = water.abs.bb$Abs[8]*u[8,]/(1-u[8,])
  
  #Empirical estimative of absorption at 560 nm (idea from Liu et al. (2019) - doi:10.3390/rs11192226)
  
  N = log(bb_lambda_745/bb_lambda_862)/log(745/862)
  
  bbp <- matrix(nrow = 8, ncol = ncol(N))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bb_lambda_745*(745/wave[i])^N
    
    
  }
  
  abs = (1- u)*(water.abs.bb$bb+bbp)/u
  
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}



iops_jiang20 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs/(0.52+1.7*rrs)
  
  u = (-0.089 + sqrt(0.089^2+4*0.0125*rrs_a))/(2*0.125)
  
  bb_lambda_745 = water.abs.bb$Abs[5]*u[5,]/(1-u[5,])

  #Empirical estimative of absorption at 560 nm (idea from Liu et al. (2019) - doi:10.3390/rs11192226)
  
  N = log(u[5,]/u[6,])
  U = -372.99*N^2+37.286*N+0.84
  
  bbp <- matrix(nrow = 8, ncol = ncol(N))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bb_lambda_745*(745/wave[i])^U
    
    
  }
  
  abs = (1- u)*(water.abs.bb$bb+bbp)/u
  
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}


iops_qaav5 <- function(rrs) {
  
  aw = c(0.005,0.011,0.064,0.368)
  bbw = c(0.0021,0.0014,0.0008,0.0004)
  
  wave = c(442, 484, 554, 654)
  
  rrs_a <- rrs/(0.52+1.7*rrs)
  
  u = (-0.089 + sqrt(0.089^2+4*0.0125*rrs_a))/(2*0.125)
  
  XX = log((rrs_a$B1+rrs_a$B2)/(rrs_a$B3+5*rrs_a$B4/rrs_a$B2*rrs_a$B4))
  
  alambda0 = aw[3] + 10^(-1.146-1.366*XX-0.469*XX^2)
  
  bb_lambda_0 = alambda0*u$B3/(1-u$B3)-bbw[3]
  
  N = 2*(1-1.2*exp(-0.9*rrs_a$B1/rrs_a$B3))

  bbp <- matrix(ncol = 4, nrow = length(N))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[,i] <- bb_lambda_0*(550/wave[i])^N
    
    
  }
  
  abs = (1- data.frame(u))*(bbw+bbp)/data.frame(u)
  
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}

iops_qaav6 <- function(rrs) {
  
  aw = c(0.005,0.011,0.064,0.368)
  bbw = c(0.0021,0.0014,0.0008,0.0004)
  
  wave = c(442, 484, 554, 654)
  rrs_a <- rrs/(0.52+1.7*rrs)
  
  u = (-0.089 + sqrt(0.089^2+4*0.0125*rrs_a))/(2*0.125)
  
  #bb_lambda_745 = water.abs.bb$Abs[5]*u[5,]/(1-u[5,])
  
  alambda0 = aw[3] + 0.39*(rrs$B4/(rrs$B1+rrs$B2))^1.14
  
  bb_lambda_0 = alambda0*u$B4/(1-u$B4)-bbw[4]
  
  N = 2*(1-1.2*exp(-0.9*rrs_a$B1/rrs_a$B3))
  
  bbp <- matrix(ncol = 4, nrow = length(N))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[,i] <- bb_lambda_0*(654/wave[i])^N
    
    
  }
  
  abs = (1- data.frame(u))*(bbw+bbp)/data.frame(u)
  
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}




iops_maciel2020_Landsat8 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  
  #Empirical estimative of absorption at 560 nm (idea from Liu et al. (2019) - doi:10.3390/rs11192226)
  
  
  a560 <- NULL
  
  for(i in 1:length(rrs_a[2,])) {
    
    
    if(rrs_a[2,i] < 0.000002) {
      
      a560[i] = 0.062 + 1.3 * (rrs_a[2,i]/(rrs_a[3,i]+rrs_a[4,i]))^-3 
    }
    
    else{    
      
      a560[i] = 0.062 + 1.3 * (rrs_a[2,i]/(rrs_a[3,i]+rrs_a[4,i]))^-3
      
    }
  }
  
  #Estimatinga anw680 - Wang et al
  
  #a560 = 0.9398*(rrs_a[3,]/rrs_a[2,])^2 + 0.865 * rrs_a[3,]/rrs_a[2,] - 0.0852
  
  #Coeficiens of g0p (l) and g1p (m) taken from Lin et al. (2018) - doi 10.1364/oe.26.00a157
  #here we use coefficients for teta s = 15°, teta v = 40° and azimute = 130
  
  j = 0.0614
  k = 0.0524
  l = 0.0425
  m = 0.1408
  
  w = water.abs.bb[2,3]
  
  r = rrs_a[2,]
  
  
  #Here i consider that bbw is zero
  bbp.l0 = ((-2*r*a560 + m*a560 + l*a560 - a560*sqrt(l^2+m^2+2*l*m+4*r))/(2*(r-m-1-l)))
  
  
  p1 = ((-2*r*a560 + m*a560 + l*a560 + k*w+j*w))
  
  p2 = sqrt(l^2*a560^2+m^2*a560^2+2*l*m*a560^2+4*r*a560^2-2*k*m*w*a560-2*m*j*w*a560-2*k*l*w*a560-2*l*j*w*a560-4*k*w*a560-4*j*w*a560+k^2*w^2+2*k*j*w^2+j^2*w^2+4*r*w^2-4*m*w^2-4*w^2-4*l*w^2)
  
  p3 = 2*(r-m-1-l)
  
  bbp.l0 = (p1-p2)/p3
  
  
  
  #B = log(u[350,]/u[380,])
  
  #slope.bb <- 2.5*(1-1.30*exp(-0.9*rrs.sub[275,]/rrs.sub[155,]))
  #slope.bb_TURBID <- -372.99*B^2+37.286*B + 0.84
  
  slp.qaa.mod = 2.5*(1-1.3*exp(-1.5*rrs_a[3,]/rrs_a[2,]))
  
  
  bbp <- matrix(nrow = 4, ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp.l0*(560/wave[i])^slp.qaa.mod
    
    
  }
  
  
  #step 6
  
  #abs = (1-u)*(water.abs.bb$bb+bbp)/u
  
  w = water.abs.bb[,3]
  
  abs = (-2*rrs_a*bbp+m*bbp+l*bbp+bbp*sqrt(l^2+m^2+2*l*m+4*rrs_a))/(2*rrs_a)
  
  p1a = -2*rrs_a*bbp+m*bbp+l*bbp+k*w+j*w
  
  p2a = sqrt(l^2*bbp^2+m^2*bbp^2+2*l*m*bbp^2+4*rrs_a*bbp^2-2*k*m*w*bbp-2*m*j*w*bbp-2*k*l*w*bbp-2*l*j*w*bbp-4*k*w*bbp-4*j*w*bbp+k^2*w^2+2*k*j*w^2+j^2*w^2+4*rrs_a*w^2-4*m*w^2-4*w^2-4*l*w^2)
  
  p3a = 2*rrs_a
  
  abs = (p1a + p2a)/p3a
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}


#Rodrigues et al. 2017 (recalibrated) 

iops_rodrigues2017 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.125
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  
  XX = log((0.5*rrs_sub[1,]+rrs_sub[3,])/(rrs_sub[2,]+0.005*rrs_sub[3,]/rrs_sub[1,]*rrs_sub[3,]))
  
  a560 = 0.062 + 10 ^ (-0.7153-1.8*XX-2*XX^2)
  
  #a560 = 0.062 + 1.3 * (rrs_a[2,]/(rrs_a[4,]+rrs_a[5,]))^-2.1 
  
  
  bbp560 = u_lambda[2,]*a560/(1-u_lambda[2,])-water.abs.bb[2,3]
  
  
  slp.qaa.mod = 2.2*(1-1.2*exp(-0.9*rrs_sub[1,]/rrs_sub[2,]))
  
  
  bbp <- matrix(nrow = 8, ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp560*(560/wave[i])^slp.qaa.mod
    
    
  }
  
  
  abs = (1-u_lambda)*(water.abs.bb[,3]+bbp/u_lambda)
  

  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}

iops_watanabe16 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.125
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  
  XX = log((rrs_a[2,]+rrs_a[3,])/(rrs_a[4,]+5*rrs_a[3,]/rrs_a[1,]*rrs_a[3,]))
  
  a709 = 0.71 + 10 ^ (-0.7702-0.0999*XX-0.0566*XX^2)
  
  #a560 = 0.062 + 1.3 * (rrs_a[2,]/(rrs_a[4,]+rrs_a[5,]))^-2.1 
  
  
  bbp709 = u_lambda[1,]*a709/(1-u_lambda[2,])-water.abs.bb[2,3]
  
  
  slp.qaa.mod = 2*(1-1.2*exp(-0.9*rrs_sub[1,]/rrs_sub[2,]))
  
  
  bbp <- matrix(nrow = 8, ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp709*(709/wave[i])^slp.qaa.mod
    
    
  }
  
  
  abs = (1-u_lambda)*(water.abs.bb[,3]+bbp/u_lambda)
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}

iops_bernardo2019 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.125
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  
  XX = log((rrs_sub[1,]+rrs_sub[2,])/(rrs_sub[3,]+5*rrs_sub[3,]^2/rrs_sub[1,]))
  
  a660 = 0.062 + 10 ^ (-1.146-1.366*XX+(-0.469*XX^2))
  
  #a560 = 0.062 + 1.3 * (rrs_a[2,]/(rrs_a[4,]+rrs_a[5,]))^-2.1 
  
  
  bbp660 = u_lambda[2,]*a660/(1-u_lambda[2,])-water.abs.bb[2,3]
  
  
  slp.qaa.mod = 2*(1-1.2*exp(-0.9*rrs_sub[1,]/rrs_sub[2,]))
  
  
  bbp <- matrix(nrow = 8, ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp660*(660/wave[i])^slp.qaa.mod
    
    
  }
  
  
  abs = (1-u_lambda)*bbp/u_lambda
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}

iops_curtarelli <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.1245
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  

  a560 = 0.062 + 0.43*(rrs_a[2,]/(rrs_a[3,]+rrs_a[4,]))^(-1.44)
  

  
  bbp560 = u_lambda[2,]*a560/(1-u_lambda[2,])-water.abs.bb[2,3]
  
  
  slp.qaa.mod = 0.5248 * exp(rrs_sub[3,]/rrs_sub[4,])
  
  
  bbp <- matrix(nrow = 8, ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp560*(560/wave[i])^slp.qaa.mod
    
    
  }
  
  
  abs = (1-u_lambda)*(water.abs.bb[,3]+bbp/u_lambda)
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}

iops_curtarelli2 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.1245
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  

  a560 = 0.062 + 1.3*(rrs_a[2,]/(rrs_a[4,]+rrs_a[5,]))^(-2.1)
  
  
  
  bbp560 = u_lambda[2,]*a560/(1-u_lambda[2,])-water.abs.bb[2,3]
  
  
  slp.qaa.mod = 0.5248 * exp(rrs_sub[3,]/rrs_sub[4,])
  
  
  bbp <- matrix(nrow = 8, ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp560*(560/wave[i])^slp.qaa.mod
    
    
  }
  
  
  abs = (1-u_lambda)*(water.abs.bb[,3]+bbp/u_lambda)
  
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  
  return(dados_finais)
  
}



## Secchi and Kd calculation ######

secchi_calculation <- function(abs,bbp, water.abs.bb, teta_s, rrs.t) {
  
  
  
  ################ KD CALCUATION ##################
  
  m0 = 0.005
  m1 = 4.26
  m2 = 0.52
  m3 = 10.54
  chi = 0.265
  teta_s = teta_s
  
  raz_bbw_bb = water.abs.bb$bb/(bbp+water.abs.bb$bb)
  
  bb = bbp+water.abs.bb$bb
  
  kd_lee2013 = (1+m0*teta_s)*abs + (1-chi*raz_bbw_bb)*m1 * (1 - m2 *exp(-m3*abs))*(bb)
  
  dados_finais = list(abs, bb, kd_lee2013)
  
  names(dados_finais) <- c("ABS", "BB", "KD_LEE2013_s2a")
  
  SECCHI_jian2019 = secchi_jian2019(kd = dados_finais$KD_LEE2013_s2a,
                                       bbp = dados_finais$BB, 
                                       abso = dados_finais$ABS, 
                                       rrs = rrs.t)
  SECCHI_lee2015= secchi_lee2015(kd = dados_finais$KD_LEE2013_s2a,
                                       bbp = dados_finais$BB, 
                                       abso = dados_finais$ABS, 
                                       rrs = rrs.t)
  
  
  res = data.frame(jian2019 = SECCHI_jian2019, lee2015 = SECCHI_lee2015)
  
  
  return(res)
  
}

secchi_calculation_landsat8 <- function(ABS, bbp, teta_s, rrs.t) {
  
  
  aw = c(0.005,0.011,0.064,0.368)
  bbw = c(0.0021,0.0014,0.0008,0.0004)
  
  wave = c(442, 484, 554, 654)

  
  ################ KD CALCUATION ##################
  
  m0 = 0.005
  m1 = 4.26
  m2 = 0.52
  m3 = 10.54
  chi = 0.265
  teta_s = teta_s
  
  raz_bbw_bb = bbw/(bbp+bbw)
  
  bb = bbp+bbw
  
  kd_lee2013 = (1+m0*teta_s)*ABS + (1-chi*raz_bbw_bb)*m1 * (1 - m2 *exp(-m3*ABS))*(bb)
  
  dados_finais = list(ABS, bb, kd_lee2013)
  
  names(dados_finais) <- c("ABS", "BB", "Kd_lee")
  
  SECCHI_jian2019 = secchi_jian2019(kd = dados_finais$Kd_lee,
                                    bbp = dados_finais$BB, 
                                    abso = dados_finais$ABS, 
                                    rrs = rrs.t)
  
  SECCHI_lee2015= secchi_lee2015(kd = dados_finais$Kd_lee,
                                 bbp = dados_finais$BB, 
                                 abso = dados_finais$ABS, 
                                 rrs = rrs.t)
  
  
  res = data.frame(jian2019 = SECCHI_jian2019, lee2015 = SECCHI_lee2015)
  
  
  return(res)
  
}

secchi_calculation_yin <- function(ABS, bbp, teta_s, rrs.t) {
  
  #https://doi.org/10.1016/j.jag.2021.102457
  aw = c(0.005,0.011,0.064,0.368)
  bbw = c(0.0021,0.0014,0.0008,0.0004)
  
  wave = c(442, 484, 554, 654)
  
  
  ################ KD CALCUATION ##################
  
  m0 = 0.005
  m1 = 4.26
  m2 = 0.52
  m3 = 10.54
  chi = 0.265
  teta_s = teta_s
  
  raz_bbw_bb = bbw/(bbp+bbw)
  
  bb = bbp+bbw
  
  kd_lee2013 = (1+m0*teta_s)*ABS + (1-chi*raz_bbw_bb)*m1 * (1 - m2 *exp(-m3*ABS))*(bb)
  
  dados_finais = list(ABS, bb, kd_lee2013)
  
  names(dados_finais) <- c("ABS", "BB", "Kd_lee")
  

  
  SECCHI_lee2015= secchi_lee2015(kd = dados_finais$Kd_lee,
                                 bbp = dados_finais$BB, 
                                 abso = dados_finais$ABS, 
                                 rrs = rrs.t)
  
  
    
  
  return(SECCHI_lee2015)
  
}


index_calc = function(blue, green, red) {
  
  
  green_red = green/red
  blue_green = blue/green
  blue_red = blue/red
  
  C = (665-560)/(665-490)
  LH =  (green-C*blue-(1-C)*red)
  
  res = data.frame(green_red, blue_green,blue_red, LH)
  
  return(res)
  
  
}



index_calc_bind = function(df,blue, green, red) {
  
  
  green_red = green/red
  blue_green = blue/green
  blue_red = blue/red
  
  C = (665-560)/(665-490)
  LH =  (green-C*blue-(1-C)*red)
  
  res = data.frame(green_red, blue_green,blue_red, LH)
  
  res = cbind(df, res)
  return(res)
  
  
}


