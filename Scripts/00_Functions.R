### IOPS #####

#Maciel et al. 2020 function 

iops_maciel2020 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs 
  
  
  #Empirical estimative of absorption at 560 nm (idea from Liu et al. (2019) - doi:10.3390/rs11192226)
  
  

  
  a560 = 0.062 + 1.3 * (rrs_a[160,]/(rrs_a[305,]+rrs_a[340,]))^-2.1
      

  
  #Estimatinga anw680 - Wang et al
  
  #a560 = 0.9398*(rrs_a[3,]/rrs_a[2,])^2 + 0.865 * rrs_a[3,]/rrs_a[2,] - 0.0852
  
  #Coeficiens of g0p (l) and g1p (m) taken from Lin et al. (2018) - doi 10.1364/oe.26.00a157
  #here we use coefficients for teta s = 15°, teta v = 40° and azimute = 130
  
  j = 0.0614
  k = 0.0524
  l = 0.0425
  m = 0.1408
  
  w = water.abs.bb[160,3]
  
  r = rrs_a[160,]
  
  
  #Here i consider that bbw is zero
  bbp.l0 = ((-2*r*a560 + m*a560 + l*a560 - a560*sqrt(l^2+m^2+2*l*m+4*r))/(2*(r-m-1-l)))
  
  
  p1 = ((-2*r*a560 + m*a560 + l*a560 + k*w+j*w))
  
  p2 = sqrt(l^2*a560^2+m^2*a560^2+2*l*m*a560^2+4*r*a560^2-2*k*m*w*a560-2*m*j*w*a560-2*k*l*w*a560-2*l*j*w*a560-4*k*w*a560-4*j*w*a560+k^2*w^2+2*k*j*w^2+j^2*w^2+4*r*w^2-4*m*w^2-4*w^2-4*l*w^2)
  
  p3 = 2*(r-m-1-l)
  
  bbp.l0 = (p1-p2)/p3
  
  
  
  #B = log(u[350,]/u[380,])
  
  #slope.bb <- 2.5*(1-1.30*exp(-0.9*rrs.sub[275,]/rrs.sub[155,]))
  #slope.bb_TURBID <- -372.99*B^2+37.286*B + 0.84
  
  slp.qaa.mod = 2.5*(1-1.3*exp(-1.5*rrs_a[260,]/rrs_a[160,]))
  
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp.l0*(560/wave[i])^slp.qaa.mod
    
    print(i)
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


iops_jiang20 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs/(0.52+1.7*rrs)
  
  u = (-0.089 + sqrt(0.089^2+4*0.125*rrs_a))/(2*0.125)
  
  bb_lambda_745 = water.abs.bb$Abs[345]*u[345,]/(1-u[345,])

  N = log(u[345,]/u[375,])
  U = -372.99*N^2+37.286*N+0.84
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(N))
  
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


iops_qaav5 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs/(0.52+1.7*rrs)
  
  u = (-0.089 + sqrt(0.089^2+4*0.0125*rrs_a))/(2*0.125)
  
  #bb_lambda_745 = water.abs.bb$Abs[5]*u[5,]/(1-u[5,])
  
  XX = log((rrs_a[90,]+rrs_a[90,])/(rrs_a[160,]+5*rrs_a[260,]/rrs_a[90,]*rrs_a[260,]))
  
  alambda0 = water.abs.bb$Abs[160] + 10^(-1.146-1.366*XX-0.469*XX^2)
  
  bb_lambda_0 = alambda0*u[160,]/(1-u[160,])
  
  N = 2*(1-1.2*exp(-0.9*rrs_a[90,]/rrs_a[160,]))

  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(N))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bb_lambda_0*(550/wave[i])^N
    
  print(i)  
  
  }
  
  abs = (1- u)*(water.abs.bb$bb+bbp)/u
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}


iops_qaav6 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs/(0.52+1.7*rrs)
  
  u = (-0.089 + sqrt(0.089^2+4*0.125*rrs_a))/(2*0.125)
  
  #bb_lambda_745 = water.abs.bb$Abs[5]*u[5,]/(1-u[5,])
  
  alambda0 = water.abs.bb$Abs[270] + 0.39*(rrs[270,]/(rrs[43,]+rrs[90,]))^1.14
  
  bb_lambda_0 = alambda0*u[270,]/(1-u[270,])
  
  N = 2*(1-1.2*exp(-0.9*rrs_a[43,]/rrs_a[155,]))
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = length(N))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bb_lambda_0*(660/wave[i])^N
    
    
  }
  
  abs = (1- u)*(water.abs.bb$bb+bbp)/u
  
  
  
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
  
  XX = log((rrs_a[160,]+rrs_a[260,])/(rrs_a[305,]+5*rrs_a[260,]/rrs_a[90,]*rrs_a[260,]))
  
  a709 = 0.71 + 10 ^ (-0.7702-0.0999*XX-0.0566*XX^2)
  
  #a560 = 0.062 + 1.3 * (rrs_a[2,]/(rrs_a[4,]+rrs_a[5,]))^-2.1 
  
  
  bbp709 = u_lambda[90,]*a709/(1-u_lambda[160,])-water.abs.bb[160,3]
  
  
  slp.qaa.mod = 2*(1-1.2*exp(-0.9*rrs_sub[90,]/rrs_sub[160,]))
  
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(slp.qaa.mod))
  
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

  a560 = 0.062 + 0.43*(rrs_a[160,]/(rrs_a[260,]+rrs_a[305,]))^(-1.44)
  
  bbp560 = u_lambda[160,]*a560/(1-u_lambda[160,])-water.abs.bb[160,3]
  
  slp.qaa.mod = 0.5248 * exp(rrs_sub[260,]/rrs_sub[305,])
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    

    bbp[i,] <- bbp560*(560/wave[i])^slp.qaa.mod
    
    print(paste("Getting backscaterring coefficient for band: ", wave[i]))
  }
  
  
  #abs = (1-u_lambda)*(water.abs.bb[,3]+bbp)/u_lambda
  
  abs = (1- u_lambda)*(water.abs.bb$bb+bbp)/u_lambda
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}



opwt_01 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.1245
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  
  X = log((rrs_a[43,]+rrs_a[90,])/(rrs_a[160,]+5*(rrs_a[265,]/rrs_a[90,])+rrs_a[265,]))
  
  a560 = 0.062 +10^(-1.146-1.366*X-0.469*X^2)
  
  bbp560 = u_lambda[160,]*a560/(1-u_lambda[160,])-water.abs.bb[160,3]
  
  slp.qaa.mod = 0.5248 * exp(rrs_sub[260,]/rrs_sub[305,])
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp560*(560/wave[i])^slp.qaa.mod
    
    print(paste("Getting backscaterring coefficient for band: ", wave[i]))
  }
  
  
  #abs = (1-u_lambda)*(water.abs.bb[,3]+bbp)/u_lambda
  
  abs = (1- u_lambda)*(water.abs.bb$bb+bbp)/u_lambda
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}

opwt_02 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.1245
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  
  a670 = 0.40 + 0.39*(rrs_a[270,]/(rrs_a[43,]+rrs_a[90,]))^1.14
  
  bbp670 = u_lambda[270,]*a670/(1-u_lambda[270,])-water.abs.bb[270,3]
  
  slp.qaa.mod = 0.5248 * exp(rrs_sub[260,]/rrs_sub[305,])
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp670*(670/wave[i])^slp.qaa.mod
    
    print(paste("Getting backscaterring coefficient for band: ", wave[i]))
  }
  
  
  #abs = (1-u_lambda)*(water.abs.bb[,3]+bbp)/u_lambda
  
  abs = (1- u_lambda)*(water.abs.bb$bb+bbp)/u_lambda
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}


opwt_03 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.1245
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  
  a750 = water.abs.bb[340,2]
  
  bbp750 = u_lambda[340,]*a750/(1-u_lambda[340,])-water.abs.bb[340,3]
  
  slp.qaa.mod = 0.5248 * exp(rrs_sub[260,]/rrs_sub[305,])
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp750*(750/wave[i])^slp.qaa.mod
    
    print(paste("Getting backscaterring coefficient for band: ", wave[i]))
  }
  
  

  abs = (1- u_lambda)*(water.abs.bb$bb+bbp)/u_lambda
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}


opwt_04 <- function(rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.1245
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  
  a850 = water.abs.bb[400,2]
  
  bbp850 = u_lambda[400,]*a850/(1-u_lambda[400,])-water.abs.bb[400,3]
  
  slp.qaa.mod = 0.5248 * exp(rrs_sub[260,]/rrs_sub[305,])
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    
    
    bbp[i,] <- bbp850*(800/wave[i])^slp.qaa.mod
    
    print(paste("Getting backscaterring coefficient for band: ", wave[i]))
  }
  
  
  
  abs = (1- u_lambda)*(water.abs.bb$bb+bbp)/u_lambda
  
  dados_finais = list(abs, bbp)
  
  names(dados_finais) <- c("ABS", "BB")
  return(dados_finais)
  
  
}

iops_optimize <- function(g0, g1, rrs, water.abs.bb, wave) {
  
  rrs_a <- rrs
  
  rrs_sub = rrs_a/(0.52+1.7*rrs_a)
  
  g1 = 0.089
  g2 = 0.1245
  
  u_lambda = (-g1+sqrt(g1^2+4*g2*rrs_sub))/(2*g2)
  
  #We know:
  
  #Rrs (400-800)
  #Must know: at and bbp

  #From Balasubramanian et al. (2020), Rrs could be considered as:
  
  F_Q = 0.105 #f/Q factor
  
  Rrs = F_Q*(bbp)
  
  
  
  
  
  a560 = 0.062 + 0.43*(rrs_a[160,]/(rrs_a[260,]+rrs_a[305,]))^(-1.44)
  
  bbp560 = u_lambda[160,]*a560/(1-u_lambda[160,])-water.abs.bb[160,3]
  
  slp.qaa.mod = 0.5248 * exp(rrs_sub[260,]/rrs_sub[305,])
  
  bbp <- matrix(nrow = nrow(rrs_a), ncol = ncol(slp.qaa.mod))
  
  bbp = data.frame(bbp)
  
  wave = wave
  
  for(i in 1:length(wave)) {
    

    bbp[i,] <- bbp560*(560/wave[i])^slp.qaa.mod
    
    print(paste("Getting backscaterring coefficient for band: ", wave[i]))
  }
  
  
  #abs = (1-u_lambda)*(water.abs.bb[,3]+bbp)/u_lambda
  
  abs = (1- u_lambda)*(water.abs.bb$bb+bbp)/u_lambda
  
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



## estatiticas ####



estatisicas = function(real, estimado) {
  
  require(Metrics)
  
  df = data.frame(real = real, estimado = estimado) %>% na.omit() %>% dplyr::filter(real > 0 & estimado > 0 & estimado < 1000)
  
  Y = log10(df$estimado/df$real)
  
  E = 100* (10^median(abs(Y))-1)
  BIAS_log_perc = 100* sign(median(Y))*(10^abs(median(Y))-1)
  
  bias = 10^mean(log10(df$estimado)-log10(df$real))
  
  MAPE = mean(abs((df$real-df$estimado)/df$real))*100
  
  MaE = 10^median(abs(log10(df$estimado)-log10(df$real)))
  
  pearSon = cor(df$real, df$estimado)
  
  RMSLE = rmsle(actual = df$real, predicted =df$estimado)
  N = nrow(df)
  SLOPE = lm(log(estimado)~log(real), data = df)$coefficients[2]
  

  
  resultados = data.frame(BIAS = bias, 
                          BIAS_perc = BIAS_log_perc,
                          MAPE = MAPE,
                          MAE = MaE,
                          R = pearSon, 
                          N = N, 
                          E = E,
                          RMSLE = RMSLE, 
                          slope = SLOPE)
  
  return(resultados)
  
}


statistics_separator = function(estimado, medido, separador) {
  
  df3 = data.frame(est = estimado, 
                   measured = medido, 
                   separador = separador) %>% na.omit() %>% filter(measured > 0 & est > 0 & est< 10)
  
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  

  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median(Y))*(10^(abs(median(Y)))-1))
  
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  
  resultados = data.frame(Separador = MAPE_Sumarrise_$separador, 
                          MAPE = MAPE_Sumarrise_$MAPES,
                          BIAS = BIAS_Sumarrise_$BIAS, 
                          N = N_Sumarrise_$N, 
                          R = cor_summarise$R2,
                          bias2 = bias2_summarise$BIAS2,
                          E = E_summarise$E)
  
  return(resultados)
  
  
  
  
  
}

rrs_interpolation_g0_g01_052_17 = function(g0, g1, ABS, BBP, rrs) {

rrs_new = data.frame(matrix(ncol = ncol(rrs), nrow = 501))
u = BBP/(ABS+BBP)

rrs_RES = (g0+g1*u)*u

Rrs_RES = 0.52*rrs_RES/(1-1.7*rrs_RES)


resultado = data.frame(Wave = 1:ncol(BBP), r2 = 1, mape = 1, rmse = 1)
rrs.800_900_predicted = rrs[1:101,]


for(k in 1:ncol(BBP)) {
  

  df = data.frame(bbp = BBP[[k]][1:300], WV = c(400:699))
  
  if(is.na(df$bbp[1]) == F) {
    
  SLOPE = nls(formula = bbp~bbp[160]*(560/WV)^X, data = df, start = list(X = 1))
  
  bbp = data.frame(WV = c(700:900), BBP =1)
  
  for(i in 1:nrow(bbp)) {
    
    bbp[i,2] <- BBP[[k]][160]*(560/bbp$WV[i])^summary(SLOPE)$coefficients[1]
    
  }
  
  #Plot Bbp
  plot(BBP[[k]], ylim = c(0, max(BBP[[k]])), x = c(400:800),  xlim = c(400,900),
       type = 'l', xlab = "Wavelength", ylab = expression(b[bp]), lwd = 3)
  par(new = T)
  plot(bbp$BBP, x = c(700:900), ylim = c(0, max(BBP[[k]])), xlab = '', ylab = '',
       xlim = c(400,900), type = 'l', col = 'red', lwd = 3)
  
  
  #Absoprtion
  
  dfabs = data.frame(WV = c(400:699), abs = (ABS[[k]][1:300])-water.abs.bb$Abs[1:300])
  
  SLOPE = nls(formula = abs~abs[160]*(560/WV)^X, data = dfabs, start = list(X = 1))
  
  ABS_800_900 = data.frame(WV = c(700:900), ABS =1)
  
  for(i in 1:nrow(ABS_800_900)) {
    
    ABS_800_900[i,2] <- ABS[[k]][160]*(560/ABS_800_900$WV[i])^summary(SLOPE)$coefficients[1]+water.abs.bb$Abs[i+300]
    
  }
  
  #Plot anw
  plot(ABS[[k]][1:300], ylim = c(0, max(ABS[[k]][1:300])), x = c(400:699),  xlim = c(400,900),
       type = 'l', xlab = "Wavelength", ylab = expression(a[t]), lwd = 3)
  par(new = T)
  plot(ABS_800_900$ABS, x = c(700:900), ylim = c(0, max(ABS[[k]][1:300])), xlab = '', ylab = '',
       xlim = c(400,900), type = 'l', col = 'red', lwd = 3)
  

  u = bbp$BBP/(ABS_800_900$ABS+bbp$BBP)
  
  rrs_RES = (g0+g1*u)*u
  
  Rrs_RES = (0.52*rrs_RES/(1-1.7*rrs_RES))[-c(1:100)]
  
  Rrs_RES = Rrs_RES + (rrs[[k]][400]-Rrs_RES[1])
  
  joined = c(rrs[[k]][1:400], Rrs_RES)
  plot(joined[1:400], x = c(400:799), xlab ='', ylab = '', xlim = c(400,900), ylim = c(0, max(joined)), type = 'l', col = 'black', lwd = 2)
  par(new=T)
  plot(joined[401:501], x = c(800:900), xlim = c(400,900), ylim = c(0, max(joined)), type = 'l', col = 'red', lwd = 2,
       xlab = "Wavelength", ylab = "Rrs")
  
  rrs_new[,k] = joined[1:501]
  
  print(k)   
  
}

}

return(rrs_new)

}

rrs_interpolation_g0_g01_052_17_optimized = function(g0, g1, ABS, BBP, rrs) {

rrs_new = data.frame(matrix(ncol = ncol(rrs), nrow = 500))
u = BBP/(ABS+BBP)

rrs_RES = (g0+g1*u)*u

Rrs_RES = 0.52*rrs_RES/(1-1.7*rrs_RES)


resultado = data.frame(Wave = 1:ncol(BBP), r2 = 1, mape = 1, rmse = 1)
rrs.800_900_predicted = rrs[1:101,]


for(k in 1:ncol(BBP)) {
  
  DIF_percentage = 10
  
  while(DIF_percentage > 5) {
    
  df = data.frame(bbp = BBP[[k]][1:400], WV = c(400:799))
  SLOPE = nls(formula = bbp~bbp[160]*(560/WV)^X, data = df, start = list(X = 1))
  bbp = data.frame(WV = c(800:900), BBP =1)
  
  for(i in 1:nrow(bbp)) {
    
    bbp[i,2] <- BBP[[k]][160]*(560/bbp$WV[i])^summary(SLOPE)$coefficients[1]
    
  }
  
  plot(c(BBP[[k]], bbp$BBP), type = 'l')
  
  dfabs = data.frame(WV = c(400:799), abs = (ABS[[k]][1:400]-water.abs.bb$Abs[1:400]))
  
  SLOPE = nls(formula = abs~abs[160]*(560/WV)^X, data = dfabs, start = list(X = 1))
  
  ABS_700_900 = data.frame(WV = c(800:900), ABS =1)
  
  for(i in 1:nrow(ABS_800_900)) {
    
    ABS_800_900[i,2] <- dfabs$abs[160]*(560/ABS_800_900$WV[i])^summary(SLOPE)$coefficients[1]+water.abs.bb$Abs[i+400]
    
  }
  
  plot(c(dfabs$abs, ABS_800_900$ABS))
  
  u = bbp$BBP/(ABS_800_900$ABS+bbp$BBP)
  
  rrs_RES = (g0+g1*u)*u
  
  Rrs_RES = 0.52*rrs_RES/(1-1.7*rrs_RES)
  
  Rrs_RES = Rrs_RES + (rrs[[k]][400]-Rrs_RES[1])
  
  DIF_percentage = abs((rrs[[k]][400]-Rrs_RES[1])/rrs[[k]][400]*100)
  
  joined = c(rrs[[k]], Rrs_RES)
  
  plot(joined, x = c(400:901))
  
  rrs_new[,k] = joined[1:500]
  
  print(k)   
  
  }
  
}

return(rrs_new)

}
