##Plot scripts


plots_secchi_validation_sep_density = function(estimado, medido,separador,color, MAX_ZSD,
                                                   METODO,campanha, size_axis, size_title, size_txt) {
  
  
  
  df3 = data.frame(est = estimado, 
                   measured = medido, 
                   separador = separador, 
                   color = color) %>% na.omit() %>% filter(measured > 0 & est > 0 & est < MAX_ZSD & measured < MAX_ZSD)
  
  xlimits = c(min(log(df3$measured)-3), c(max(log(df3$measured)+20)))
  ylimits = c(min(log(df3$measured)-3), c(max(log(df3$measured)+20)))
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median((Y)))*(10^(abs(median(Y)))-1))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,2),
                  round(cor_summarise$R2,2), 
                  round(E_summarise$E,2), 
                  round(bias2_summarise$BIAS2,2), 
                  round(N_Sumarrise_$N,2),
                  round(MAE_Sumarrise_$MAE, 2))
  
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,2),
                  round(cor_summarise$R2,2), 
                  round(E_summarise$E,2), 
                  round(bias2_summarise$BIAS2,2), 
                  round(N_Sumarrise_$N,2))
  
  
  names(df) = c('separador', 'MAPE', 'COR', 'E', 'BIAS', 'N')
  
  
  #max = max(df3$measured)
  min = 0
  max = MAX_ZSD
  df3 %>% ggplot(aes(x = measured, y = est)) + 
    #geom_point(aes(colour = color),size = 2) + 
    facet_wrap(~separador) + 
    geom_pointdensity(adjust = 4) +
    scale_color_viridis() +
    
    
    geom_text(x = 0, y = max, hjust = 0, vjust = 1,
              aes(label = paste0(" R = ", COR, "\n", 
                                 "MAPE = ", MAPE, "\n", 
                                 "E = ", E, "\n",
                                 "BIAS = ", BIAS, "\n", 
                                 "N = ", N, '\n')), data = df, size = size_txt) +
    
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_continuous(limits = c(0,max), name = expression(Z[sd]~Measured~(m))) + 
    scale_y_continuous(limits = c(0,max), name = expression(Z[sd]~Modelled~(m))) + 
    
    
    labs(title=METODO) +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          #plot.title = element_text(size=size_title, face='bold', hjust = 0.5),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_title, face='bold')) +
    theme(plot.margin = unit(c(4,4,4,4), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}


plots_secchi_validation_sep_log_density = function(estimado, medido,separador,color, MAX_ZSD,
                                                   METODO,campanha, size_axis, size_title, size_txt, size_points) {
  
  
  
  df3 = data.frame(est = estimado, 
                   measured = medido, 
                   separador = separador, 
                   color = color) %>% na.omit() %>% filter(measured > 0 & est > 0 & est < MAX_ZSD & measured < MAX_ZSD)
  
  xlimits = c(min(log(df3$measured)-3), c(max(log(df3$measured)+20)))
  ylimits = c(min(log(df3$measured)-3), c(max(log(df3$measured)+20)))
  
  df3$error_ <- abs((df3$measured-df3$est)/df3$measured)*100
  df3$BIAS <- log10(df3$est)-log10(df3$measured)
  df3$MAE <- abs(log10(df3$est)-log10(df3$measured))
  df3$Y = log10(df3$est/df3$measured)
  
  
  MAPE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAPES = mean(error_))
  MAE_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(MAE = 10^median(MAE))
  BIAS_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(BIAS = 10^mean(BIAS))
  N_Sumarrise_ = df3 %>% group_by(separador) %>% summarise(N = length( separador ))
  E_summarise = df3 %>% group_by(separador) %>% summarise(E = 100*(10^(median(abs(Y)))-1))
  bias2_summarise = df3 %>% group_by(separador) %>% summarise(BIAS2 = 100*sign(median((Y)))*(10^(abs(median(Y)))-1))
  cor_summarise = df3 %>% group_by(separador) %>% summarise(R2 = cor(est,measured))
  SLOPE = df3 %>% group_by(separador) %>% summarise(SLOPE = lm(log(est)~log(measured))$coefficients[2])
  RMSLE = df3 %>% group_by(separador) %>% summarise(RMSLE = rmsle(actual = measured, predicted = est))
  
  df = data.frame(MAPE_Sumarrise_$separador,
                  round(MAPE_Sumarrise_$MAPES,2),
                  round(cor_summarise$R2,2), 
                  round(E_summarise$E,2), 
                  round(bias2_summarise$BIAS2,2), 
                  round(N_Sumarrise_$N,2),
                  round(MAE_Sumarrise_$MAE, 2),
                  round(SLOPE$SLOPE, 2), 
                  round(RMSLE$RMSLE, 2))
  
  BETA = '\U03B2'
  epsilon = '\U03B5'
  
  
  max = log(min(df3$est))
  min = xlimits[1]
  
  names(df) = c('separador', 'MAPE', 'COR', 'E', 'BIAS', 'N', 'mAE', 'SLOPE', 'RMSLE')
  
  df3 %>% ggplot() + 
    #geom_point(aes(x = measured, y = est), fill = 'black', color = 'black', size = 3, pch = 21) + 
    facet_wrap(~separador) + 
    geom_pointdensity(aes(x = measured, y = est), adjust = 4, size = 2) +
    #geom_point(aes(x = measured, y = est), fill = alpha('white', 0),  color = 'black', size = 3, pch = 21) + 
    
    scale_color_viridis() +
  
    # 
    # 
    # geom_text(x = -1, y = 1,5, hjust = 0,  label.padding = unit(0.3, "lines"),
    #            aes(label = paste0(epsilon, " = ", E,'%', "\n",
    #                               BETA, " = ", BIAS, '%', "\n", 
    #                               "S = ", SLOPE)), data = df, size = size_txt) +
    # 
    # 
   geom_text(x = -1, y = 1.5, hjust = 0, vjust = 1,
             aes(label = paste0(epsilon, " = ", E, '%', "\n",
                                BETA, " = ", BIAS, '%', '\n', 
                                'MAPE = ', MAPE, '%', '\n',
                                'RMSLE = ', RMSLE, '\n',
                                "S = ", SLOPE, '\n',
                                "N = ", N)), data = df, size = size_txt) +
    
    
    
    geom_abline(slope = 1, intercept = 0) + 
    
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                  labels = trans_format("log10", math_format(10^.x)), 
                  limits = c(0.1,30),
                  name = expression(Z[sd]~Measured~(m))) +
    
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits = c(0.1,30), 
                  name = expression(Z[sd]~Modelled~(m))) +

    
    labs(title=METODO) +
    labs(color="Density") +
    theme_bw() + 
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.78,0.26),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.key.height = unit(0.8, 'cm'),
          legend.title = element_text(size=30),
          legend.text = element_text(size=25),
          plot.title = element_text(size=size_title, face='bold', hjust = 0.5),
          axis.title = element_text(face="bold", size = size_axis),
          axis.text.x = element_text(colour="black", size = size_axis),
          axis.text.y = element_text(colour="black", size = size_axis),
          axis.line = element_line(size=2, colour = "black"),
          strip.text = element_text(size=size_title, face='bold')) +
    theme(plot.margin = unit(c(2,2,2,2), "lines")) +
    geom_abline(linetype = "dashed") 
  
  
  
}
