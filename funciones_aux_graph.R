# Función para graficar modelo conjugado normal con media desconocida 
fx_norm_n = function(t0,d,variance,yn,n){
  
  #Parámetros de entrada -----  
  #t0: Desviación estándar de la distribución a priori
  #d: Indica a cuántas desviaciones estándar queremos que la media a priori está de la media muestral
  #variance: Varianza de la distribución de verosimilitud
  #yn: Media muestral
  #n: Tamaño  de la muestra
  
  
  #Parámetros calculados -------
  sigma = sqrt(variance) #Desviación estándar muestral
  mu0 = mean(yn)+(d*sigma) # Media de la distribución a priori. 
  #Estos valores permiten obtener a cuatro desviaciones estándar el rango de la muestra a graficar
  ymin = min(c((yn - 4*sigma),(mu0-4*t0),(mu0-4*sigma))) # Valor mínimo de X
  ymax = max(c((yn + 4*sigma),(mu0+4*t0),(mu0+4*sigma))) # Valor máximo de X
  
  tn_2 = t0^2+(sigma^2)/(n) #Desviación estándar de la distribución posterior
  mn = (((yn*n)/sigma^2)+(mu0/(t0^2)))/((n/(sigma^2))+(1/(t0^2))) #Media de la distribución posterior
  
  xx = seq(ymin, ymax,length.out = 10000) #Secuencia de valores para graficar
  
  fy1 = dnorm(x = xx,mean = mu0,sd = t0) #Densidades para la distribución a priori
  fy2 = dnorm(x = xx,mean = mn,sd = sqrt(tn_2)) #Densidades para la distribución posterior
  fy3 = dnorm(x = xx,mean = yn,sd =  sigma) #Densidades para la distribución de verosimilitud
  
  df1 = data.frame(xx,fy1) #Base de datos para distribución a priori: valores en X y densidades
  df2 = data.frame(xx,fy2)  #Base de datos para distribucón a priori: valores en X y densidades
  df3 = data.frame(xx,fy3) #Base de datos para distribución a priori: valores en X y densidades
  
  #Gráfico de las distribuciones -----
  p_1 = ggplot(data=df1, aes(x=xx, y=fy1, colour="A priori")) + 
    geom_line(linewidth=0.8)+
    geom_line(linewidth=0.8) +
    geom_line(data=df2, aes(x=xx, y=fy2, colour="Posterior"), linewidth=0.8) +
    geom_line(data=df3, aes(x=xx, y=fy3, colour="Verosimilitud"), linewidth=0.8) +
    theme_bw() +
    labs(color="Distribución", y= "Densidad", x= "valores variable") +
    ggtitle("Media desconocida y varianza conocida.") 
  
  p_1 #Gráfico
}

# Función para graficar el modelo conjugado normal con varianza desconocida 
fy_ivgamma <- function(a,b,theta,n,s_2){
  
  # Parámetros calculados  -------
  sigma_n = sqrt(s_2) # Desviación estándar muestral
  v0 = 2*a # usado para calcular los parámetros alpha y beta de la distribuiones a priori y posterior de sigma^2
  sigma0_2 = (2*b)/v0 # usado para calcular los parámetros alpha y beta de la distribuiones a priori y posterior de sigma^2
  
  #Distribución de verosimilitud
  veros <- rnorm(10000,mean = theta,sd = sigma_n)
  dens_vero <- density(veros)
  veros_m = sample(veros, n, replace = TRUE)
  v_1=sum((veros_m-theta)^2)/n # usado para calcular los parámetros alpha y beta de la distribuiones a priori y posterior de sigma^2
  
  v0_n = (v0+n)/2 # Cálculo del parámetro alpha de la distribución gamma inversa a priori de sigma^2
  v_n = (n*v_1+v0*sigma0_2)/2 # usado para calcular el parámetro beta de la distribución posterior de sigma2
  
  
  x = seq(0.1,0.99,length.out = 10000)
  #Parámetros para a priori de sigma ----
  cuant_apr_sigma = qinvgamma(p = x, shape = a, scale = 1/b) #Valores de la distribución a priori
  den_apri_sigma = dinvgamma(cuant_apr_sigma, shape = a, scale = 1/b) #Densidades muestrales
  P = ecdf(cuant_apr_sigma)
  df4 = data.frame(cuant_apr_sigma,den_apri_sigma)
  df4_red = df4[df4$cuant_apr_sigma<cuant_apr_sigma[min(which(P(cuant_apr_sigma) >0.99))],]
  
  dposterior <-  rinvgamma(10000,shape = v0_n,scale = 1/v_n)
  dens_pos <- density(dposterior)
  
  df2 = data.frame(dens_pos$x,dens_pos$y)
  df3 = data.frame(dens_vero$x,dens_vero$y)
  
  p_21 = ggplot(data=df4_red, aes(x=cuant_apr_sigma, y=den_apri_sigma)) + 
    geom_line(linewidth = 0.8, col = 3) +
    labs(color = "Distribución", y= "Densidad", x="Varianza")+
    ggtitle("varianza desconocida - A priori") +
    theme_bw()
  
  p_22 = ggplot() + 
    geom_line(data = df2, aes(x=dens_pos.x, y=dens_pos.y), linewidth=0.8, col = 4)+
    labs(color = "Distribución.", y= "Densidad", x="Varianza")+
    ggtitle("Varianza desconocida - Posterior") +
    theme_bw()
  
  p_23 = ggplot()+
    geom_line(data = df3, aes(x=dens_vero.x, y=dens_vero.y), linewidth=0.8, col = 2)+
    labs(color = "Dsitribución", y= "Densidad", x="Valores variable")+
    ggtitle("Varianza desconocida - Verosimilitud") +
    theme_bw()
  
  return(list(p_21,p_22,p_23))
}

# Función para graficar modelo conjugado normal con media condicional en la varianza
f_norm_uni = function(y_barn, S_y, mu0, kappa_0, alpha_0, beta_0, n){
  #Parámetros de entrada -----   
  # n: Tamaño de la muestra
  # mu0: Representa la media de la distribución a prior condicionada en la varianza.
  # y_barn: Media muestral
  # S_y: Varianza muestral 
  # kappa_0: Creencia a priori que se tiene sobre la varianza (sigma_2)
  # alpha_0: Parámetro de forma para la distribución a priori
  # beta_0: Parámetro de escala para la distribución posterior
  
  #Parámetros calculados -------
  nu_0 = 2*alpha_0 
  sigma0_2 = (2*beta_0)/nu_0
  
  #Distribución de verosimilitud ----
  val_vero = rnorm(10000,mean = y_barn, sd = sqrt(S_y)) #Muestra para verosimilitud
  densfy1 = density(val_vero)
  fy1 = densfy1$y #Densidades de los valores muestrales
  df1 = data.frame(densfy1$x, densfy1$y) #Base de datos para verosimilitud: valores en X y densidades
  
  # Parámetros para la apiori de theta ----
  sigma_2 = rinvgamma(1, shape = alpha_0, scale = 1/beta_0) 
  while (sigma_2 == Inf) {
    sigma_2 = rinvgamma(1,shape =  alpha_0,scale = 1/beta_0)
  }
  
  val_apriori_theta = rnorm(10000, mean=mu0, sd=sqrt(sigma_2/kappa_0)) #Muestra para a priori
  densfy2 = density(val_apriori_theta) 
  fy2 = densfy2$y #Densidades muestrales
  df2 = data.frame(densfy2$x, densfy2$y) #Base de datos para distribución a priori: valores en X y densidades
  
  x = seq(0.01,0.99,length.out = 10000) #Secuencia de cuantiles desde 0.01 hasta 0.99
  
  #Parámetros para a priori de sigma ----
  cuant_apr_sigma = qinvgamma(p = x, shape = alpha_0, scale = 1/beta_0) #Valores de la distribución a priori
  den_apri_sigma = dinvgamma(cuant_apr_sigma, shape = alpha_0, scale = 1/beta_0) #Densidades muestrales
  P = ecdf(cuant_apr_sigma)
  df4 = data.frame(cuant_apr_sigma,den_apri_sigma)
  df4_red = df4[df4$cuant_apr_sigma<cuant_apr_sigma[min(which(P(cuant_apr_sigma) > 0.99))],] #Base de datos filtrada con valores hasta el cuantil 99 para distribuci?n a priori
  
  #Parámetros para posterior de sigma ----
  kappa_n = kappa_0 + n 
  nu_n = nu_0 + n
  val_vero_m = sample(val_vero, n, replace = TRUE) #Muestra tamaño n de los valores de verosimilitud para calcular el parámetro nu, necesario en el cálculo de la varianza posterior de sigma
  nu = sum((val_vero_m-mu0)^2)/n
  alpha_n = nu_n/2  #Parámetro de forma para la distribución posterior
  beta_n = (n*nu+nu_0*sigma0_2)/2 #Parámetro de escala para distribución psoterior
  sigma_n2 = (nu_0*sigma0_2 + (n-1)*S_y + ((n*kappa_0*(y_barn-mu0)^2)/(kappa_n)))/(nu_n) #Varianza psoterior
  cuant_post_sigma = rinvgamma(10000, shape= alpha_n, scale = 1/beta_n) #Valores de la distribución posterior
  den_post_sigma = density(cuant_post_sigma) #Densidades de la distribución
  df5 = data.frame(den_post_sigma$x, den_post_sigma$y) #Base de datos para distribución posterior de sigma
  
  
  #Parámetros para posterior de theta ------
  gl = n + nu_0 # Grados de libertad
  mu_n = ((mu0*kappa_0)+(n*y_barn))/(kappa_n) #Media de la distribución psoterior
  val_theta = rt(1000,gl) * sqrt(sigma_n2/(n+kappa_0))+ mu_n #Muestra para distribución posterior de theta
  densidades = density(val_theta) # Densidades muestrales
  df3 = data.frame(densidades$x, densidades$y) # Base de datos para distribución posterior de theta
  
  #Gráfico para cada parámetro -----
  
  # Gráfico para theta ----
  p_31 = ggplot(data=df1, aes(x=densfy1.x, y=densfy1.y,colour="Likelihood")) +
    geom_line(linewidth = 0.8) +
    geom_line(data = df2, aes(x=densfy2.x, y=densfy2.y,colour="A priori"), linewidth=0.8)+
    geom_line(data = df3, aes(x=densidades.x, y=densidades.y, colour="Posterior"), linewidth=0.8)+
    labs(color = "Distribución", y= "Densidad", x="Valores")+
    ggtitle("Media y varianza desconocidos: distribucion a priori de la media dependiendo de la varianza", subtitle = "Theta") +
    theme_bw()
  
  # Gráfico para apriori de sigma ----
  p_32 = ggplot(data = df4_red, aes(cuant_apr_sigma,den_apri_sigma))+
    geom_line(linewidth = 0.8, color = 3) +
    labs(y= "Densidad", x="Varianzas")+
    ggtitle("Distribucion a priori", subtitle = "Varianza") +
    theme_bw()
  
  # Gráfico para posterior de sigma ----
  p_33 = ggplot(data = df5, aes(den_post_sigma.x,den_post_sigma.y))+
    geom_line(linewidth = 0.8, color = 4) +
    labs(y= "Densidad", x="Varianzas")+
    ggtitle("Distribucion posterior", subtitle = "Varianza") +
    theme_bw()
  p_33
  return(list(p_31,p_32,p_33))
}

# Función para graficar modelo conjugado binomial
fy_bin = function(n_ensayos,a,b,y){
  # y: número de éxitos en los n ensayos bernoulli
  
  # Parámetros verosimilitud:
  y_val = 0:n_ensayos
  theta_est = y/n_ensayos #Estimación muestral
  fy1 = dbinom(y_val,n_ensayos,theta_est) #Densidades para la distribución de verosilimitud
  df1 = data.frame(y_val, fy1 = fy1,yend= rep(0, length(y_val))) #Base de datos para distribución verosimilitud: valores en y - densidades
  
  # Parámetros a priori:
  theta_val = seq(0.01,0.99,length.out = 10000)
  fy2 = dbeta(x = theta_val, shape1 = a, shape2 = b) #Densidades para la distribución a priori
  df2 = data.frame(theta_val,fy2)  #Base de datos para distribucón a priori : probabilidades - densidades
  
  #Parámetros posterior: 
  a_pos = y + a 
  b_pos = n_ensayos - y + b
  fy3 = dbeta(x = theta_val, shape1 = a_pos, shape2 = b_pos) 
  df3 = data.frame(theta_val,fy3)
  
  #Gráfico verosimilitud
  
  limInf_vero = floor((n_ensayos*theta_est) - 3*sqrt(n_ensayos*theta_est*(1-theta_est)))
  limSupe_vero = ceiling((n_ensayos*theta_est) + 3*sqrt(n_ensayos*theta_est*(1-theta_est)))
  
  if(limInf_vero < 0){
    limInf_vero = min(y_val)
  }else if(limSupe_vero > n_ensayos){
    limSupe_vero = max(y_val)
  }
  
  g_vero = ggplot(data=df1, aes(x=y_val, y=fy1, xend = y_val, yend = yend)) + 
    geom_point(linewidth = 0.8) +
    geom_segment()+ 
    labs(y= "Probabilidad", x= "y") +
    ggtitle("Distribución de verosimilitud")+
    scale_x_continuous(breaks=seq(limInf_vero,limSupe_vero, by = ceiling((limSupe_vero - limInf_vero)/10)),
                       limits = c(limInf_vero,limSupe_vero)) +
    scale_y_continuous(limits = c(0,(max(fy1)+0.02))) +
    theme_bw()
  
  
  # Gráfico a priori
  g_1 = ggplot(data=df2, aes(x=theta_val, y=fy2)) + 
    geom_line(linewidth=0.8) +
    labs(y= "Densidad", x= "Probabilidad de éxito") +
    ggtitle("Distribución a priori") +
    scale_x_continuous(breaks=seq(0,max(theta_val), by = 0.1),
                       limits = c(0,(max(theta_val))))+
    scale_y_continuous(
      limits = c(0,(max(fy2)+0.05))) +
    theme_bw()
  
  
  #Gráfico posterior
  
  media_pos = a_pos / (a_pos+b_pos)
  varianza_pos = (a_pos*b_pos)/((a_pos+b_pos)^2 * (a_pos+b_pos+1))
  
  limInf_pos = media_pos-3*sqrt(varianza_pos)
  limSupe_pos = media_pos+3*sqrt(varianza_pos)
  
  if(limInf_pos < 0){
    limInf_pos = min(theta_val)
  }else if(limSupe_pos > 1){
    limSupe_pos = max(theta_val)
  }
  
  g_2 = ggplot(data = df3, aes(x=theta_val, y=fy3)) + 
    geom_line(linewidth=0.8) +
    labs(y= "Densidad", x= "Probabilidad de éxito") +
    ggtitle("Distribución posterior") +
    scale_x_continuous(breaks = seq(round(limInf_pos,2),round(limSupe_pos,2), by = round(((limSupe_pos-limInf_pos)/5),2)),
                       limits = c(limInf_pos,limSupe_pos))+
    scale_y_continuous(limits = c(0,(max(fy3)+0.05))) +
    theme_bw()
  
    return(list(g_vero, g_1,g_2))

}

# Función para graficar modelo conjugado poisson
fx_pois = function(nobs, theta_m, alpha_0, beta_0){
  ## Likelihood:
  like_pois_original <- rpois(n = 10000, lambda = theta_m)
  
  ## Prior:
  prior_pois <- rgamma(n = 10000, shape = alpha_0, rate = beta_0)
  
  ## Posterior:
  like_pois <- sample(like_pois_original,nobs, replace = TRUE)
  alpha_pos <- sum(like_pois) + alpha_0
  beta_pos <- length(like_pois) + beta_0
  post_pois <- rgamma(n = 10000, shape = alpha_pos, rate = beta_pos)
  
  ### Graphics
  ## Likelihood:
  datos_like <- data.frame(x = like_pois_original, y = dpois(like_pois_original,theta_m))
  
  likelihood <- ggplot(data = data.frame(x =  datos_like[ ,1],
                                         y =  datos_like[ ,2],
                                         yend = rep(0,length(datos_like[,1]))),
                       aes(x = x, y = y, xend = x, yend = yend)) +
    geom_point(color = "blue") +
    geom_segment(color = "blue") +
    scale_x_continuous(name="\nx",
                       breaks=seq(min(datos_like[ ,1]),max( datos_like[ ,1]), by = ceiling((max(datos_like[,1]) - min(datos_like[,1]))/5)),
                       limits = c((min(datos_like[,1])),(max(datos_like[ ,1])))) +
                       #)+
    scale_y_continuous(name="Densidad\n",
                       breaks=seq(0,max( datos_like[ ,2]), by = round(((max(datos_like[,2]) - min(datos_like[,2]))/4),2)), 
                       limits = c(0,(max(datos_like[ ,2])))) 
    #)+
    ggtitle("Distribución de verosimilitud") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 10))
  
  ## A priori:
  data_prior <- data.frame(x = prior_pois, y = dgamma(prior_pois,shape = alpha_0, rate = beta_0))
  
  prior <- ggplot(data = data_prior, aes(x = x, y = y)) + 
    geom_line(color = "blue", linewidth = 0.8) + theme_bw() + 
    labs(title = "Distribución a priori", y = "Densidad") +
    scale_x_continuous(
      breaks=seq(round(min(data_prior[ ,1]),3),round(max(data_prior[ ,1]),3), by = round(((max(data_prior[,1]) - min(data_prior[,1]))/5),3)),
      limits = c(round(min(data_prior[,1]),3),round(max(data_prior[ ,1]),3))) +
    scale_y_continuous(
      breaks=seq(0,round(max(data_prior[ ,2]),3), by = round(((max(data_prior[,2]) - min(data_prior[,2]))/6),3)),
      limits = c(0,round((max(data_prior[ ,2])),3)))
  
  
  # Posterior:
  data_pos <- data.frame(x = post_pois, y = dgamma(post_pois,shape = alpha_pos, rate = beta_pos))
  
  
  posterior <- ggplot(data = data_pos, aes(x = x, y = y)) +
    geom_line(color = "blue", linewidth = 0.8) + theme_bw() +
    labs(title = "Distribución posterior", y = "Densidad") +
    scale_x_continuous(
      breaks=seq(round(min(data_pos[ ,1]),3),round(max(data_pos[ ,1]),3), by = round(((max(data_pos[,1]) - min(data_pos[,1]))/5),3)),
      limits = c(round(min(data_pos[,1]),3),round(max(data_pos[ ,1]),3))) 

  return(list(likelihood, prior,posterior))
  
}
