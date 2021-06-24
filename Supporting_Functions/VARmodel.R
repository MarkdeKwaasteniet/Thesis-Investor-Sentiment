install.packages("vars")
install.packages("stargazer")
install.packages("patchwork")
install.packages("devtools")
install.packages("usethis")
source("white_r.R")
library(vars)
library(stargazer)
library(patchwork)
library(devtools)
library(usethis)

# VAR model Analysis

VAR_model <- function(df, nlags, maxlag){
  # Create VAR model
  var_model <- VAR(df, p = nlags, type = "const", season = NULL, exogen = NULL)
  
  # Run tests on the model
  # Testing for Serial Autocorrelation - Portmanteau Test
  serial <- serial.test(var_model, lags.bg = maxlag, type = "BG")
  print(serial)
  
  # Testing for Heteroskedasticity - White test
  white <- whites.htest(var_model)
  print(white)
  
  # Testing for Normality - Jarque Bera test
  norm <- normality.test(var_model, multivariate.only = TRUE)
  print(norm$jb.mul$JB)
  
  # Testing for structural breaks
  stability <- stability(var_model, type = "OLS-CUSUM")
  plot(stability)
  return(var_model)
}

causality_test <- function(var_model, cause1, cause2, robust){
  grang_test <- causality(var_model, cause = cause1, vcov.=vcovHC(var_model, type = robust))
  print(grang_test$Granger)
  grang_test <- causality(var_model, cause = cause2, vcov.=vcovHC(var_model, type = robust))
  paste("H0:", cause2, "is not granger causal on", cause1)
  print(grang_test$Granger)
}

irfplot <- function(df, df1, df2, cause1, cause2, window, lagdaily, lag30min, lag60min){
  var_model <- VAR(df, p = lagdaily, type = "const", season = NULL, exogen = NULL)
  irf_plot1 <- irf(var_model, impulse = cause1, response = cause2, n.ahead = window, boot = TRUE, ortho = TRUE)
  irf_plot2 <- irf(var_model, impulse = cause2, response = cause1, n.ahead = window, boot = TRUE, ortho = TRUE)
  single_varirf <- extract_varirf(irf_plot1, irf_plot2)
  var_model1 <- VAR(df1, p = lag30min, type = "const", season = NULL, exogen = NULL)
  irf_plot3 <- irf(var_model1, impulse = cause1, response = cause2, n.ahead = window, boot = TRUE, ortho = TRUE)
  irf_plot4 <- irf(var_model1, impulse = cause2, response = cause1, n.ahead = window, boot = TRUE, ortho = TRUE)
  single_varirf1 <- extract_varirf(irf_plot3, irf_plot4)
  var_model2 <- VAR(df2, p = lag60min, type = "const", season = NULL, exogen = NULL)
  irf_plot5 <- irf(var_model2, impulse = cause1, response = cause2, n.ahead = window, boot = TRUE, ortho = TRUE)
  irf_plot6 <- irf(var_model2, impulse = cause2, response = cause1, n.ahead = window, boot = TRUE, ortho = TRUE)
  single_varirf2 <- extract_varirf(irf_plot5, irf_plot6)
  
  irf_plot1 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf[,2], ymin=single_varirf[,3], ymax=single_varirf[,4])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause1, "effect on", cause2))+
    ylab(paste("Response of", cause2)) +
    xlab(paste("Daily, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  irf_plot2 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf[,5], ymin=single_varirf[,6], ymax=single_varirf[,7])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause2, "effect on", cause1))+
    ylab(paste("Response of", cause1)) +
    xlab(paste("Daily, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  irf_plot3 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf1[,2], ymin=single_varirf1[,3], ymax=single_varirf1[,4])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause1, "effect on", cause2))+
    ylab(paste("Response of", cause2)) +
    xlab(paste("30min, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  irf_plot4 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf1[,5], ymin=single_varirf1[,6], ymax=single_varirf1[,7])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause2, "effect on", cause1))+
    ylab(paste("Response of", cause1)) +
    xlab(paste("30min, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  irf_plot5 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf2[,2], ymin=single_varirf2[,3], ymax=single_varirf2[,4])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause1, "effect on", cause2))+
    ylab(paste("Response of", cause2)) +
    xlab(paste("60min, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  irf_plot6 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf2[,5], ymin=single_varirf2[,6], ymax=single_varirf2[,7])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause2, "effect on", cause1))+
    ylab(paste("Response of", cause1)) +
    xlab(paste("60min, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  grid.arrange(irf_plot1, irf_plot2, irf_plot3, irf_plot4, irf_plot5, irf_plot6, nrow=3, top=textGrob("Orthogonal Impulse Response Function",gp=gpar(fontsize=20,font=3)))
  
}

irfplot_volume <- function(df, df1, cause1, cause2, cause3, window, lagdaily, lagdaily1){
  var_model <- VAR(df, p = lagdaily1, type = "const", season = NULL, exogen = NULL)
  irf_plot1 <- irf(var_model, impulse = cause1, response = cause2, n.ahead = window, boot = TRUE, ortho = TRUE)
  irf_plot2 <- irf(var_model, impulse = cause2, response = cause1, n.ahead = window, boot = TRUE, ortho = TRUE)
  single_varirf <- extract_varirf(irf_plot1, irf_plot2)
  var_model1 <- VAR(df1, p = lagdaily, type = "const", season = NULL, exogen = NULL)
  irf_plot3 <- irf(var_model1, impulse = cause1, response = cause3, n.ahead = window, boot = TRUE, ortho = TRUE)
  irf_plot4 <- irf(var_model1, impulse = cause3, response = cause1, n.ahead = window, boot = TRUE, ortho = TRUE)
  single_varirf1 <- extract_varirf(irf_plot3, irf_plot4)
  
  irf_plot1 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf[,2], ymin=single_varirf[,3], ymax=single_varirf[,4])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause1, "effect on", cause2))+
    ylab(paste("Response of", cause2)) +
    xlab(paste("Daily, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  irf_plot2 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf[,5], ymin=single_varirf[,6], ymax=single_varirf[,7])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause2, "effect on", cause1))+
    ylab(paste("Response of", cause1)) +
    xlab(paste("Daily, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  irf_plot3 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf1[,2], ymin=single_varirf1[,3], ymax=single_varirf1[,4])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause1, "effect on", cause3))+
    ylab(paste("Response of", cause3)) +
    xlab(paste("30min, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  irf_plot4 <- single_varirf %>% 
    ggplot(aes(x=period, y=single_varirf1[,5], ymin=single_varirf1[,6], ymax=single_varirf1[,7])) +
    geom_hline(yintercept = 0, color="red") +
    geom_ribbon(fill="grey", alpha=0.2) +
    geom_line() +
    theme_light() +
    ggtitle(paste(cause3, "effect on", cause1))+
    ylab(paste("Response of", cause1)) +
    xlab(paste("30min, 10 period Time Window")) +
    theme(plot.title = element_text(size = 11, hjust=0.5),
          axis.title.y = element_text(size=11))
  
  grid.arrange(irf_plot1, irf_plot2, irf_plot3, irf_plot4, nrow=2, top=textGrob("Orthogonal Impulse Response Function",gp=gpar(fontsize=20,font=3)))
}


extract_varirf <- function(...){
  
  varirf_object <- list(...) #list one or more varirf input objects
  
  get_vec_length <- function(list_item){nrow(list_item[[1]][[1]])}
  
  if (!("varirf" %in% mapply(class, varirf_object))){
    stop("this function only accepts 'varirf' class objects")
  }
  
  if (length(unique(mapply(class, varirf_object)))!=1){
    stop("all input items must be 'varirf' class objects")
  }    
  if (length(unique(mapply(get_vec_length, varirf_object)))!=1){
    stop("all irf vectors must have the same length")   
  }  
  
  period <- as.data.frame(0:(nrow(varirf_object[[1]][[1]][[1]])-1)) 
  names(period) <- "period"
  
  for (l in 1:length(varirf_object)){
    for (i in 1:3){
      for (j in 1:dim(varirf_object[[l]][[i]][[1]])[2]){
        for (k in 1:length(varirf_object[[l]][[1]])){
          temp_colname <- paste(names(varirf_object[[l]][i]), #vector type (irf, lower, or upper)
                                names(varirf_object[[l]][[i]])[k], #impulse name
                                colnames(varirf_object[[l]][[i]][[k]])[j], #response name
                                sep = "_")
          
          temp <- as.data.frame(varirf_object[[l]][[i]][[k]][, j]) #extracts the vector
          
          names(temp) <- temp_colname #add the column name (vectortype_impulse_reponse)
          period <- cbind(period, temp) 
        }
        
      }
    }
  }
  names(period) <- tolower(names(period))
  return(period)
}