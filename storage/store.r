 ggplotRegression  <- function (fit,labelname) {
                        ggplot(fit$model, aes_string(x = names(fit$model)[2], 
                                                     y = names(fit$model)[1])) + 
                        ylab('Fork length (mm)') +
                        xlab(labelname) + 
                        geom_point(colour="#72AFD2", shape=19, size=1) +
                        stat_smooth(method = "lm",   colour="black") +
                        theme_bw() # +
                        # labs(title = paste( "AdjR2= ",signif(summary(fit)$adj.r.squared, 5),
                        #                   "b = ",signif(fit$coef[[1]],5 ),
                        #                   "m= ",signif(fit$coef[[2]], 5),
                        #                   "P = ",signif(summary(fit)$coef[2,4], 5)),
                        #                    cex.lab=0.4)
                                          }
    
    lm_eqn           <- function(df,y,x, N){   # y = mx + b function  m=slope b=intercept
                        m      <- lm(y ~ x, df);
                        eq     <- substitute(italic(y) == b + m %.% italic(x)*","~~italic(r)^2~"="~r2~", n ="~italic(n)~" ", 
                        list(b  =  format(unname(coef(m)[1]),    digits = 2),
                             m  =  format(unname(coef(m)[2]),    digits = 3),
                             r2 =  format(summary(m)$r.squared,  digits = 3),
                             n  =  N))
                        as.character(as.expression(eq));
                        }



     #  fit the regression line and get the results
     hd                     <-  hdUj                                        # original data
     n                      <-  length(hd$SPECIMEN_ID)                      # sample size
     hd.fit.outliers        <-  lm(Fork_Length ~ Upper_jaw_length, data=hd) 
     linear regression model #plot(hd.fit.outliers, which = 1, pch=21) 
     plot(hd$Upper_jaw_length,hd$Fork_Length, xlim=c(0,20),ylim=c(0,400))  
     abline(hd.fit.outliers)
     hd.fit.outliersummary  <-  summary(hd.fit.outliers)                    # summary

signif(hd.fit.outliers$coef[[2]], 5)

summary(hd.fit.outliers)$coef[2] 
summary(hd.fit.outliers)$coef[,2] 






