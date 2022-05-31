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



(ref:figure8) mea

```{r figure8, fig.cap='(ref:figure8)', results='asis',echo=FALSE, dpi=500, out.width = "400px", out.height ="290px", fig.align = "center"}

png("C:/github/sablehead/figures/figure8.png", units="px", width=1600, height=1200, res=300) # write png to file

# specify grid theme colors and size
    theme_nogrid <- function (base_size = 12, base_family = "") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace% 
        theme(
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 3, angle=90),
            axis.ticks = element_line(colour = "black", size = 0.1)
     )   
    }
  
 samples1 <- samples[samples$type=="ID",]    
 samples2 <- samples[samples$type=="SL",] 
 samples3 <- samples[samples$type=="UJ",]
    
 # plot the pilot data
 p1<-ggplot(samples1, aes(x=reorder(obs,SMean), y = SMean, 
                       colour = type, group = type,
                       label = id)) +
  geom_errorbar(
     aes(ymin = SMean - Sstdev, ymax = SMean + Sstdev),
     width = 0,
     size = 0.38
   ) +
  geom_line() +
  geom_point(size = 0.3, color="black") + 
  theme_nogrid()  +
  xlab("Mean") + 
  ylab("Length (cm)") + 
  labs(color='Measurement') +
     geom_text(size = 1, nudge_x = 0.1, color="black") +
     scale_x_discrete(breaks=samples$obs, label=samples$id) # +
     #facet_wrap(~ type) 
 
 p2<-ggplot(samples2, aes(x=reorder(obs, SMean), y = SMean, 
                       colour = type, group = type,
                       label = id)) +
  geom_errorbar(
     aes(ymin = SMean - Sstdev, ymax = SMean + Sstdev),
     width = 0,
     size = 0.38
   ) +
  geom_line() +
  geom_point(size = 0.3, color="black") + 
  theme_nogrid()  +
  xlab("Mean") + 
  ylab("Length (cm)") + 
  labs(color='Measurement') +
     geom_text(size = 1, nudge_x = 0.1, color="black") +
     scale_x_discrete(breaks=samples$obs, label=samples$id) # +
     #facet_wrap(~ type) 
 
  p3<-ggplot(samples3, aes(x=reorder(obs, SMean), y = SMean, 
                       colour = type, group = type,
                       label = id)) +
  geom_errorbar(
     aes(ymin = SMean - Sstdev, ymax = SMean + Sstdev),
     width = 0,
     size = 0.38
   ) +
  geom_line() +
  geom_point(size = 0.3, color="black") + 
  theme_nogrid()  +
  xlab("Mean") + 
  ylab("Length (cm)") + 
  labs(color='Measurement') +
     geom_text(size = 1, nudge_x = 0.1, color="black") +
     scale_x_discrete(breaks=samples$obs, label=samples$id) # +
     #facet_wrap(~ type) 

  while (!is.null(dev.list()))  dev.off()
  img <-   paste('C:/github/sablehead/figures/figure8.png',sep="")   # -- retrieve png 
              knitr::include_graphics(img)


```






