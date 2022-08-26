library(cowplot)
library(lattice)

#OK Variograms

# Save Variograms
 dgt <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2

 mdl <- Density_variogram$var_model
 cls <- as.character(mdl[2, "model"])
 ngt <- sum(mdl[1, "psill"])
 sll <- sum(mdl[, "psill"])
 rng <- sum(mdl[, "range"])
 lbl <- paste("Model:", cls,
              "\nNugget:", round(ngt,5),
              "\nSill:", round(sll,4),
              "\nRange:", round(rng,2))

 if (cls %in% c("Mat", "Ste")) {
   kpp <- mdl[2, "kappa"]
   lbl <- paste(lbl, "\nKappa:", round(kpp, dgt(kpp)), "")
 }
 OK_DVG=xyplot(gamma ~ dist, data=Density_variogram$exp_var,
                          main = "Density variogram",
                          xlab = "Distance (m)",
                          ylab = "Semi-variance",
                          panel = function(x, y, ...) {
                            gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
                            ltext(max(x), 0.2 * max(y), lbl, font = 2, cex = .9, adj = c(1, 0),
                                  col = "grey30")},
                          labels=NULL,mode='direct',model=mdl,
                          direction=c(Density_variogram$exp_var$dir.hor[1],Density_variogram$exp_var$dir.ver[1]))

 DV_grob=as_grob(OK_DVG)
 ggdraw()+draw_plot(DV_grob)


 dgt2 <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2

 mdl2 <- Prevalence_variogram$var_model
 cls2 <- as.character(mdl2[2, "model"])
 ngt2 <- sum(mdl2[1, "psill"])
 sll2 <- sum(mdl2[, "psill"])
 rng2 <- sum(mdl2[, "range"])
 lbl2 <- paste("Model:", cls2,
               "\nNugget:", round(ngt2, 5),
               "\nSill:", round(sll2, 4),
               "\nRange:", round(rng2,4))

 if (cls2 %in% c("Mat", "Ste")) {
   kpp2 <- mdl2[2, "kappa"]
   lbl2 <- paste(lbl2, "\nKappa:", round(kpp2, dgt2(kpp2)), "")
 }

 #compare
 OK_PVG=xyplot(gamma ~ dist, data=Prevalence_variogram$exp_var,
                             main = "Prevalence variogram",
                             xlab = "Distance (m)",
                             ylab = "Semi-variance",
                             panel = function(x, y, ...) {
                               gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
                               ltext(max(x), 0.2 * max(y), lbl2, font = 2, cex = .9, adj = c(1, 0),
                                     col = "grey30")},
                             labels=NULL,mode='direct',model=mdl2,
                             direction=c(Prevalence_variogram$exp_var$dir.hor[1],Prevalence_variogram$exp_var$dir.ver[1]))

 P_grob=as_grob(OK_PVG)

 variogram_grobs=ggdraw()+
   draw_plot(DV_grob,x=0,y=0,width=.5,height = 1)+
   draw_plot(P_grob,x=.5,y=0,width=.5,height=1);variogram_grobs
 
 # ggsave(variogram_grobs,filename = paste(getwd(),"/Figures/OK_Variogram_plots.pdf",sep=""),
 #        height=5,width=8,units='in',dpi=300)


# Save Universal Kriging Variograms
# Found here https://gis.stackexchange.com/questions/234221/adjust-text-font-size-in-plotting-autofitvariogram-in-r

dgt <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2

mdl <- UK_Density_variogram$var_model
cls <- as.character(mdl[2, "model"])
ngt <- sum(mdl[1, "psill"])
sll <- sum(mdl[, "psill"])
rng <- sum(mdl[, "range"])
lbl <- paste("Model:", cls,
             "\nNugget:", round(ngt,5),
             "\nSill:", round(sll,4),
             "\nRange:", round(rng,2))

if (cls %in% c("Mat", "Ste")) {
  kpp <- mdl[2, "kappa"]
  lbl <- paste(lbl, "\nKappa:", round(kpp, dgt(kpp)), "")
}


UK_DVG=xyplot(gamma ~ dist, data=UK_Density_variogram$exp_var,
                         main = "Density variogram",
                         xlab = "Distance (m)",
                         ylab = "Semi-variance",
                         panel = function(x, y, ...) {
                           gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
                           ltext(max(x), 0.2 * max(y), lbl, font = 2, cex = .9, adj = c(1, 0),
                                 col = "grey30")},
                         labels=NULL,mode='direct',model=mdl,
                         direction=c(UK_Density_variogram$exp_var$dir.hor[1],UK_Density_variogram$exp_var$dir.ver[1]))

DV_grob=as_grob(UK_DVG)
ggdraw()+draw_plot(DV_grob)


dgt2 <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2

mdl2 <- UK_Prevalence_variogram$var_model
cls2 <- as.character(mdl2[2, "model"])
ngt2 <- sum(mdl2[1, "psill"])
sll2 <- sum(mdl2[, "psill"])
rng2 <- sum(mdl2[, "range"])
lbl2 <- paste("Model:", cls2,
              "\nNugget:", round(ngt2, 5),
              "\nSill:", round(sll2, 4),
              "\nRange:", round(rng2,4))

if (cls2 %in% c("Mat", "Ste")) {
  kpp2 <- mdl2[2, "kappa"]
  lbl2 <- paste(lbl2, "\nKappa:", round(kpp2, dgt2(kpp2)), "")
}

#compare
UK_PVG=xyplot(gamma ~ dist, data=UK_Prevalence_variogram$exp_var,
                            main = "Prevalence variogram",
                            xlab = "Distance (m)",
                            ylab = "Semi-variance",
                            panel = function(x, y, ...) {
                              gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
                              ltext(max(x), 0.2 * max(y), lbl2, font = 2, cex = .9, adj = c(1, 0),
                                    col = "grey30")},
                            labels=NULL,mode='direct',model=mdl2,
                            direction=c(UK_Prevalence_variogram$exp_var$dir.hor[1],UK_Prevalence_variogram$exp_var$dir.ver[1]))

P_grob=as_grob(UK_PVG)

variogram_grobs=ggdraw()+
  draw_plot(DV_grob,x=0,y=0,width=.5,height = 1)+
  draw_plot(P_grob,x=.5,y=0,width=.5,height=1);variogram_grobs

 # ggsave(variogram_grobs,filename = paste(getwd(),"/Figures/UK_Variogram_plots.pdf",sep=""),
 #        height=5,width=8,units='in',dpi=300)
 