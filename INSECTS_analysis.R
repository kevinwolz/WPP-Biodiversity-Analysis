### INSECTS ANALYSIS
### Programmer: Kevin Wolz
### Originally Created: 20 Nov 2014
### Last Updated: 20 Nov 2014

if(ANALYSIS){
  
  
  ##### SPECIES RICHNESS #####
  sp.rich = subset(alpha.pitfall, samples==36)
  sp.rich$S.est.84CI.lb = sp.rich$S.est + sp.rich$S.est.sd * 1.372
  sp.rich$S.est.84CI.ub = sp.rich$S.est - sp.rich$S.est.sd * 1.372
  
  sp.rich.range = with(sp.rich, range(S.est, S.est.84CI.ub, S.est.84CI.lb))
  abun.range = with(sp.rich, range(indiv.comp))
  abun.range = c(floor(abun.range[1]), ceiling(abun.range[2]))
  
  ## PLOT SPECIES RICHNESS vs. MONTH by SYSTEM
  pdf(paste(outputPlotPath, "Species_Richness_", yr, ".pdf",sep=""))
  par(mar=c(5.1,5.1,4.1,2.1)) #bottom, left, top, right margins
  add = FALSE
  for (i in unique(sp.rich$system)){
    data = subset(sp.rich, system==i)
    attach(data)
    plotCI(month, S.est,
           (S.est.sd * 1.372),
           #ui = S.est.84CI.ub,
           #li = S.est.84CI.lb,
           col = system,
           ylim = sp.rich.range,
           pch = 19, cex.main = 1.6, cex.lab = 1.4,
           main = paste("Species Richness ", yr, sep = ""),
           xlab = "Month", 
           ylab = "Species Richness",
           add = add)
    add = TRUE
    lines(month, S.est, col = system, lty = 1)
    abline(h=mean(S.est), col = system, lwd = 2, lty = 2)
    detach(data)
  }
  legend("topleft", c("WPP (84% CI)", "CSR (84% CI)", "WPP Monthly Mean", "CSR Monthly Mean"), col = c(2,1,2,1), pch = c(19,19,NA,NA), lty = c(NA, NA, 2, 2), lwd = c(NA, NA, 2, 2))
  dev.off()
  
  ## PLOT ABUNDANCE vs. MONTH by SYSTEM
  pdf(paste(outputPlotPath, "Abundance_", yr, ".pdf",sep=""))
  par(mar=c(5.1,5.1,4.1,2.1)) #bottom, left, top, right margins
  plot(sp.rich$month, sp.rich$indiv.comp, type="n",
      ylim = abun.range,
      cex.main = 1.6, cex.lab = 1.4,
      main = paste("Insect Abundance ", yr, sep = ""),
      xlab = "Month", 
      ylab = "Number of Individuals")
  for (i in unique(sp.rich$system)){
    data = subset(sp.rich, system==i)
    attach(data)
    lines(month, indiv.comp,
           type = "b",
           col = system,
           pch = 19)
    abline(h=mean(indiv.comp), col = system, lwd = 2, lty = 2)
    detach(data)
  }
  legend("topright", c("WPP", "CSR", "WPP Monthly Mean", "CSR Monthly Mean"), col = c(2,1,2,1), pch = c(19,19,NA,NA), lty = c(NA, NA, 2, 2), lwd = c(NA, NA, 2, 2))
  dev.off()
  
  
  ##### ALPHA DIVERSITY #####
  alpha = subset(alpha.pitfall, samples==16)
  alpha.indices = c("ACE", "ICE", "chao1", "chao2", "jack1", "jack2", "bootstrap", "alpha", "shannon", "shannon.exp", "simpson.inv")
  #alpha$S.est.84CI.lb = sp.rich$S.est + sp.rich$S.est.sd * 1.372
  #lpha$S.est.84CI.ub = sp.rich$S.est - sp.rich$S.est.sd * 1.372
  
  
  ## PLOT ALPHA DIVERSITY vs. MONTH by SYSTEM
  for(index in alpha.indices){
    MEAN = paste(index, ".mean", sep = "")
    SD = paste(index, ".sd", sep = "")
    INDEX = cbind(alpha[, c(1:8)], alpha[, c(MEAN, SD)])
    names(INDEX)[9:10] = c("avg", "sd")
    
    alpha.range = range((INDEX$avg+INDEX$sd), (INDEX$avg-INDEX$sd))
    
    pdf(paste(outputPlotPath, "ALPHA_DIVERISTY_", index, "_", yr, ".pdf",sep=""))
    par(mar=c(5.1,5.1,4.1,2.1)) #bottom, left, top, right margins
    add = FALSE
    for (i in unique(INDEX$system)){
      data = subset(INDEX, system==i)
      attach(data)
      plotCI(month, avg,
             sd,
             col = system,
             ylim = alpha.range,
             pch = 19, cex.main = 1.6, cex.lab = 1.4,
             main = paste(index, yr, sep = " "),
             xlab = "Month", 
             ylab = paste(index),
             add = add)
      add = TRUE
      lines(month, avg, col = system, lty = 1)
      abline(h=mean(avg), col = system, lwd = 2, lty = 2)
      detach(data)
    }
    legend("topleft", c("WPP (sd)", "CSR (sd)", "WPP Monthly Mean", "CSR Monthly Mean"), col = c(2,1,2,1), pch = c(19,19,NA,NA), lty = c(NA, NA, 2, 2), lwd = c(NA, NA, 2, 2))
    dev.off()
  }
  
  
  
  ##### BEA DIVERSITY #####
  beta = beta.pitfall
  #beta = subset(beta, shared.species.obs > 0)
  beta.indices = c("jaccard", "sorensen", "chao.jaccard.est", "chao.sorensen.est", "morisita.horn", "bray.curtis")
  
  beta.summary = summaryBy(list(beta.indices, c("system", "year", "month")), data = beta, FUN = c(mean, sd))
  
  
  ## PLOT BETA DIVERSITY vs. MONTH by SYSTEM
  for(index in beta.indices){
    MEAN = paste(index, ".mean", sep = "")
    SD = paste(index, ".sd", sep = "")
    INDEX = cbind(beta.summary[, c(1:3)], beta.summary[, c(MEAN, SD)])
    names(INDEX)[4:5] = c("avg", "sd")
    
    beta.range = range((INDEX$avg+INDEX$sd), (INDEX$avg-INDEX$sd))
    
    pdf(paste(outputPlotPath, "BETA_DIVERISTY_", index, "_", yr, ".pdf", sep=""))
    par(mar=c(5.1,5.1,4.1,2.1)) #bottom, left, top, right margins
    add = FALSE
    for (i in unique(INDEX$system)){
      data = subset(INDEX, system==i)
      attach(data)
      plotCI(month, avg,
             sd,
             col = system,
             ylim = beta.range,
             pch = 19, cex.main = 1.6, cex.lab = 1.4,
             main = paste(index, yr, sep = " "),
             xlab = "Month", 
             ylab = paste(index),
             add = add)
      add = TRUE
      lines(month, avg, col = system, lty = 1)
      abline(h=mean(avg), col = system, lwd = 2, lty = 2)
      detach(data)
    }
    legend("topleft", c("WPP (sd)", "CSR (sd)", "WPP Monthly Mean", "CSR Monthly Mean"), col = c(2,1,2,1), pch = c(19,19,NA,NA), lty = c(NA, NA, 2, 2), lwd = c(NA, NA, 2, 2))
    dev.off()
  }
  

  
  
  ##### COUNT DATA #####
  INSECTS = names(count.pitfall)[15:ncol(count.pitfall)]
  count.summary = summaryBy(list(c(INSECTS), c("year", "month", "system")), data=count.pitfall, FUN = sum, na.rm = TRUE, keep.names = TRUE)
  
  ## PLOT SPECIES ABUNDANCE THROUGH TIME by SYSTEM
  for(insect in INSECTS){
    yrange = c(0,ifelse(max(count.summary[,insect])>0,max(count.summary[,insect]),1))
    pdf(paste(outputCountPlotPath, insect, ".pdf",sep=""))
    par(mar=c(5.1,5.1,4.1,2.1)) #bottom, left, top, right margins
    plot(count.summary$month, count.summary[,insect], type="n",
         ylim = yrange,
         cex.main = 1.6, cex.lab = 1.4,
         main = paste(insect),
         xlab = "Month", 
         ylab = "Number of Individuals")
    for(season in unique(count.summary$year)){
      for(sys in unique(count.summary$system)){
      year.count.summary = subset(count.summary, year==season & system==sys)
      lines(year.count.summary$month, year.count.summary[,insect],
            type="b",
            pch = 19,
            col = year.count.summary$system,
            lty = year.count.summary$year-2012)
    }
    }
    
    legend("topleft", c("WPP 2013", "CSR 2013", "WPP 2014", "CSR 2014"), col = c(2,1,2,1), pch = c(19,19,19,19), lty = c(1, 1, 2, 2), lwd = c(1, 1, 1, 1))
    
    dev.off()
  }
  
  
  ##### ABUNDANCE ANALYSIS #####
  #count.summary$n.indivs = rowSums(count.summary[,4:ncol(count.summary)])
  
  count.sys.summary = summaryBy(list(c(INSECTS), c("system")), data=count.summary, FUN = sum, na.rm = TRUE, keep.names = TRUE)
  
  abundance = data.frame(species =  names(count.sys.summary[,2:ncol(count.sys.summary)]), WPP = as.integer(count.sys.summary[2,2:ncol(count.sys.summary)]), CSR = as.integer(count.sys.summary[1,2:ncol(count.sys.summary)]))

  
  abundance$WPP.minus.CSR = abundance$WPP - abundance$CSR
  abundance$WPP.div.CSR = abundance$WPP  / abundance$CSR
  abundance$WPP.perc.CSR = abundance$WPP.minus.CSR  / abundance$CSR
  abundance$WPP.div.CSR[which(abundance$CSR==0)] = NA
  abundance$WPP.perc.CSR[which(abundance$CSR==0)] = NA
  
  abundance$WPPrel = abundance$WPP/36
  abundance$CSRrel = abundance$CSR/16
  
  abundance$WPPrel.minus.CSRrel = abundance$WPPrel - abundance$CSRrel
  abundance$WPPrel.div.CSRrel = abundance$WPPrel  / abundance$CSRrel
  abundance$WPPrel.perc.CSRrel = abundance$WPPrel.minus.CSRrel  / abundance$CSRrel
  abundance$WPPrel.div.CSRrel[which(abundance$CSRrel==0)] = NA
  abundance$WPPrel.perc.CSRrel[which(abundance$CSRrel==0)] = NA
  
  write.table(abundance, paste(outputDataPath, "Abundance_Comparison.csv", sep=""), row.names = FALSE, sep = ",")
  
  
  ##### COMMUNITY OVERLAP ANALYSIS #####
  incidence.summary = count.summary
  incidence.summary[4:ncol(incidence.summary)] = as.numeric(count.summary[4:ncol(count.summary)] > 0)
  incidence.summary[which(incidence.summary$system=="WPP"),4:ncol(incidence.summary)] = incidence.summary[which(incidence.summary$system=="WPP"),4:ncol(incidence.summary)] * 10
  incidence.overlap = summaryBy(list(c(INSECTS), c("year", "month")), data=incidence.summary, FUN = sum, na.rm = TRUE, keep.names = TRUE)

  cats = incidence.overlap[,1:2]
  incidence.overlap = sapply(incidence.overlap, as.factor)
  
  incendence.table = apply(as.matrix(incidence.overlap[,3:ncol(incidence.overlap)]), 1, table)
  
  ios = cats
  ios = rbind(ios,ios,ios)
  ios$metric = rep(c("WPP.only", "CSR.only", "both"), each = nrow(cats))
  ios$num = c(incendence.table[3,], incendence.table[2,], incendence.table[4,])
  
  ios.tots = summaryBy(num ~ year + month, data = ios, FUN = sum)
  ios.rel = ios
  for(i in 1:nrow(ios.rel)){
    ios.rel$num[i] = ios.rel$num[i] / ios.tots$num.sum[which(ios.tots$year == ios.rel$year[i] & ios.tots$month == ios.rel$month[i] )] * 100
  }
  
  pdf(paste(outputPlotPath, "Community_Overlap.pdf",sep=""))
  barchart( num ~ as.factor(month) | as.factor(year), 
            data=ios, 
            groups=metric, 
            stack=T, 
            horizontal=F, 
            auto.key=T, 
            main = list("Community Overlap", cex = 2.2), 
            xlab = list("Month", cex = 2), 
            ylab = list("Number of Species", cex = 2), 
            scales = list(cex = 1.05),
            par.strip.text = list(cex = 1.2),
            col = c("darkred","black","red"),
            aspect = 1/2,
            par.settings = list(strip.background=list(col="lightgrey")), 
            key = list(space = "top",
                       rectangles = list(col=c("darkred","black","red")),
                       text = list(c("BOTH", "CSR ONLY", "WPP ONLY"))))
  dev.off()
  
  pdf(paste(outputPlotPath, "Community_Overlap_Perc.pdf",sep=""))
  barchart( num ~ as.factor(month) | as.factor(year), 
            data=ios.rel, 
            groups=metric, 
            stack=T, 
            horizontal=F, 
            auto.key=T, 
            main = list("Community Overlap", cex = 2.2), 
            xlab = list("Month", cex = 2), 
            ylab = list("Number of Species (%)", cex = 2), 
            scales = list(cex = 1.05),
            par.strip.text = list(cex = 1.2),
            col = c("darkred","black","red"),
            aspect = 1/2,
            par.settings = list(strip.background=list(col="lightgrey")), 
            key = list(space = "top",
                       rectangles = list(col=c("darkred","black","red")),
                       text = list(c("BOTH", "CSR ONLY", "WPP ONLY"))))
 dev.off()
 
#plot(ios.rel$num[which(ios.rel$metric=="both")], type = "b")

}