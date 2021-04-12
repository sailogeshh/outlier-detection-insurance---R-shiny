library(ggplot2)
library(plyr)
library(ggrepel)
library(pdp)
data1=read.csv("new.csv")
log.regr <- function(data,oneplot=FALSE){
  data=data[,-c(2,5,6,8,15,16,20,22,25,26,27,38,39,40)]
  names(data)
  options(scipen=999)
  str(data)
  data$fraud_reported<-as.factor(data$fraud_reported)
  data$bodily_injuries<-as.factor(data$bodily_injuries)
  data$witnesses<-as.factor(data$witnesses)
  str(data)
  table(data$property_damage)
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  Mode(data$property_damage)
  Mode(data$police_report_available)
  DV_name <-names(data[1])
  IV_names <- names(data)[names(data) != DV_name]
  IV_group <- paste(IV_names, collapse="+")
  frm <- as.formula(paste(DV_name, "~", IV_group, sep = ""))
  fit <- glm(frm, data=data, family=binomial(logit))
  print(summary(fit))
  
  
  coeff <-  cbind(coef = coef(fit), confint(fit))
  df.coeff <- data.frame(Predictors=dimnames(coeff)[[1]], Coefficient_Estimate=coeff[,1], cf_lcl=coeff[,2], cf_ucl=coeff[,3], p=c(coef(summary(fit))[,4],"NA"))
  df.coeff$p <- as.numeric(sub(",", ".", levels(df.coeff$p))[df.coeff$p])
  df.coeff$p.value <- ifelse(df.coeff$p < 0.01,"<0.01",ifelse(df.coeff$p < 0.05,"0.01<p<0.05",ifelse(df.coeff$p < 0.1,"0.05<p<0.1", ">0.1")))
  
  p1 <- ggplot(df.coeff, aes(x=Coefficient_Estimate, y=Predictors, color=p.value, label=round(Coefficient_Estimate,3))) + geom_point() + 
    geom_errorbarh(aes(xmax = cf_ucl, xmin = cf_lcl, height = .2)) + geom_text(vjust = 0, nudge_y = 0.08) + geom_vline(xintercept = 0,
                              colour="grey", linetype = "longdash") + theme_bw() + labs(x = "Coefficients", y="Predictors")
  print(coeff)
  
  odds <- cbind(OR = exp(coef(fit)), exp(confint(fit)))
  df.odds <- data.frame(Predictors=dimnames(odds)[[1]], Odds_Ratio=odds[,1], or_lcl=odds[,2], or_ucl=odds[,3], p=c(coef(summary(fit))[,4],"NA"))
  df.odds.to.use <- df.odds[-1,]
  df.odds.to.use$p <- as.numeric(sub(",", ".", levels(df.odds.to.use$p))[df.odds.to.use$p])
  
  df.odds.to.use$p.value <- ifelse(df.odds.to.use$p < 0.01,"<0.01",ifelse(df.odds.to.use$p < 0.05,"0.01<p<0.05", ifelse(df.odds.to.use$p < 0.1, "0.05<p<0.1",">0.1")))
  p2 <- ggplot(df.odds.to.use, aes(x=Odds_Ratio, y=Predictors, color=p.value, label=round(Odds_Ratio,3))) + geom_point() + geom_errorbarh(aes(xmax = or_ucl, xmin = or_lcl, height = .2)) + geom_text(vjust = 0, nudge_y = 0.08) + geom_vline(xintercept = 1, colour="grey", linetype = "longdash") + theme_bw() + labs(x = "Odds ratios", y="Predictors")
  print(odds)
  
  df.diagn <- data.frame(point_lbl=as.numeric(rownames(data)), y=fit$y, pred.prob=fit$fitted.values, res=rstandard(fit), CookDist=cooks.distance(fit), DepVar=as.factor(fit$y), leverage=hatvalues(fit))
  n.of.predictors <- sum(hatvalues(fit))-1 #get the number of parameters (ecluding the intercept); it takes into account the levels of dummy-coded predictors if present
  lev.thresh <- round(3*((n.of.predictors+1)/nrow(data)),3)
  df.diagn$lever.check <- ifelse(df.diagn$leverage>lev.thresh,"lever. not ok","lever. ok")
  obs_per_factor <- count(df.diagn$DepVar) # requires 'plyr'
  U <- wilcox.test(df.diagn$pred.prob ~ df.diagn$DepVar)$statistic
  auc <- round(1-U/(obs_per_factor$freq[1]*obs_per_factor$freq[2]), 3)
  p3 <- ggplot(df.diagn, aes(x=pred.prob, y=y, color=DepVar)) + geom_jitter(position=position_jitter(h=0.1), alpha=0.70) + xlim(0,1) + theme_bw() + labs(x = paste("Predicted probability\n( AUC:", auc, " )"), y="Dependent variable")
  p4 <- ggplot(df.diagn, aes(x=pred.prob, y=res, color=DepVar, label=point_lbl)) + geom_point(aes(size = CookDist), shape=1, alpha=.90) + geom_hline(yintercept = 0, colour="grey", linetype = "longdash") + theme_bw() + geom_text_repel(data = subset(df.diagn, abs(res) > 3 & CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Predicted probability\n(labelled points=residual >|3| AND Cook's dist.>1)", y="Standardized residuals") #requires 'ggrepel'
  p5 <- ggplot(df.diagn, aes(x=pred.prob, y=leverage, color=lever.check)) + geom_point(aes(size = leverage), shape=1, alpha=.90) + geom_hline(yintercept = lev.thresh, colour="grey", linetype = "longdash") + theme_bw() + geom_text_repel(data = subset(df.diagn, leverage > lev.thresh), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = paste("Predicted probability\n( leverage threshold: 3*(k+1)/N=", lev.thresh, " )"), y="Leverage") #requires 'ggrepel'
  p6 <- ggplot(df.diagn, aes(x=pred.prob, y=CookDist, color=DepVar)) + geom_point(aes(size = CookDist), shape=1, alpha=.90) + theme_bw() + geom_text_repel(data = subset(df.diagn, CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Predicted probability\n(labelled points=Cook's dist.>1)", y="Cook's distance") #requires 'ggrepel'
  p7 <- ggplot(df.diagn, aes(x=res, y=leverage, color=DepVar)) + geom_point(aes(size = CookDist), shape=1, alpha=.80) + geom_hline(yintercept = lev.thresh, colour="grey", linetype = "longdash") + theme_bw() + geom_text_repel(data = subset(df.diagn, abs(res) > 3 | leverage > lev.thresh | CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = paste("Standardized residuals\n(labelled points=residual>|3| OR lever.>", lev.thresh,"OR Cook's dist.>1)"), y="Leverage") #requires 'ggrepel'
  p8 <- ggplot(df.diagn, aes(x=res, y=leverage, color=DepVar)) + geom_point(aes(size = CookDist), shape=1, alpha=.80) + geom_hline(yintercept = lev.thresh, colour="grey", linetype = "longdash") + theme_bw() + geom_text_repel(data = subset(df.diagn, abs(res) > 3 | leverage > lev.thresh & CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = paste("Standardized residuals\n(labelled points=residual>|3| OR lever.>", lev.thresh,"AND Cook's dist.>1)"), y="Leverage") #requires 'ggrepel'
  p9 <- ggplot(df.diagn, aes(x=point_lbl, y=res, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, abs(res) > 3), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Observation number\n(labelled points=resid.>|3|)", y="Standardized residuals") #requires 'ggrepel'
  p10 <- ggplot(df.diagn, aes(x=point_lbl, y=leverage, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, leverage > lev.thresh), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = paste("Observation number\n(labelled points=leverage>",lev.thresh,")"), y="Leverage") #requires 'ggrepel'
  p11 <- ggplot(df.diagn, aes(x=point_lbl, y=CookDist, label=point_lbl)) + geom_point() + theme_bw() + geom_text_repel(data = subset(df.diagn, CookDist > 1), aes(label = point_lbl), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x = "Observation number\n(labelled points=Cook's dist.>1)", y="Cook's distance") #requires 'ggrepel'
  
  if (oneplot==TRUE) {
    grid.arrange(p1,p2,p3, p4, p5, p6, p7, p8,p9, p10, p11,  ncol=2) #requires 'gridExtra'
    #grid.arrange(p9, p10, p11, ncol=1) #requires 'gridExtra'
  } else {
    print(p1)
    print(p2)
    print(p3)
    print(p4)
    print(p5)
    print(p6)
    print(p7)
    print(p8)
    grid.arrange(p9, p10, p11, ncol=1) #requires 'gridExtra'
  }
}
