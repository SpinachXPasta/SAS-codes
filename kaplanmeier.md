Kaplan-Meier-template
================

<http://www.sthda.com/english/wiki/survival-analysis-basics#log-rank-test-comparing-survival-curves-survdiff>

``` r
library("dplyr")
library("ggplot2")
library("survival")
```

1.  Simulate data

``` r
set.seed(4231)
cnsr <- sample(c(0,1,1,1,1),100, replace = TRUE) # Randomly sample 100 observation with 1/5 chance of censored
subid <- 1000:1099
treatn <- c(rep(1,50), rep(2,50))
time <- c(rweibull(n = 50, scale = 100, shape = 0.5),rweibull(n = 50, scale = 600, shape = 0.5))
timetoevent <- data.frame(cnsr, subid, treatn,time)
timetoevent$time <- round(timetoevent$time)
timetoevent$time <- ifelse(timetoevent$time == 0, 1, timetoevent$time)
timetoevent$cnsr <- ifelse(timetoevent$time > 12, 0, timetoevent$cnsr)
timetoevent$time <- ifelse(timetoevent$time > 12, 12, timetoevent$time)
```

2.  Model

``` r
model <- survfit(Surv(time, cnsr) ~ treatn, data = timetoevent)
inference <- survdiff(Surv(time, cnsr) ~ treatn, data = timetoevent)
grp <- c(rep(1,model$strata[1]) , rep(2,model$strata[2]))
kp_est <- data.frame(model$time,model$surv, grp , model$lower, model$upper)
kp_est$cnsr <- 0
kp_est$cnsr[
  kp_est$model.time %in% unique(timetoevent$time[timetoevent$cnsr == 1 & 
                                                   timetoevent$treatn == 1]) & kp_est$grp == 1] <- 1
kp_est$cnsr[
  kp_est$model.time %in% unique(timetoevent$time[timetoevent$cnsr == 1 & 
                                                   timetoevent$treatn == 2]) & kp_est$grp == 2] <- 1
pvalue <- round(pchisq(inference$chisq, df=1, lower.tail=FALSE),4)
kp_est$cnsrtime <- NA
kp_est$cnsrprob <- NA
kp_est$cnsrtime[kp_est$cnsr == 1] = kp_est$model.time[kp_est$cnsr == 1]
kp_est$cnsrprob[kp_est$cnsr == 1] = kp_est$model.surv[kp_est$cnsr == 1]
kp_est <- rbind(kp_est,c(0,1,1,1,1,0))
kp_est <- rbind(kp_est,c(0,1,2,1,1,0))
kpcnsr <- kp_est %>% filter(cnsr == 1)
kp_est
```

    ##    model.time model.surv grp model.lower model.upper cnsr cnsrtime  cnsrprob
    ## 1           1  0.8400000   1   0.7442897   0.9480180    1        1 0.8400000
    ## 2           2  0.8200000   1   0.7201354   0.9337133    1        2 0.8200000
    ## 3           3  0.8000000   1   0.6964676   0.9189228    1        3 0.8000000
    ## 4           6  0.8000000   1   0.6964676   0.9189228    0       NA        NA
    ## 5           9  0.7794872   1   0.6724919   0.9035056    1        9 0.7794872
    ## 6          10  0.7384615   1   0.6257761   0.8714386    1       10 0.7384615
    ## 7          12  0.7179487   1   0.6029642   0.8548606    1       12 0.7179487
    ## 8           1  0.9600000   2   0.9071919   1.0000000    1        1 0.9600000
    ## 9           3  0.9400000   2   0.8764252   1.0000000    1        3 0.9400000
    ## 10          8  0.9400000   2   0.8764252   1.0000000    0       NA        NA
    ## 11         12  0.9400000   2   0.8764252   1.0000000    0       NA        NA
    ## 12          0  1.0000000   1   1.0000000   1.0000000    0        0 1.0000000
    ## 13          0  1.0000000   2   1.0000000   1.0000000    0        0 1.0000000

3.  Plot

``` r
ggplot()+geom_step(aes(x = model.time, y = model.surv, color = factor(grp)), data = kp_est, linetype = 2)+
  geom_ribbon(aes(x=model.time, ymin = model.lower, ymax = model.upper, fill = factor(grp)), data = kp_est , alpha = 0.1)+
  scale_x_continuous(limits = c(0, 14), breaks = c(0:14))+scale_y_continuous(limits = c(0, 1))+
  geom_point(aes(x = cnsrtime, y = cnsrprob, color = factor(grp)), data = kpcnsr, shape = 3)+
  annotate("text",x = 2,y = 0.2,label = paste0("P-Value = ",pvalue)) + xlab("Months from Study Start") + ylab("Survival Probability")+labs(fill = "Treatment Group", color = "Treatment Group") + theme_classic()
```

![](kaplanmeier_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
