

/*
Step 1: Simulate time to event data-set
Distribution is weibull with different parameters
study ends in 12 months so, any observations beyond 12 months 
is censored.
*/

data timetoevent;
 call streaminit (4231);
 do i = 1 to 100;
  array set1{5}  (0,1,1,1,1);
   cnsr = set1[rand("integer",1, dim(set1))];
   subid = 1000 + i;
   treatn = mod(i,2);
   if treatn = 0 then do;
    time = round(rand("weibull", 0.5, 100));
   end;
   
   else do;
    time = round(rand("weibull", 0.5, 600));
   end;
   
  output;
 end;
 
 drop i set11 set12 set13 set14 set15 ;
run;

/*Censor events where simulated time is greater then 12, and cap values at 12*/
data timetoevent;
 set timetoevent;
 if time > 12 then do;
  time = 12;
  cnsr = 0;
 end;
run;

ods graphics;
ods exclude SurvivalPlot HomTests HomStats ProductLimitEstimates
       Quartiles Means CensoredSummary LogrankHomCov WilcoxonHomCov ;

ods trace on;
ods output survivalplot=surv;
ods output HomTests = pvalue;
proc lifetest data=timetoevent plots=(survival);
   time time * cnsr(0);
   strata treatn;
run;
ods trace off;
ods output close;

data surv;
 set surv;
 if _censor_ = 0 then time3 = time;
run;

**** MODIFY THE STYLE TEMPLATE TO GET DESIRED LINES;
ods path sashelp.tmplmst(read) work.templat;


ods pdf 
 file="/home/sp16670/figure0.pdf" style=newblue;
 
Options nodate nonumber;

ods graphics on / reset imagename="figure0" outputfmt=png;
proc template;
  define style newblue / store=work.templat; 
  parent=styles.htmlblue;
  class graph / attrpriority='none'; 
  class GraphData1 / linestyle=3;
  class GraphData1 / linestyle=2;
 end;
run;


proc sql noprint;
 select probchisq into :pval from pvalue
 where test = "Log-Rank";
quit;


proc format;
 value $trt
 "0" = "Control"
 "1" = "Therapy"
 ;
run; 

title1 j=l "Kaplan-Meier Survival Plot";
title2 j=l "Intervention compared with control";

proc template;
   define statgraph kp_plot;
      begingraph / datacontrastcolors=(orange blue);
         layout overlay / 
            xaxisopts=(type=linear label="Months from Study Start"
               linearopts=(viewmin = 0 viewmax = 12 tickvaluesequence=(start=0 end=12 increment=1) ))
            yaxisopts=(type=linear label="Survival Probability" 
               linearopts=(viewmin = 0 viewmax = 1));
            stepplot x=time y=survival / group=stratum name="step32" 
               lineattrs=(thickness=1px);
            scatterplot x=time y=censored /group=stratum markerattrs=(symbol=plus) name="cnsr" LEGENDLABEL="Censored";
            discretelegend "step32";
            entry halign=right "Censored +" / valign=top border=true;
            entry halign=left "Log Rank Pr: &pval." / valign=bottom border=true;
         endlayout;
      endgraph;
   end;
run;

/* Render the graph */
proc sgrender data=surv template=kp_plot;
format stratum $trt.;
run;

ods graphics off;
ods pdf close;



