### Correction for Range Restriction
#rxyr - Validity Coefficient Computed on Restricted Scores
#rxy - Estimated Validity Coefficient without Restriction
#sdu - Standard Deviation of Predictor Scores from Unrestricted Group
#sdr - Standard Deviation of Predicted Scored from Restricted Group

range.correct<-function(rxyr,rxy,sdu,sdr){
  coef<-rxyr*(sdu/sdr)/sqrt(1-(rxyr)+(rxy^2)*((sdu^2)/(sdr^2))
  print(coef)
  return(coef)
}

### Correction for Unreliability of Criterion
#rxy - Validity Coefficient
#ryy - Criterion Reliability Coefficient

rel.correct<-function(rxy,ryy){
  coef<- rxy/(sqrt(ryy))
  print(coef)
  return(coef)
}

### Utility Analysis
#each variable in the function is explained in its name or on page 326 in "Human Resource Selection"

utility<-function(jobapplicants,validity,sdy,avg.app.hired.zscore.over.avg.app.zscore.nothired,numberofapps,assessment.cost){
  expected.gain<-(jobapplicants*validity*sdy*avg.app.hired.zscore.over.avg.app.zscore.nothired)-(numberofapps*assessment.cost)
  print(expected.gain)
  return(expected.gain)
}

### Calculate Band Size and Standard Error of Measurement
#score - a numeric vector of scores or predicted scores on a selection test
#rel - the reliability coefficient of the measure (generally Cronbach's Alpha)

band<-function(score,rel){
  sem<-sd(score,na.rm=T)*(sqrt(1-rel))
  sed<-sem*sqrt(2)*1.96
  print(paste('Standard Error of Measurement: ',sem))
  print(paste('Band Size: ',sed))
  s_list<-list('sem' = sem,'sed(band_size)' = sed)
  return(s_list)
}
