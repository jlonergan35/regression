#       schedule function
apply.schedule <- function(dat,sch) {       
  dat$Day <- strftime(dat$Date,'%A')
  dn <- strftime(seq(1,7,1)*60*60*24 + Sys.time(),'%A')
  for(i in dn) {
    for(j in seq(1,length(index(sch)))) {
      
      start_hour <- sch[paste(i,".Start",sep="")][j,]
      end_hour   <- sch[paste(i,".End",sep="")][j,]
      start_date <- sch$Start.Date[j]
      end_date   <- sch$End.Date[j] + 24 * 60 * 59
      window     <- sch[paste(i,".Window",sep="")][j,]
      name 	   <- as.integer(sch["Schedule.Name"][j,])
      
      dat$Schedule[with(dat,Date >= start_date & Date <= end_date)] <- name 
      if(window == 1) {
        dat$Occ[with(dat,Date >= start_date & Date <= end_date & Day == i & hour >= start_hour & hour < end_hour)] <- 1 
      } else if(window == 0) {
        if (start_hour == 0) {
          dat$Occ[with(dat,Date >= start_date & Date <= end_date & Day == i & hour < start_hour)] <- 1 
        } else {
          dat$Occ[with(dat,Date >= start_date & Date <= end_date & Day == i & hour < start_hour)] <- 1 
        }
        dat$Occ[with(dat,Date >= start_date & Date <= end_date & Day == i & hour > end_hour)] <- 1 
      } #if
    } #for j
  } # for i
  
  dat$Occ[is.na(dat$Occ)] <- 0
  dat$Occ <- as.factor(dat$Occ)
  dat$Schedule <- as.factor(dat$Schedule)
  dat$Day <- as.factor(dat$Day)
  return(dat) # return
}

#function to calculate temp bins for regression
temperature_bounds <- function(min_temp,max_temp,segs) {
  
  ifelse(segs < 2,stop("segments must be greater than 1"),T)
  B <- quantile(c(min_temp,max_temp),probs=seq(0,100,100/(segs-1))/100,names=F)
  return(B)
}

component_temperatures <- function(temp,min_temp,max_temp,segs) {
  
  
  # create a matrix to hold results
  ma<-matrix(0,nrow=1,ncol=segs)
  
  # split temp range into n-1 segments
  b <- (max_temp-min_temp)/(segs/2)
  B <- temperature_bounds(min_temp+b,max_temp-b,segs)
  
  if(is.na(temp) || is.null(temp)) {
    return(ma)
  }
  
  # 1) 
  if( temp > B[1] ) {
    ma[1] <- B[1]
  } else {
    ma[1] <-temp
    ma[2:segs] <- 0
    return(ma)
  }
  
  # 2)
  for( n in 2:segs ) {
    if( temp > B[n] ) {
      ma[n] <- B[n] - B[n-1]
    } else {
      ma[n] <- temp - B[n-1]
      ma[(n+1):segs] <- 0
      return(ma)
    }
  }
  
  # 3)
  if(temp > B[segs-1]) {
    ma[segs-1] <- B[segs-1] - B[segs-2]
    ma[segs] <- temp - B[segs-1]
  }
  return(ma)
}