NicheMapR <- function(niche) {
  unlist(niche)
  
  errors<-0
  
  # error trapping - originally inside the Fortran code, but now checking before executing Fortran
  if(sitemethod%in%c(0,1)==FALSE){
    cat("ERROR: the variable 'sitemethod' be either 0 or 1.
        Please correct.", '\n')
    errors<-1
  }
  if(DEP[2]-DEP[1]>3 | DEP[3]-DEP[2]>3){
    cat("warning, nodes might be too far apart near the surface, try a different spacing if the program is crashing \n")
  }
  if(longlat[1]>180 | longlat[2] > 90){
    cat("ERROR: Latitude or longitude (longlat) is out of bounds.
        Please enter a correct value.", '\n')
    errors<-1
  }
  if(EC<0.0034 | EC > 0.058){
    cat("ERROR: the eccentricity variable (EC) is out of bounds.
        Please enter a correct value (0.0034 - 0.058).", '\n')
    errors<-1
  }
  if(RUF<0.0001){
    cat("ERROR: The roughness height (RUF) is too small ( < 0.0001).
        Please enter a larger value.", '\n')
    errors<-1
  }
  if(RUF>2){
    cat("ERROR: The roughness height (RUF) is too large ( > 2).
        Please enter a smaller value.", '\n')
    errors<-1
  }
  if(DEP[1]!=0){
    cat("ERROR: First soil node (DEP[1]) must = 0 cm.
        Please correct", '\n')
    errors<-1
  }
  if(length(DEP)!=10){
    cat("ERROR: You must enter 10 different soil depths.", '\n')
    errors<-1
  }
  for(i in 1:9){
    if(DEP[i+1]<=DEP[i]){
      cat("ERROR: Soil depth (DEP array) is not in ascending size", '\n')
      errors<-1
    }
  }
  if(DEP[10]>500){
    cat("ERROR: Deepest soil depth (DEP array) is too large (<=500 cm)", '\n')
    errors<-1
  }  
  if(Thcond<0){
    cat("ERROR: Thermal variable conductivity (THCOND) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(Density<0){
    cat("ERROR: Density variable (Density) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(SpecHeat<0){
    cat("ERROR: Specific heat variable (SpecHeat) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(BulkDensity<0){
    cat("ERROR: Bulk density value (BulkDensity) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(Clay<0){
    cat("ERROR: Clay density value (Clay) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(SoilMoist<0 | SoilMoist >1){
    cat("ERROR: Soil moisture value (SoilMoist) is out of bounds.
        Please input a value between 0 and 1.", '\n')
    errors<-1
  }
  if(REFL<0 | REFL>1){
    cat("ERROR: Soil reflectivity value (REFL) is out of bounds.
        Please input a value between 0 and 1.", '\n')
    errors<-1
  }
  if(slope<0 | slope>90){
    cat("ERROR: Slope value (slope) is out of bounds.
        Please input a value between 0 and 90.", '\n')
    errors<-1
  }
  if(aspect<0 | aspect>365){
    cat("ERROR: Aspect value (aspect) is out of bounds.
        Please input a value between 0 and 365.", '\n')
    errors<-1
  }
  if(max(hori)>90 | min(hori)<0){
    cat("ERROR: At least one of your horizon angles (hori) is out of bounds.
        Please input a value between 0 and 90", '\n')
    errors<-1
  }
  if(length(hori)!=24){
    cat("ERROR: You must enter 24 horizon angle values.", '\n')
    errors<-1
  }
  if(SLE<0.5 | SLE > 1){
    cat("ERROR: Emissivity (SLE) is out of bounds.
        Please enter a correct value (0.05 - 1.00).", '\n')
    errors<-1
  }
  if(ERR<0){
    cat("ERROR: Error bound (ERR) is too small.
        Please enter a correct value (> 0.00).", '\n')
    errors<-1
  }
  if(Usrhyt<RUF){
    cat("ERROR: Reference height (Usrhyt) smaller than roughness height (RUF).
        Please use a larger height above the surface.", '\n')
    errors<-1
  }
  if(Usrhyt<0.5 | Usrhyt>120){
    cat("ERROR: Reference height (Usrhyt) is out of bounds.
        Please enter a correct value (0.05 - 120).", '\n')
    errors<-1
  }
  if(CMH2O<0.5 | CMH2O>120){
    cat("ERROR: Preciptable water in air column (CMH2O) is out of bounds.
        Please enter a correct value (0.1 - 2).", '\n')
    errors<-1
  }
  if(max(TIMAXS)>24 | min(TIMAXS)<0){
    cat("ERROR: At least one of your times of weather maxima (TIMAXS) is out of bounds.
        Please input a value between 0 and 24", '\n')
    errors<-1
  }
  if(max(TIMINS)>24 | min(TIMINS)<0){
    cat("ERROR: At least one of your times of weather minima (TIMINS) is out of bounds.
        Please input a value between 0 and 24", '\n')
    errors<-1
  }
  if(minshade>maxshade | minshade==maxshade){
    cat("ERROR: Your value for minimum shade (minshade) is greater than or equal to the maximum shade (maxshade).
        Please correct this.", '\n')
    errors<-1
  }  
  if(minshade>100 | minshade<0){
    cat("ERROR: Your value for minimum shade (minshade) is out of bounds.
        Please input a value between 0 and 100.", '\n')
    errors<-1
  }    
  if(maxshade>100 | maxshade<0){
    cat("ERROR: Your value for maximum shade (maxshade) is out of bounds.
        Please input a value between 0 and 100.", '\n')
    errors<-1
  }   
  if(write_input%in%c(0,1)==FALSE){
    cat("ERROR: the variable 'write_input' be either 0 or 1.
        Please correct.", '\n')
    errors<-1
  }
  # end error trapping
  if(errors==0){ # continue
    
    ################## loading packages ###################################
    if(require("zoo")){
      print("zoo is loaded correctly")
    } else {
      print("trying to install zoo")
      install.packages("zoo")
      if(require(zoo)){
        print("zoo installed and loaded")
      } else {
        stop("could not install zoo")
      }
    }
    if(require("ncdf")){
      print("ncdf is loaded correctly")
    } else {
      print("trying to install ncdf")
      install.packages("ncdf")
      if(require(ncdf)){
        print("ncdf installed and loaded")
      } else {
        stop("could not install ncdf")
      }
    }
    
    if(require("XML")){
      print("XML is loaded correctly")
    } else {
      print("trying to install XML")
      install.packages("XML")
      if(require(XML)){
        print("XML installed and loaded")
      } else {
        stop("could not install XML")
      }
    }
    
    if(require("dismo")){
      print("dismo is loaded correctly")
    } else {
      print("trying to install dismo")
      install.packages("dismo")
      if(require(dismo)){
        print("dismo installed and loaded")
      } else {
        stop("could not install dismo")
      }
    }
    
    if(require("chron")){
      print("chron is loaded correctly")
    } else {
      print("trying to install chron")
      install.packages("chron")
      if(require(chron)){
        print("chron installed and loaded")
      } else {
        stop("could not install chron")
      }
    }
    
    if(require("rgdal")){
      print("rgdal is loaded correctly")
    } else {
      print("trying to install rgdal")
      install.packages("rgdal")
      if(require(rgdal)){
        print("rgdal installed and loaded")
      } else {
        stop("could not install rgdal")
      }
    }
    
    ################## end loading packages ###################################
    
    timeinterval<-365 # number of time intervals to generate predictions for over a year (must be 12 <= x <=365)
    tzone<-paste("Etc/GMT-12",sep="") # doing it this way ignores daylight savings!
    dim<-length(seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days"))
    juldays12<-c(15.,46.,74.,105.,135.,166.,196.,227.,258.,288.,319.,349.)
    juldaysn<-juldays12
    if(nyears>1){ # create sequence of days for splining across multiple years
      for(i in 1:(nyears-1)){
        juldaysn<-c(juldaysn,(juldays12+365*i))
      }
    }
    daystart<-1
    dates<-Sys.time()-60*60*24
    curyear<-as.numeric(format(dates,"%Y"))
    REFL<-rep(REFL,dim) # soil reflectances
    SoilMoist<-rep(SoilMoist,dim)
    Density<-Density/1000 # density of minerals - convert to Mg/m3
    BulkDensity<-BulkDensity/1000 # density of minerals - convert to Mg/m3
    if(soildata==0){
      soilprop<-cbind(0,0)
      maxshades <- rep(maxshade,dim)
      minshades <- rep(minshade,dim)
    }
    pctwet_mult<-0#0.01 # factor by which uppper soil wetness is multiplied to get surface %wet for evaporative cooling
    
    adiab_cor<-1 # correct for lapse rate
    microdaily<-1 # run microclimate model where one iteration of each day occurs and last day gives initial conditions for present day
    if(sitemethod==1){
      longlat <- geocode(loc)[3:4] # assumes first geocode match is correct
    }
    x <- rbind(longlat) # get long/lat in a form usable by the geocode and extract function
    
    
    
    #get UTM from dec degrees
    #UTMzone<-(floor((x[1] + 180)/6) %% 60) 
    #if(sign(x[2])<0){hemisph<-'south'}else{hemisph<-'north'}
    #utm<-project(x, paste("+proj=NZTM +",hemisph," +zone=",UTMzone," ellps=WGS84",sep=""))
    
    
    r1<-raster(paste(spatial,'nz_geo3_km.asc',sep=""))
    NZDEM<-extract(r1,x)*1000
    
    utm<-project(as.matrix(x), "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m")
    nc<-open.ncdf(paste(spatial,"elevslpasphori.nc",sep=""))
    easting<-get.var.ncdf(nc,"easting")
    northing<-get.var.ncdf(nc,"northing")
    dist1<-abs(easting-utm[1])
    index1<-which.min(dist1)
    dist2<-abs(northing-utm[2])
    index2<-which.min(dist2)
    start<-c(index1,index2,1)
    count<-c(1,1,-1)
    elevslpasphori<-as.numeric(get.var.ncdf(nc,varid="variable",start=start,count=count))
    close.ncdf(nc) 
    
    ALTITUDES <- elevslpasphori[1]
    if(is.na(ALTITUDES)==TRUE){ALTITUDES<-NZDEM}
  
    if(terrain==1){
      cat("extracting terrain data")
      
      # now extract terrain data from elevslpasphori.nc
      # get UTM from dec degrees, NZTM
      HORIZONS <- elevslpasphori[4:27]
      SLOPES <- elevslpasphori[2]
      AZMUTHS <- elevslpasphori[3]
      # the horizons have been arranged so that they go from 0 degrees azimuth (north) clockwise - r.horizon starts
      # in the east and goes counter clockwise!
      HORIZONS <- (ifelse(is.na(HORIZONS),0,HORIZONS))/10 # get rid of na and get back to floating point
      HORIZONS <- data.frame(HORIZONS)
    }else{
      HORIZONS <- hori
      HORIZONS <- data.frame(HORIZONS)
      VIEWF_all <- rep(1,length(x[,1]))
      SLOPES<-rep(slope,length(x[,1]))
      AZMUTHS<-rep(aspect,length(x[,1]))
    } 
    hori<-HORIZONS
    row.names(hori)<-NULL
    hori<-as.numeric(as.matrix(hori))
    
    if(soildata==1){
      VIEWF<-VIEWF_all
      SLES<-SLES2
    }else{
      VIEWF<-VIEWF_all
    }
    
    # setting up for temperature correction using lapse rate given difference between 9sec DEM value and 0.05 deg value
#     if(NZDEM==-9999 | is.na(NZDEM)=='TRUE'){
#       delta_elev = AGG - ALTITUDES
#     }else{
       delta_elev = NZDEM - ALTITUDES
#     }
    adiab_corr = delta_elev * 0.0058 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
    adiab_corr_max = delta_elev * 0.0077 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
    adiab_corr_min = delta_elev * 0.0039 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
    
    # read daily weather
    yearlist<-seq(ystart,(ystart+(nyears-1)),1)
    for(j in 1:nyears){ # start loop through years
      cat(paste('reading weather input for ',yearlist[j],' \n',sep=""))
      lon_1<-as.numeric(longlat[1])
      lat_1<-as.numeric(longlat[2])
      lat<-read.csv('ncdf_lat.csv')[,2]
      lon<-read.csv('ncdf_lon.csv')[,2]
      dist1<-abs(lon-lon_1)
      index1<-which.min(dist1)
      dist2<-abs(lat-lat_1)
      index2<-which.min(dist2)
      start<-c(index1,index2,1)
      count<-c(1,1,-1)
      
      if(j==1){
        Tmax<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Tmax.nc',sep="")),varid="variable",start=start,count))
        Tmin<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Tmin.nc',sep="")),varid="variable",start=start,count))
        VP<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_VP.nc',sep="")),varid="variable",start=start,count))
        Rain<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Rain.nc',sep="")),varid="variable",start=start,count))
        SoilM<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_SoilM.nc',sep="")),varid="variable",start=start,count))
        Wind<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Wind.nc',sep="")),varid="variable",start=start,count))
        clear<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,'clearsky.nc',sep="")),varid="variable",start=start,count))
        Rad<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Rad.nc',sep="")),varid="variable",start=start,count))
        if(length(Rad)==366){# add day for leap year if needed
          clear<-c(clear[1:59],clear[59],clear[60:365])
        }
        cloud<-(1-Rad/clear)*100  
        cloud[cloud<0]<-0
        cloud[cloud>100]<-100
        CCMAXX<-as.numeric(cloud)
      }else{
        Tmax<-as.numeric(c(Tmax,get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Tmax.nc',sep="")),varid="variable",start=start,count)))
        Tmin<-as.numeric(c(Tmin,get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Tmin.nc',sep="")),varid="variable",start=start,count)))
        VP<-as.numeric(c(VP,get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_VP.nc',sep="")),varid="variable",start=start,count)))
        Rain<-as.numeric(c(Rain,get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Rain.nc',sep="")),varid="variable",start=start,count)))
        SoilM<-as.numeric(c(SoilM,get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_SoilM.nc',sep="")),varid="variable",start=start,count)))
        Wind<-as.numeric(c(Wind,get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Wind.nc',sep="")),varid="variable",start=start,count)) )      
        Rad<-as.numeric(get.var.ncdf(open.ncdf(paste(spatial,yearlist[j],'_Rad.nc',sep="")),varid="variable",start=start,count))
        if(length(Rad)==366){# add day for leap year if needed
          clear<-c(clear[1:59],clear[59],clear[60:365])
        }
        cloud<-(1-Rad/clear)*100  
        cloud[cloud<0]<-0
        cloud[cloud>100]<-100
        CCMAXX<-as.numeric(c(CCMAXX,cloud))
      }
    } 
    
    CCMINN<-CCMAXX    
    Wind[Wind==0]<-0.1
    
    ndays<-length(Tmax)
    julnum<-ndays
    juldays<-seq(daystart,julnum,1)
    julday <- subset(juldays, juldays!=0)
    #julday<-rep(julday,nyears)
    ida<-ndays
    idayst <- 1 # start month
    # end preliminary test for incomplete year, if simulation includes the present year 
    
    if((soildata==1 & nrow(soilprop)>0)|soildata==0){
      
      if(soildata==1){
        # get static soil data into arrays
        REFL <- static_soil_vars[,1]  # albedo/reflectances
        maxshades <- static_soil_vars[,2:13] # assuming FAPAR represents shade
        shademax<-maxshades
        
      }else{
        if(manualshade==0){
          maxshades <- static_soil_vars[,2:13] # assuming FAPAR represents shade
        }
        shademax<-maxshades
      }
      
      if(is.na(ALTITUDES)!=TRUE){ 
        
        
        ####### get solar attenuation due to aerosols with program GADS #####################
        maindir<-getwd()
        setwd('../micro_global/')
        ####### get solar attenuation due to aerosols with program GADS #####################
        lat5s<-seq(-90,90,5) #lat range for GADS
        lon5s<-seq(-180,175,5) #long range for GADS
        lat5<-(45)# lat5s[which.min(abs(lat5s-x[2]))]
        lon5<-(-90)#lon5s[which.min(abs(lon5s-x[1]))]
        relhum<-1.
        season<-0.
        gadin<-list(lat5=lat5,lon5=lon5,relhum=relhum,season=season)
        source('gads/gads.R')
        gadout<-gads(gadin)
        optdep.summer<-as.data.frame(gadout$optdep)
        season<-1.
        gadin<-list(lat5=lat5,lon5=lon5,relhum=relhum,season=season)
        gadout<-gads(gadin)
        optdep.winter<-as.data.frame(gadout$optdep)
        optdep<-cbind(optdep.winter[,1],rowMeans(cbind(optdep.summer[,2],optdep.winter[,2])))
        optdep<-as.data.frame(optdep)
        colnames(optdep)<-c("LAMBDA","OPTDEPTH")
        a<-lm(OPTDEPTH~poly(LAMBDA, 6, raw=TRUE),data=optdep)
        LAMBDA<-c(290,295,300,305,310,315,320,330,340,350,360,370,380,390,400,420,440,460,480,500,520,540,560,580,600,620,640,660,680,700,720,740,760,780,800,820,840,860,880,900,920,940,960,980,1000,1020,1080,1100,1120,1140,1160,1180,1200,1220,1240,1260,1280,1300,1320,1380,1400,1420,1440,1460,1480,1500,1540,1580,1600,1620,1640,1660,1700,1720,1780,1800,1860,1900,1950,2000,2020,2050,2100,2120,2150,2200,2260,2300,2320,2350,2380,2400,2420,2450,2490,2500,2600,2700,2800,2900,3000,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000)
        TAI<-predict(a,data.frame(LAMBDA))
        setwd(maindir) #getting back to working directory
        ################ end GADS ################################################## 
        
        
        
        
        if(adiab_cor==1){
          TMAXX<-as.matrix(Tmax+adiab_corr_max)
          TMINN<-as.matrix(Tmin+adiab_corr_min)
        }
        RAINFALL<-Rain

        VAPRES<-VP*100 # convert from hectopascals to pascals
        TMAXK<-TMAXX+273.15
        loge<-TMAXK
        loge[loge>273.16]<- -7.90298*(373.16/TMAXK-1.)+5.02808*log10(373.16/TMAXK)-1.3816E-07*(10.^(11.344*(1.-TMAXK/373.16))-1.)+8.1328E-03*(10.^(-3.49149*(373.16/TMAXK-1.))-1.)+log10(1013.246)
        loge[loge<=273.16]<- -9.09718*(273.16/TMAXK-1.)-3.56654*log10(273.16/TMAXK)+.876793*(1.-TMAXK/273.16)+log10(6.1071)
        estar<-(10.^loge)*100. 
        RHMINN<-(VAPRES/estar)*100
        RHMINN[RHMINN>100]<-100
        RHMINN[RHMINN<0]<-0.01
        #RHMINN
        TMINK<-TMINN+273.15
        loge<-TMINK
        loge[loge>273.16]<- -7.90298*(373.16/TMINK-1.)+5.02808*log10(373.16/TMINK)-1.3816E-07*(10.^(11.344*(1.-TMINK/373.16))-1.)+8.1328E-03*(10.^(-3.49149*(373.16/TMINK-1.))-1.)+log10(1013.246)
        loge[loge<=273.16]<- -9.09718*(273.16/TMINK-1.)-3.56654*log10(273.16/TMINK)+.876793*(1.-TMINK/273.16)+log10(6.1071)
        estar<-(10.^loge)*100. 
        RHMAXX<-(VAPRES/estar)*100
        RHMAXX[RHMAXX>100]<-100
        RHMAXX[RHMAXX<0]<-0.01
        
        ALLMINTEMPS<-TMINN
        ALLMAXTEMPS<-TMAXX
        ALLTEMPS <- cbind(ALLMAXTEMPS,ALLMINTEMPS)
        
        WNMAXX <- Wind
        WNMINN <- Wind  
        
        
        if(soildata==1){
          #           uppermoist1<-spline(juldaysn2,moistupper,n=ndays,xmin=1,xmax=ndays,method="periodic")
          #           lowermoist1<-spline(juldaysn2,moistlower,n=ndays,xmin=1,xmax=ndays,method="periodic")
          #           uppermoists<-uppermoist1$y
          #           lowermoists<-lowermoist1$y
          
          SLES1<-spline(juldays12,SLES,n=timeinterval,xmin=1,xmax=365,method="periodic")
          SLES<-rep(SLES1$y,nyears)
          SLES<-SLES[1:ndays]
          maxshades1 <-spline(juldays12,shademax,n=timeinterval,xmin=1,xmax=365,method="periodic")
          MAXSHADES<-rep(maxshades1$y*100,nyears)
          MAXSHADES<-MAXSHADES[1:ndays]
          if(manualshade==1){
            maxshades <- rep(maxshade,365)
            maxshades <- rep(maxshades,nyears)
            MAXSHADES<-maxshades
            minshades <- rep(minshade,365)
            minshades <- rep(minshades,nyears)
            MINSHADES<-minshades
          }
        }else{
          if(manualshade==0){
            maxshades1 <-spline(juldays12,shademax,n=timeinterval,xmin=1,xmax=365,method="periodic")
            MAXSHADES<-rep(maxshades1$y*100,nyears)
            minshades <- rep(minshade,365)
            minshades <- rep(minshades,nyears)
            MINSHADES<-minshades
          }else{
            MAXSHADES<-maxshades
            MINSHADES<-minshades
          }
        }
        
        REFLS <- (1:(dim))*0+REFL
        if((soildata==1)&(length(RAINFALL)>0)){
          soilwet<-RAINFALL
          soilwet[soilwet<=rainwet] = 0 
          soilwet[soilwet>0] = 90
          #PCTWET <- uppermoists*pctwet_mult
          #PCTWET <- uppermoists*soilprop$A_01bar*pctwet_mult*100
          PCTWET<-pmax(soilwet,PCTWET)
        }else{
          REFLS <- (1:(dim))*0+REFL
          PCTWET <- (1:(dim))*0+PCTWET
          soilwet<-RAINFALL
          soilwet[soilwet<=rainwet] = 0 
          soilwet[soilwet>0] = 90
          PCTWET<-pmax(soilwet,PCTWET)
        }
        
        
        
        
        
        #         if(soildata==1){
        #           # extra code for soil moisture start 
        #           Intrvls <-(1:julnum) # user-supplied last Julian day in each time interval sequence
        #           Numint <- julnum  # number of time intervals
        #           Numtyps <- 4
        #           depinterval<-findInterval(upperdep*100, DEP)
        #           deepnode1<-depinterval
        #           depinterval<-findInterval(lowerdep*100, DEP)
        #           deepnode2<-depinterval
        #           deepnode3<-10
        #           toprow<-rep(deepnode1,julnum)
        #           middlerow<-rep(deepnode2,julnum)
        #           bottomrow<-rep(deepnode3,julnum)
        #           Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # deepest nodes for each substrate type
        #           Nodes[1,1:julnum]<-3
        #           Nodes[2,1:julnum]<-toprow
        #           Nodes[3,1:julnum]<-middlerow
        #           Nodes[4,1:julnum]<-bottomrow
        #         }else{
        Intrvls<-rep(0,dim)  
        Intrvls[1] <- 1 # user-supplied last Julian day in each time interval sequence
        Numtyps <- 1 # number of substrate types
        Numint <- 1  # number of time intervals
        Nodes <- matrix(data = 0, nrow = 10, ncol = dim) # deepest nodes for each substrate type
        Nodes[1,1] <- 10. # deepest nodes for each substrate type
        #         }
        
        
        ALREF <- abs(trunc(x[1]))
        
        
        HEMIS <- ifelse(x[2]<0,2.,1.) 
        ALAT <- abs(trunc(x[2]))
        AMINUT <- (abs(x[2])-ALAT)*60
        ALONG <- abs(trunc(x[1]))
        ALMINT <- (abs(x[1])-ALONG)*60
        if(adiab_cor==1){
          ALTT<-ALTITUDES
        }else{
          ALTT<-NZDEM
        }
        SLOPE<-SLOPES
        AZMUTH<-AZMUTHS 
        
        avetemp<-(sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2)
        soilinit<-rep(avetemp,length(DEP))
        tannul<-mean(unlist(ALLTEMPS))
        
        if(nyears==1){
          avetemp<-(sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2)
          tannulrun<-rep(avetemp,365)
        }else{
          if(nrow(TMAXX)==1){
            avetemp<-colMeans(cbind(TMAXX, TMINN), na.rm=TRUE)
          }else{
            avetemp<-rowMeans(cbind(TMAXX, TMINN), na.rm=TRUE)
          }
          #library("TTR")
          #tannulrun<-SMA(avetemp,n=365)
          if(length(TMAXX)<365){
            tannulrun<-rep((sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2),length(TMAXX))
          }else{
            tannulrun<-movingFun(avetemp,n=365,fun=mean,type='to')
            yearone<-rep((sum(TMAXX[1:365])+sum(TMINN[1:365]))/(365*2),365)
            tannulrun[1:365]<-yearone
            # SST
          }
        }
        
        
        # correct for fact that wind is measured at 10 m height
        # wind shear equation v / vo = (h / ho)^a
        #where
        #v = the velocity at height h (m/s)
        #vo = the velocity at height ho (m/s)
        #a = the wind shear exponent
        #Terrain   Wind Shear Exponent
        #- a -
        #  Open water   0.1
        #Smooth, level, grass-covered   0.15
        #Row crops 	0.2
        #Low bushes with a few trees 	0.2
        #Heavy trees 	0.25
        #Several buildings 	0.25
        #Hilly, mountainous terrain 	0.25
        WNMAXX<-WNMAXX*(1.2/2)^0.15
        WNMINN<-WNMINN*(1.2/2)^0.15
        
        
        SNOW <- rep(0,dim) # no snow simulated on surface
        
        # impose uniform warming
        TMAXX<-TMAXX+warm
        TMINN<-TMINN+warm
        
        SLES<-matrix(nrow=dim,data=0) 
        SLES<-SLES+SLE
        
        moists2<-matrix(nrow=10, ncol = ndays, data=0)
        moists2[1,ndays]<-SoilMoist[1]
        moists<-moists2
        
        if(runmoist==1){
          moists2<-matrix(nrow=10, ncol = dim, data=0) # set up an empty vector for soil moisture values through time
          moists2[1:10,]<-SoilMoist_Init
          moists<-moists2
        }
        soilprops<-matrix(data = 0, nrow = 10, ncol = 6)
        soilprops[1,1]<-BulkDensity 
        soilprops[1,2]<-SatWater    
        soilprops[1,3]<-Clay       
        soilprops[1,4]<-Thcond 
        soilprops[1,5]<-SpecHeat        
        soilprops[1,6]<-Density 
        soilprops<-(ifelse(is.na(soilprops),0,soilprops))
        
        
        if(loop>0){
          TMAXX<-c(TMAXX[((loop)*365+1):(nyears*365)],TMAXX[1:((loop)*365)])
          TMINN<-c(TMINN[((loop)*365+1):(nyears*365)],TMINN[1:((loop)*365)])
          RHMAXX<-c(RHMAXX[((loop)*365+1):(nyears*365)],RHMAXX[1:((loop)*365)])
          RHMINN<-c(RHMINN[((loop)*365+1):(nyears*365)],RHMINN[1:((loop)*365)])
          CCMAXX<-c(CCMAXX[((loop)*365+1):(nyears*365)],CCMAXX[1:((loop)*365)])
          CCMINN<-c(CCMINN[((loop)*365+1):(nyears*365)],CCMINN[1:((loop)*365)])
          WNMAXX<-c(WNMAXX[((loop)*365+1):(nyears*365)],WNMAXX[1:((loop)*365)])
          WNMINN<-c(WNMINN[((loop)*365+1):(nyears*365)],WNMINN[1:((loop)*365)])
          PCTWET<-c(PCTWET[((loop)*365+1):(nyears*365)],PCTWET[1:((loop)*365)])
          moists<-cbind(moists[,((loop)*365+1):(nyears*365)],moists[,1:((loop)*365)])
          RAINFALL<-c(RAINFALL[((loop)*365+1):(nyears*365)],RAINFALL[1:((loop)*365)])
          
        }
        fieldcap<-0
        wilting<-0
        # microclimate input parameters listALTT,ALREF,ALMINT,ALONG,AMINUT,ALAT
        ALTT<-as.numeric(ALTT)
        ALREF<-as.numeric(ALREF)
        ALMINT<-as.numeric(ALMINT)
        ALONG<-as.numeric(ALONG)
        AMINUT<-as.numeric(AMINUT)
        ALAT<-as.numeric(ALAT)
        if(snowmodel==1){
          microinput<-c(julnum,RUF,ERR,Usrhyt,Numtyps,Numint,Z01,Z02,ZH1,ZH2,idayst,ida,HEMIS,ALAT,AMINUT,ALONG,ALMINT,ALREF,SLOPE,AZMUTH,ALTT,CMH2O,microdaily,tannul,EC,VIEWF,snowtemp,snowdens,snowmelt,undercatch,fieldcap,wilting,rainmult,rainmult,runshade,rainmelt)
        }else{
          microinput<-c(julnum,RUF,ERR,Usrhyt,Numtyps,Numint,Z01,Z02,ZH1,ZH2,idayst,ida,HEMIS,ALAT,AMINUT,ALONG,ALMINT,ALREF,SLOPE,AZMUTH,ALTT,CMH2O,microdaily,tannul,EC,VIEWF,snowtemp,snowdens,snowmelt,undercatch,rainmult,runshade,runmoist,maxpool,evenrain)
        }
        shore<-0
        if(shore==0){
          tides<-matrix(data = 0., nrow = 24*dim, ncol = 3) # make an empty matrix
        }
        
        micro<-list(dim=dim,tides=tides,microinput=microinput,julday=julday,SLES=SLES,DEP=DEP,Intrvls=Intrvls,Nodes=Nodes,MAXSHADES=MAXSHADES,MINSHADES=MINSHADES,TIMAXS=TIMAXS,TIMINS=TIMINS,TMAXX=TMAXX,TMINN=TMINN,RHMAXX=RHMAXX,RHMINN=RHMINN,CCMAXX=CCMAXX,CCMINN=CCMINN,WNMAXX=WNMAXX,WNMINN=WNMINN,SNOW=SNOW,REFLS=REFLS,PCTWET=PCTWET,soilinit=soilinit,hori=hori,TAI=TAI,soilprops=soilprops,moists=moists,RAINFALL=RAINFALL,tannulrun=tannulrun,PE=PE,KS=KS,BB=BB,BD=BD,L=L,LAI=LAI)
        
        # write all input to csv files in their own folder
        if(write_input==1){
          cat('writing out model input \n')
          write.table(as.matrix(microinput), file = "microclimates/csv input/microinput.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(julday, file = "microclimates/csv input/julday.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(SLES, file = "microclimates/csv input/SLES.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(DEP, file = "microclimates/csv input/DEP.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(Intrvls, file = "microclimates/csv input/Intrvls.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(Nodes, file = "microclimates/csv input/Nodes.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(MAXSHADES, file = "microclimates/csv input/Maxshades.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(MINSHADES, file = "microclimates/csv input/Minshades.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TIMAXS, file = "microclimates/csv input/TIMAXS.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TIMINS, file = "microclimates/csv input/TIMINS.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TMAXX, file = "microclimates/csv input/TMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TMINN, file = "microclimates/csv input/TMINN.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(RHMAXX, file = "microclimates/csv input/RHMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(RHMINN, file = "microclimates/csv input/RHMINN.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(CCMAXX, file = "microclimates/csv input/CCMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(CCMINN, file = "microclimates/csv input/CCMINN.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(WNMAXX, file = "microclimates/csv input/WNMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(WNMINN, file = "microclimates/csv input/WNMINN.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(SNOW, file = "microclimates/csv input/SNOW.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(REFLS, file = "microclimates/csv input/REFLS.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(PCTWET, file = "microclimates/csv input/PCTWET.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(soilinit, file = "microclimates/csv input/soilinit.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(hori, file = "microclimates/csv input/hori.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TAI, file = "microclimates/csv input/TAI.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(soilprops, file="microclimates/csv input/soilprop.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(moists,file="microclimates/csv input/moists.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(RAINFALL,file="microclimates/csv input/rain.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(tannulrun,file="microclimates/csv input/tannulrun.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(PE,file="microclimates/csv input/PE.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(BD,file="microclimates/csv input/BD.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(BB,file="microclimates/csv input/BB.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(KS,file="microclimates/csv input/KS.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(L,file="microclimates/csv input/L.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(LAI,file="microclimates/csv input/LAI.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(tides,file="microclimates/csv input/tides.csv", sep = ",", col.names = NA, qmethod = "double")
        }
        
        
        setwd('../microclimate')
        cat('running microclimate model \n')
        if(mac==1){
          source('microrun_mac.R')
        }else{
          source('microrun.R')
        }
        microut<-microclimate(micro)
        setwd(maindir)
        metout<-microut$metout # retrieve above ground microclimatic conditions, min shade
        shadmet<-microut$shadmet # retrieve above ground microclimatic conditions, max shade
        soil<-microut$soil # retrieve soil temperatures, minimum shade
        shadsoil<-microut$shadsoil # retrieve soil temperatures, maximum shade
        soilmoist<-microut$soilmoist # retrieve soil moisture, minimum shade
        shadmoist<-microut$shadmoist # retrieve soil moisture, maximum shade
        humid<-microut$humid # retrieve soil humidity, minimum shade
        shadhumid<-microut$shadhumid # retrieve soil humidity, maximum shade
        soilpot<-microut$soilpot # retrieve soil water potential, minimum shade
        shadpot<-microut$shadpot # retrieve soil water potential, maximum shade
        # metout/shadmet variables:
        # 1 JULDAY - day of year
        # 2 TIME - time of day (mins)
        # 3 TALOC - air temperature (deg C) at local height (specified by 'Usrhyt' variable)
        # 4 TAREF - air temperature (deg C) at reference height (1.2m)
        # 5 RHLOC - relative humidity (%) at local height (specified by 'Usrhyt' variable)
        # 6 RH  - relative humidity (%) at reference height (1.2m)
        # 7 VLOC - wind speed (m/s) at local height (specified by 'Usrhyt' variable)
        # 8 VREF - wind speed (m/s) at reference height (1.2m)
        # 9 ZEN - zenith angle of sun (degrees - 90 = below the horizon)
        # 10 SOLR - solar radiation (W/m2)
        # 11 TSKYC - sky radiant temperature (deg C)
        # 12 SNOWFALL - snow predicted to have fallen (mm)
        # 13 SNOWDEP - predicted snow depth (cm)
        
        # soil and shadsoil variables:
        # 1 JULDAY - day of year
        # 2 TIME - time of day (mins)
        # 3-12 D0cm ... - soil temperatures at each of the 10 specified depths
        
        
        return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,humid=humid,shadhumid=shadhumid,soilpot=soilpot,shadpot=shadpot,RAINFALL=RAINFALL,ALTT=ALTT,REFL=REFL[1],fieldcap=fieldcap,wilting=wilting,MAXSHADES=MAXSHADES,longlat=c(x[1],x[2]),dim=dim))
        
      } # end of check for na sites
    } # end of check if soil data is being used but no soil data returned
    
  } # end error trapping
} # end of NicheMapR_Setup_micro function    
