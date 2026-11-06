# Offline representative-date solar approximation, for map appearance only.
# NOAA general solar equations (fractional-year approximation):
# https://gml.noaa.gov/grad/solcalc/solareqns.PDF
# All dates/times use Melbourne local civil time, including daylight saving.
main_map_solar_times <- function(month, latitude = -37.8136, longitude = 144.9631) {
  stopifnot(length(month)==1L, grepl('^[0-9]{4}-(0[1-9]|1[0-2])$',month))
  date <- as.Date(paste0(month,'-15'))
  year <- as.integer(format(date,'%Y'))
  leap <- (year %% 4==0 && year %% 100!=0) || year %% 400==0
  gamma <- 2*pi/(if(leap)366 else 365)*(as.integer(format(date,'%j'))-1)
  eqtime <- 229.18*(0.000075+0.001868*cos(gamma)-0.032077*sin(gamma)-
    0.014615*cos(2*gamma)-0.040849*sin(2*gamma))
  decl <- 0.006918-0.399912*cos(gamma)+0.070257*sin(gamma)-
    0.006758*cos(2*gamma)+0.000907*sin(2*gamma)-
    0.002697*cos(3*gamma)+0.00148*sin(3*gamma)
  lat <- latitude*pi/180
  ha <- acos(cos(90.833*pi/180)/(cos(lat)*cos(decl))-tan(lat)*tan(decl))*180/pi
  local_noon <- as.POSIXct(paste(date,'12:00:00'),tz='Australia/Melbourne')
  offset <- format(local_noon,'%z',tz='Australia/Melbourne')
  tz_minutes <- (as.integer(substr(offset,2,3))*60+as.integer(substr(offset,4,5)))*
    if(substr(offset,1,1)=='-')-1 else 1
  noon <- 720-4*longitude-eqtime+tz_minutes
  list(date=as.character(date),sunrise=noon-4*ha,sunset=noon+4*ha,
    timezone='Australia/Melbourne',utc_offset_minutes=tz_minutes)
}

main_map_light_frame <- function(month,hour) {
  stopifnot(length(hour)==1L,is.finite(hour),hour %in% 0:23)
  solar <- main_map_solar_times(month)
  # Counts represent a whole hour. Its midpoint determines the visual phase.
  minute <- as.integer(hour)*60+30
  preset <- if(minute < solar$sunrise-45 || minute >= solar$sunset+45) 'night' else
    if(minute < solar$sunrise+45) 'dawn' else
    if(minute < solar$sunset-45) 'day' else 'dusk'
  format_minute <- function(x) sprintf('%02d:%02d',as.integer(round(x))%/%60,as.integer(round(x))%%60)
  list(preset=preset,month=month,hour=as.integer(hour),
    representativeDate=solar$date,sunrise=format_minute(solar$sunrise),
    sunset=format_minute(solar$sunset),timezone=solar$timezone)
}
