c=======================================================================
      integer*4 function ymd2mjd(iyear, imonth, iday)
c=======================================================================
c
c     ... Convert an input calender YYYY, MM, DD to a Modified Julian
c         Date.
c
c     If the year is greater than 1000 then the it is assumed to
c     contain the full centuries; otherwise 1900 is added.
c
c     This routine is only valid for dates after 1600 Jan 0.
c
c     Input parameters:
c     -----------------
c     iyear   four-digit year (if < 1000, then 1900 is added)
c     imonth  month number of year
c     iday    day number of month
c
c     Local parameters:
c     -----------------
c     days_to_month(12)  number of days from start of year to
c                        each month
c     leap_days          number of leap days we need to include
c     years_from_1600    number of years since 1600 Jan 0.
c     days_from_1600     number of days since 1600 Jan 0.
c     leap_year          indicates that this is a leap year
c
c
      implicit none
c
      integer         iyear, imonth, iday
      integer*4       days_to_month(12), leap_days,
     .                years_from_1600, days_from_1600
      logical         leap_year
c 
      data  days_to_month /   0,  31,  59,  90, 120, 151,
     .                      181, 212, 243, 273, 304, 334 /
c
c     ... start, make sure year is from 1900
c
      if ( iyear.lt.1000 ) iyear = iyear + 1900
c
c     ... now compute number of years since 1600
c
      years_from_1600 = iyear - 1600
c
c     ... now compute number of leap days up to the start of the
c     current year (but not including it)
c
      leap_days =   (years_from_1600 -   1)/  4
     .            - (years_from_1600 +  99)/100
     .            + (years_from_1600 + 399)/400  + 1
c
      if ( years_from_1600 .eq. 0 ) leap_days = leap_days - 1
c
c     ... now see if we are in leap year
c 
      leap_year = .false.
      if (  mod(years_from_1600,  4) .eq. 0     .and.
     .     (mod(years_from_1600,100) .ne. 0 .or.
     .      mod(years_from_1600,400) .eq. 0)       )
     .      leap_year = .true.
c
c     ... now compute number of days since 1600
c
      days_from_1600 = years_from_1600*365  + leap_days +
     .                 days_to_month(imonth) + iday
c 
c     ... add extra day if we are after February and this is a leap year
      if( imonth.gt.2 .and. leap_year ) then
          days_from_1600 = days_from_1600 + 1
      end if
c
c     ... compute the mjd; Note that the MJD of 1600 Jan 0 is -94554
c
      ymd2mjd = -94554.d0 + days_from_1600
c
      return
      end
c      
