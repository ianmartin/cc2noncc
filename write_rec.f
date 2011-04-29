c=======================================================================
      subroutine write_rec(ou, fmt_vers, nobs, itime, sec, msec, flag,
     +                     numsat, prn, char, clockerr, obs, lli, snr,
     +                     eventrecs)
c=======================================================================
c
c     ... Write a record to a RINEX file, either RINEX version 1 or 2.
c
      implicit none
c
      integer         ou, nobs, itime(5), prn(24), numsat, flag, sec,
     +                msec, fmt_vers
      character*80    outline, outline2, eventrecs(64), dynfmt, dynfmt2
      character*1     char(24), lli(9,24), snr(9,24)
      real*8          obs(9,24), clockerr
      
      integer         i, i1, i2, itrack, j
c
c
      outline = ' '
      outline2 = ' '
c      write(outline(1:32), fmt='(5I3,X,I2,''.'',I3.3,4X,2I3)')
      write(outline(1:32), fmt='(5I3,X,I2,''.'',I7.7,2I3)')
     +         (itime(i), i=1,5), sec, msec, flag, numsat

c Nacho08 - if numsat is above 12 need to write two lines
      if (((numsat-1)/12).eq.1) then
        write(outline2(1:32), fmt='(32X)')
      endif
c
c     ... Write the satellite ID numbers if this is a normal
c         observation record, a record indicating a power
c         failure since the previous epoch, or a cycle slip
c         record.
c
c Nacho08 - if numsat is above 12 need to write two lines
      do j=0,((numsat-1)/12)
       if (flag.le.1 .or. flag.eq.6) then
         do itrack = 1, 12
            i1 = 33 + 3*(itrack-1)
            i2 = 32 + 3*itrack
            if ((itrack+(j*12)).le.numsat) then
               if (j.eq.0) write(outline(i1:i2), fmt='(A1,I2)') 
     +                    	char(itrack+(j*12)),prn(itrack+(j*12))
               if (j.eq.1) write(outline2(i1:i2), fmt='(A1,I2)') 
     +                          char(itrack+(j*12)),prn(itrack+(j*12))
            else
               if (j.eq.0) write(outline(i1:i2), fmt='(A3)') '   '
               if (j.eq.1) write(outline2(i1:i2), fmt='(A3)') '   '
            endif
         enddo
       endif
      enddo
c
c     fix fmt error: 12.7 should be 12.9 ... JimR 30Apr99
      if (fmt_vers.gt.1 .and. clockerr.ne.0.d0) then
         write(outline(69:80), fmt='(F12.9)') clockerr
c Nacho08 - if numsat is above 12 need to write two lines
         if (((numsat-1)/12).eq.1) 
     +            write(outline2(69:80), fmt='(F12.9)') clockerr
      endif
      call writeline(ou, outline, 80)
c Nacho08 - if numsat is above 12 need to write two lines
      if (((numsat-1)/12).eq.1) call writeline(ou, outline2, 80)
c
      if (flag .le. 1) then
c
c        ... Write a normal observation record
c
         do itrack = 1, numsat
            if (nobs .le. 5) then
               outline = ' '
               write(dynfmt, fmt='(A, I3.3, A)')
     +             "(", nobs, "(F14.3, 2A1))"
               write(outline, fmt=dynfmt)
     +            (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=1,nobs)
               call writeline(ou, outline, 80)
            else
               outline = ' '
               write(outline, fmt='( 5(F14.3, 2A1) )')
     +            (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=1,5)
               call writeline(ou, outline, 80)
c
               outline = ' '
               write(dynfmt2, fmt='(A, I3.3, A)')
     +             "(", nobs-5, "(F14.3, 2A1))"
               write(outline, fmt=dynfmt2)
     +            (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=6,nobs)
               call writeline(ou, outline, 80)

            endif
         enddo
      else
c
c        ... Write the (uninterpreted) event records
c
         do itrack = 1, numsat
            call writeline(ou, eventrecs(itrack), 80)
         enddo
      endif
c      
      return
      end
c      
