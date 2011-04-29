c=======================================================================
      subroutine read_rec(iu, nobs, itime, sec, msec, flag, numsat, prn,
     +               char, clockerr, obs, lli, snr, eventrecs, err, eof)
c=======================================================================
c
c     ... Read a record from a RINEX file. Reads RINEX version 1 and 2
c
      implicit none
c
      integer         iu, nobs, itime(5), prn(99), numsat, flag,
     +                sec, msec
      character*1     lli(18,99), snr(18,99)
      character*80    eventrecs(64)
      character*1     char(99)
      real*8          obs(18,99), clockerr
      logical         eof, err
c
      integer         ios, i, itrack
      character*80    inline, dynfmt, dynfmt2, dynfmt3
c
      inline = ' '
      read(unit=iu, fmt='(A80)', iostat=ios) inline
c
      read(inline(1:32),'(5I3,X,I2,X,I7,2I3)')
     +         (itime(i), i=1,5), sec, msec, flag, numsat
c
c     ... Read the satellite numbers if this is an observation
c         record, a record indicating restart after power failure,
c         or a cycle slip record.
c
      if (flag.le.1 .or. flag.eq.6)
     +   read(inline(33:80),'(12(A1,I2),F12.9)')
     +         (char(i), prn(i),i=1,12), clockerr
c
c Nacho08 - this reads more than one line of sat IDs 
        if (((numsat-1)/12).eq.1) then    
         read(unit=iu, fmt='(A80)', iostat=ios) inline
         read(inline(33:80),'(12(A1,I2),F12.9)')
     +         (char(i), prn(i),i=13,24), clockerr
        endif
      if (flag .le. 1) then
         do itrack = 1, numsat
            if (nobs .le. 5) then
c	Read only one line of measurements per satellite	    
               write(dynfmt, fmt='(A, I3.3, A)')
     +           "(", nobs, "(F14.3, 2A1))"
               read(unit=iu, fmt=dynfmt, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=1,nobs)

            elseif (nobs .le. 10) then
c	Read two lines of measurements per satellite	    
               write(dynfmt2, fmt='(A, I3.3, A)') 
     +            "(5(F14.3, 2A1),/,", nobs-5, "(F14.3, 2A1))"
               read(unit=iu, fmt=dynfmt2, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=1,nobs)

            elseif (nobs .le. 15) then
c	Read three lines of measurements per satellite	    
               write(dynfmt3, fmt='(A, I3.3, A)') 
     +    "(5(F14.3, 2A1),/,5(F14.3, 2A1),/,", nobs-5, "(F14.3, 2A1))"
               read(unit=iu, fmt=dynfmt3, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),snr(i,itrack), i=1,nobs)

	    else
	     write(6,*) 
     +  	     'Read more than 15 obs per sat NOT possible'	     
             call exit(2)
            endif
         enddo
      else
         do itrack = 1, numsat
            read(unit=iu, fmt='(A80)', iostat=ios) eventrecs(itrack)
         enddo
      endif
c
      eof = (ios .eq. -1)
      err = (ios.ne.0 .and. .not.eof)
c
      return
      end
c      
