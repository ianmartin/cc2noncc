c=======================================================================
      subroutine read_bias(bu, stderr, first_obs, bias, err)
c=======================================================================
c
c     ... Read an external file of historic P1-C1 bias values.

      implicit none
c
      character*80  line
      integer       bu, stderr, first_obs(3)
      integer       j, k, n, ierr
      integer       i_yyyy, i_mm, i_dd, n_read
      integer*4     ymd2mjd, i_epoch, mjd
      real*8        bias(40)
      logical       err
c
      i_epoch = ymd2mjd(first_obs(1),first_obs(2),first_obs(3))
      err = .true.
c
c     ... skip past header lines
200   read(unit=bu, fmt='(A80)', end=205) line
      if (line(1:14) .ne. '+cc2noncc/corr') then
	goto 200
      else
	goto 210
      endif
205   write(stderr,*) 'ERROR: no +cc2noncc/corr in bias file' 
      return
c
c     ... search for epoch line
210   read(unit=bu, fmt='(A80)') line
      if (line(1:14) .eq. '-cc2noncc/corr') then
        if (n_read .eq. 40) err = .false.
        return
      end if
c
      if (line(1:1) .ne. ' ') goto 210  
      if (line(1:2) .ne. ' h') then
        write(stderr,*) 'ERROR: cannot find bias file epochs'
        write(stderr,*) line 
        return
      endif
c
c     ... should now be positioned to read epoch of new biases
      read(line(3:), *, iostat=ierr) i_yyyy, i_mm, i_dd
      if (ierr .ne. 0) then
         write(stderr,*) 'ERROR: reading bias epoch'
         write(stderr,*) 'iostat = ', ierr
         return
      endif
      mjd = ymd2mjd(i_yyyy, i_mm, i_dd)
c
c     ... new set of biases more recent than data epoch, so done
      if (i_epoch .lt. mjd) then
        if (n_read .eq. 40) err = .false.
        return
      endif
c
c     ... read new set of bias values and save 
      j = 1
      do while (j .le. 8)
        read(unit=bu, fmt='(A80)') line
        if (line(1:5) .ne. '     ') goto 310  
        n = 5*(j-1) + 1
        read(line(1:),fmt='(17x,5(e9.3,x))',iostat=ierr)
     +    (bias(k),k=n,n+4) 
        if (ierr .ne. 0) then
           write(stderr,*) 'ERROR: reading bias values'
           write(stderr,*) 'iostat = ', ierr
           return
        endif
        n_read = j * 5
        j = j + 1
310     continue
      end do

c
c     ... go back and search for next set of bias values
      goto 210
c
      end
c      
