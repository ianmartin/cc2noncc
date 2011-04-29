c=======================================================================
      subroutine read_rx_types(bu, stderr, c1p2list, nc1p2,
     +                    c1onlylist, nc1only, err)
c=======================================================================
c
c     ... Read an external file to get lists of receiver types;
c         returns with err=.true. if no receiver types found in
c         external file.

      implicit none
c
      character*80  line
      character*20  c1p2list(100), c1onlylist(100)
      integer       bu, stderr
      integer       nc1p2, nc1only
c      integer       j, k, n, ierr
      logical       err
c
      err = .true.
      nc1p2 = 0
      nc1only = 0
c
c     ... skip past header lines
200   read(unit=bu, fmt='(A80)', end=205) line
      if (line(1:14) .ne. '+cc2noncc/rcvr') then
	goto 200
      else
	goto 210
      endif
205   write(stderr,*) 'ERROR: no +cc2noncc/rcvr in bias file' 
      return
c
c     ... search for C1P2 section
210   read(unit=bu, fmt='(A80)') line
      if (line(1:14) .eq. '-cc2noncc/rcvr') then
	if (nc1p2 .eq. 0 .and. nc1only .eq. 0) then
          err = .true.
	else
          err = .false.
        endif
        return
      endif
c
      if (line(1:1) .ne. ' ') goto 210  
      if (line(1:19) .eq. ' cc2noncc-type:C1P2') then
220     read(unit=bu, fmt='(A80)') line
        if (line(1:14) .eq. '-cc2noncc/rcvr') then
          if (nc1p2 .eq. 0 .and. nc1only .eq. 0) then
            err = .true.
          else
            err = .false.
          endif
          return
        endif
        if (line(1:1) .ne. ' ') go to 220
        if (line(1:19) .ne. ' cc2noncc-type:C1  ') then
          nc1p2 = nc1p2 + 1
          call upcase (line, 80)
          c1p2list(nc1p2) = line(2:21)
	  go to 220
        else
c
c     ... read C1-only section
230       read(unit=bu, fmt='(A80)') line
          if (line(1:14) .eq. '-cc2noncc/rcvr') then
            if (nc1p2 .eq. 0 .and. nc1only .eq. 0) then
              err = .true.
            else
              err = .false.
            endif
            return
          endif
          if (line(1:1) .ne. ' ') go to 230
          nc1only = nc1only + 1
          call upcase (line, 80)
          c1onlylist(nc1only) = line(2:21)
	  go to 230
        endif
      endif
c
      end
