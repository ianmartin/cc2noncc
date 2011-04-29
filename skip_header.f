c=======================================================================
      subroutine skip_header(iu, fmt_vers, rec_type, fixed, nobs,
     +                       obs_type, marker, line_n, first_obs, err)
c=======================================================================

      implicit none
      
      integer         iu, nobs, fmt_vers, line_n, first_obs(3)
      character*20    rec_type
      character*4     marker
      character*2     obs_type(18)
      logical         fixed, err
      
      character*80    line, header_line, empty, dynfmt
      integer         ios, j, i
      logical         endheader
      
      data            empty /' '/
c
      fixed = .false.
      err = .false.

      nobs = 0
      do j = 1, 18
         obs_type(j) = '  '
      enddo
c
c     ... READ DATA HEADER
c
      read(unit=iu, fmt='(A80)', iostat=ios) header_line
      line_n = line_n + 1
      err = (err .or. ios.ne.0)
      read(header_line, fmt='(I6)') fmt_vers
c
c     ... Read past the header.
c
      endheader = .false.
      do while (.not.endheader)
         read(unit=iu, fmt='(A80)') line
         line_n = line_n + 1
c	 
         if ( (nobs.eq.0).and.(line(61:79).eq.'# / TYPES OF OBSERV'
     +       .or. line(61:79).eq.'# / types of observ') ) then
            read(line, fmt='(I6)') nobs
cNacho08 - don't try to correct files with too many observations	    
	    if (nobs.gt.18) 
     +         stop 'cc2noncc; cannot correct files with more 
     +                                       than 18 obs types!'
            if (nobs.gt.9) then
             write(dynfmt, fmt='(A, I1, A)')
     +                      "(6X,", 9, "(4X,A2))"
             read(line, fmt=dynfmt)
     +                      (obs_type(i), i=1,9)
	     read(unit=iu, fmt='(A80)') line
             read(line, fmt=dynfmt)
     +                      (obs_type(i), i=10,18)
     
            else
	    
             write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs, "(4X,A2))"
             read(line, fmt=dynfmt)
     +                      (obs_type(i), i=1,nobs)
	    endif
         endif
c	 
         if (line(61:79).eq.'REC # / TYPE / VERS'
     +       .or. line(61:79).eq.'rec # / type / vers') then
            read(line, fmt='(20X,A20)')
     +                      rec_type
         endif
         if (line(61:71).eq.'MARKER NAME'
     +       .or. line(61:71).eq.'marker name') then
            read(line, fmt='(A4)')
     +                      marker
         endif
         if (line(61:77).eq.'TIME OF FIRST OBS'
     +       .or. line(61:77).eq.'time of first obs') then
            read(line, fmt='(3I6)')
     +                      first_obs
         endif
         if (line(61:79).eq.'COMMENT            '
     +       .or. line(61:79).eq.'comment            ') then
            if (line(1:17) .eq. 'CC2nonCC executed') fixed = .true.
         endif
c
c        ... Check for the end of header marker
c
         endheader=(fmt_vers.eq.1.and.(line .eq. ' '.or. line.eq.' 
     +'))
         endheader = (endheader .or. (fmt_vers.eq.2
     +                .and. (line(61:73).eq.'END OF HEADER'
     +                .or. line(61:73).eq.'end of header')))
      enddo
c
      return
      end
c
c
