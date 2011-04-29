c=======================================================================
      subroutine copy_header(iu, ou, version, log_line, comment, cbias,
     +                   fmt_vers, nobs, obs_type, line_n, err)
c=======================================================================
c
      implicit none
      
      integer         iu, ou, nobs, fmt_vers
      character*60    version, log_line, comment, cbias(5)
      character*2     obs_type(18)
      logical         err
      integer         line_n
      
      character*80    outline, line, header_line, empty, dynfmt
      integer         ios, j, i
      logical         endheader, lastcomment, commentsdone
      
      data            empty /' '/
c
      err = .false.

C      nobs = 0
C      do j = 1, 18
C         obs_type(j) = '  '
C      enddo

c
c     ... READ DATA HEADER
c
      read(unit=iu, fmt='(A80)', iostat=ios) header_line
      line_n=line_n+1
      err = (err .or. ios.ne.0)
      write(unit=ou, fmt='(A80)') header_line
      read(header_line, fmt='(I6)') fmt_vers
c
c     ... Read and modify the header. Add a COMMENT record showing that
c         nonCC2CC has been run.
c
      endheader = .false.
      lastcomment = .false.
      commentsdone = .false.
      do while (.not.endheader)
         read(unit=iu, fmt='(A80)') line
         line_n=line_n+1
c
c        ... Write our COMMENT lines after the last COMMENT lines
c            Our COMMENTs are the nonCC2CC version and the execution time.
c
         if (line(61:67).ne.'COMMENT' .and. line(61:67).ne.'comment'
     +       .and. lastcomment .and. .not.commentsdone) then
                outline = version//'COMMENT'
                call writeline(ou, outline, 80)
                outline = log_line//'COMMENT'
                call writeline(ou, outline, 80)
                outline = comment//'COMMENT'
                call writeline(ou, outline, 80)
		do j = 1, 5
                  outline = cbias(j)//'COMMENT'
                  call writeline(ou, outline, 80)
		enddo
                lastcomment = .false.
                commentsdone = .true.
         endif
c
c        ... Check for a few special lines:
c              COMMENT                 flag it so we can add our own comments
c              MARKER NAME             If there have been no comments,
c                                      write our own comments before this line
c
c            Otherwise the header lines are copied as is.
c
         if (line(61:67).eq.'COMMENT'
     +       .or. line(61:67).eq.'comment') then
                call writeline(ou, line, 80)
                lastcomment = .true.
         else if ((line(61:71).eq.'MARKER NAME'
     +       .or. line(61:71).eq.'marker name')
     +       .and. .not.commentsdone) then
c
c               ... Assume that there have been no comment lines. Write
c                   out comment lines first, then the MARKER NAME line.
c
                outline = version//'COMMENT'
                call writeline(ou, outline, 80)
                outline = log_line//'COMMENT'
                call writeline(ou, outline, 80)
                outline = comment//'COMMENT'
                call writeline(ou, outline, 80)
		do j = 1, 5
                  outline = cbias(j)//'COMMENT'
                  call writeline(ou, outline, 80)
		enddo
                commentsdone = .true.
                call writeline(ou, line, 80)
         else if (line(61:79).eq.'# / TYPES OF OBSERV'
     +       .or.line(61:79).eq.'# / types of observ') then
c                read(line, fmt='(I6)') nobs
c                write(dynfmt, fmt='(A, I3.3, A)') 
c     +                      "(6X,", nobs, "(4X,A2))"
c                read(line, fmt=dynfmt)
c     +                      (obs_type(i), i=1,nobs)
                call writeline(ou, line, 80)
         else              ! Indentation here is OK
                call writeline(ou, line, 80)
c
c           ... Check for the end of header marker
c
            endheader=(fmt_vers.eq.1.and.(line.eq.' '.or.line.eq.' 
     +'))
            endheader = (endheader .or. (fmt_vers.eq.2
     +                .and. (line(61:73).eq.'END OF HEADER'
     +                .or. line(61:73).eq.'end of header')))
         endif
      enddo
c
      return
      end
c
