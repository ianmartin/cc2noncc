c=======================================================================
      subroutine writeline(ou, string, length)
c=======================================================================
c
c     ... Writes the string STRING to output unit, deleting
c         trailing blanks.

c
      implicit none
c
      integer        ou, length
      character*(*)  string
      character*128   dynfmt
c
      integer        ichar
c
c
      ichar = length
      do while (ichar.gt.1 .and. string(ichar:ichar).eq.' ')
         ichar = ichar - 1
      enddo
      write(dynfmt, fmt='(A, I3.3, A)') "(A", ichar, ")"
      write(unit=ou, fmt=dynfmt) string(1:ichar)
c
      return
      end
c
