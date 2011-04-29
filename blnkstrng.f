c=======================================================================
      SUBROUTINE BLNKSTRNG(ISTR,N)
c=======================================================================
C
C  This routine blanks out a characer string.
C
      CHARACTER ISTR*(*)
      INTEGER   N, I
C
      DO I = 1, N
        ISTR(I:I)=" "
      END DO
C
      RETURN
      END
c
c
c
