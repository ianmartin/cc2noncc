c=======================================================================
      SUBROUTINE UPCASE(ISTR,N)
c=======================================================================
C
C  This routine converts lower case characters to upper case.
C
      CHARACTER ISTR*(*)
      INTEGER   N, I, K
C
      DO I = 1, N
        K=ICHAR(ISTR(I:I))
        IF((K.LT.97) .OR. (K.GT.122)) THEN
          CONTINUE
        ELSE
          ISTR(I:I)=CHAR(K-32)
        END IF
      END DO
C
      RETURN
      END
c
c
