!     ******************************************************************
!     ErrorFunc.F90
!     Copyright(c) Utah Division of Water Resources 2000
!
!     Created: 3/29/2011 2:48:35 PM
!     Author : STATE OF UTAH
!     Last change: CWM 5/4/2011 4:29:05 PM
!     ******************************************************************

      REAL (KIND=8) FUNCTION ErrorFunc()
!-----------------------------------------------------------------------
!     Function ErrorVal
!     Created 05/03/2011 by Craig W. Miller
!
!     This function calculates the error for each time step
!     For example, if you wished to minimize the difference in the
!     QX(34,j,k) and QX(2,j,k) you might wish to say
!     ErrorFunc = abs(QX(34,J,K)-QX(2,J,K))
!
!     With that, Brents method will find the optimal value of the
!     parameter you specified to minimize the difference
!-----------------------------------------------------------------------
use PARAMDIM
use COMMO
use PrintStuff
REAL (KIND=8),EXTERNAL :: ARDEM,DMDMD
!-----------------------------------------------------------------------
!     SAMPLE CODE
!-----------------------------------------------------------------------
IF (QX(26,J,K).GT.0.0) THEN
  ErrorFunc=QX(26,J,K)-QX(21,J,K)
ELSE
  ErrorFunc=0.0
END IF
RETURN
END
