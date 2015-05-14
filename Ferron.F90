!     ******************************************************************
!     Ferron.F90
!     Copyright(c) Water Resources 2011
!
!     Created: Feb 15, 2011
!     Author : Craig W Miller
!     Last change: CWM 7/2/2012 1:34:10 PM
!     ******************************************************************

      SUBROUTINE LNDCLC
!-----------------------------------------------------------------------
use PARAMDIM
use COMMO
use PrintStuff
REAL (KIND=8) :: ARDEM,DMDMD,PUMPS
EXTERNAL ARDEM,DMDMD
! SevExport is taken from the top of the basin over to the Sevier River
! PExport is water taken from Ferron Creek to supply cooling water to the
!   Hunter power plant.
INTEGER (KIND=4) :: N,KK
REAL (KIND=8) :: SevExport(13),PExport(13)
Data (SevExport(N),N=1,13)/20.,0.,0.,0.,0.,0.,6.,43.,109.,48.,10.,3.,239./
Data (PExport(N),N=1,13)/8*758.,4*759.,9100./
INTEGER (KIND=4) :: IQXcon(2)
Data (IQXcon(N),N=1,2)/4,5/
!-----------------------------------------------------------------------
!     CODE FOR Ferron 08-02-03, Model 5
!-----------------------------------------------------------------------
!QX(2,J,K) is an export to the Sevier River from the top of the basin.
QX(2,J,K)=SevExport(K)
if (j.eq.14.AND.K.EQ.9) then
  KK=1
end if
!QX(3,J,K) is USGS gauge 093265000, 1911 to present
QX(1,J,K)=QX(3,J,K)+QX(2,J,K)
!Hunter power demand from the last model was a constant (see PExport above)
!Release reservoir demand to QX numbers 4 and 5 for Hunter Power
DMD=PExport(K)
CALL RESR(1,IQXcon,2)
QX(17,J,K)=QX(4,J,K)-QX(5,J,K)
CALL MUNSUB(1)
!When ag lands for land area 1 are broken out these aren't necessary
QX(19,J,K)=QX(17,J,K)-QX(18,J,K)
QX(9,J,K)=QX(19,J,K)+QX(20,J,K)
CALL ARSUB(1)
CALL ARSUB(2)
QX(15,J,K)=QX(14,J,K)+QX(13,J,K)
CALL WETLND(1)
QX(22,J,K)=QX(15,J,K)-QX(21,J,K)
QX(23,J,K)=QX(24,J,K)-QX(22,J,K)
RETURN
END

REAL (KIND=8) FUNCTION ErrorFunc()
ErrorFunc=0.0
END
