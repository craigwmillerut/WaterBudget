!     ******************************************************************
!     commo.f90
!     Copyright(c) Utah Divsion of Water Resources 2000
!
!     Created: 3/31/2010 4:07:29 PM
!     Author : STATE OF UTAH
!     Last change: CWM 4/1/2010 11:16:15 AM
!     ******************************************************************
MODULE COMMO
use PARAMDIM
      INTEGER*4 L,J,K
      REAL (KIND=8) :: QX(MQX,MYEAR,13),DMD,QDVM,EVRT(MRES,13), &
            QRMN(MRES,13),SMX(MRES),SMN(MRES),STO(MRES), &
            RAR(MRES),REL(MRES),ST(MRES,MYEAR,13), &
            EVAP(MRES,MYEAR,13)
      REAL (KIND=8) :: GWUSE(MLAND,13),RTUSE(MLAND,13),SBUSE(MLAND,13)
END MODULE

