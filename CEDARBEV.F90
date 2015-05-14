!     ******************************************************************
!     CEDARBEV.FOR
!     Copyright(c) Water Resources 2000
!
!     Created: 12/6/2005 11:58:10 AM
!     Author : DAVID B COLE
!     Last change: CWM 1/11/2011 3:26:25 PM
!     ******************************************************************

      SUBROUTINE LNDCLC
!-----------------------------------------------------------------------
      use PARAMDIM
      use COMMO
      use PrintStuff
      REAL (KIND=8) :: ARDEM,DMDMD,PUMPS,DMPMP
      EXTERNAL ARDEM,DMDMD,DMPMP
!-----------------------------------------------------------------------
!     CODE FOR CEDAR MODEL
!-----------------------------------------------------------------------
      QX(5,J,K)=QX(1,J,K)+QX(2,J,K)+QX(3,J,K)+QX(4,J,K)
      CALL ARSUB(1)
      QX(12,J,K)=QX(11,J,K)+QX(10,J,K)
      CALL MUNSUB(1)
      QX(20,J,K)=QX(12,J,K)-QX(18,J,K)
      CALL WETLND(1)
      QX(16,J,K)=QX(20,J,K)+QX(14,J,K)-QX(15,J,K)
      QX(17,J,K)=-QX(16,J,K)
      QX(19,J,K)=QX(16,J,K)+QX(17,J,K)
      RETURN
      END