MODULE FSQL_DATA
!*************************************************************************
use f90SQLConstants
!use f90SQLStructures
use f90SQL
!
! Number of Rows affected
INTEGER(KIND=4),PARAMETER :: Nf90sql=500

!
! Index Field
! Index Field  Note type of this field must match Database Index (column) name.
INTEGER(KIND=4):: IDI(Nf90sql),f90sqlIndex
! Set NumericIndex .true. if index field is numeric not string (character)
LOGICAL:: NumericIndex=.false.
!   Database Field (Column) Names
CHARACTER(LEN=10):: IDFIELD='ResID'
!
CHARACTER(LEN=256):: Table='StageAreaCap',fname='ReservoirData.mdb',&
                     DefaultDir='D:\FORTRAN\F90SQL\Access'
!
! Double Precision Fields
!
! Set NDPFIELD to number of Double Precision Fields
INTEGER(KIND=4):: NDPFIELD=3
!------------------------------------------------------------------------
! FSQL_DATA Data dictionary and explanation
! This module defines variables used in the ACCESSREAD subroutine
!
! NDPFIELD - The number of fields on record to be read
! MDPFIELD - the maximum number of double precision variables on a record
! Nf90sql - The maximum number of rows which can be read
! XI - An array to hold double precision variables read in
! Field - The double precision field names
! NDATEFIELD - The number of date fields on a record
! DATEFIELDS - The names of the date fields on a record
! MSTRINGFIELD - Maximum number of string fields on a record
! NSTRINGFIELD - The number of string fields on this record
! STRINGFIELDS - The string fields on a record
! MLOGICFIELD - The maximum number of logicfields on a record
! LOGICFIELDS - These are the logical or integer variable names on the record
!   Logic records include integer and logical fields
INTEGER(KIND=4),PARAMETER:: MLOGICFIELD=20
INTEGER(KIND=4),PARAMETER:: MDPFIELD=60
INTEGER(KIND=4),PARAMETER:: MDATEFIELD=20
INTEGER(KIND=4),PARAMETER:: MSTRINGFIELD=20
INTEGER(KIND=4),PARAMETER:: FIELDLEN=100
LOGICAL (KIND=4) :: WillOpen = .TRUE.
LOGICAL (KIND=4) :: WillClose = .TRUE.
LOGICAL (KIND=4) :: CloseBefore = .TRUE.
REAL(KIND=8):: XI(Nf90sql,MDPFIELD)
!   Database Double Precision Field Column Names
CHARACTER(LEN=12):: Field(MDPFIELD)
!
!  Date Fields
!
!  Set NDATEFIELD to number of Date fields
INTEGER(KIND=4):: NDATEFIELD
!   Database Date Field Column Names
CHARACTER(LEN=12):: DATEFIELDS(MDATEFIELD)
Type DATE_STRUCT
        SEQUENCE
        integer(SQLSMALLINT_KIND):: Year
        integer(SQLUSMALLINT_KIND)::Month
        integer(SQLUSMALLINT_KIND)::Day
End Type DATE_STRUCT
Type TIMESTAMP_STRUCT
        SEQUENCE
        integer(SQLSMALLINT_KIND):: Year
        integer(SQLUSMALLINT_KIND)::Month
        integer(SQLUSMALLINT_KIND)::Day
        integer(SQLUSMALLINT_KIND)::Hour
        integer(SQLUSMALLINT_KIND)::Minute
        integer(SQLUSMALLINT_KIND)::Second
        integer(SQLUINTEGER_KIND):: Fraction
    End Type TIMESTAMP_STRUCT
 !DATER is array to hold dates
 !DATES is local array used for each row
 !Choose Date Stucture (TIMESTAMP_STRUCT) for Access and Excel (DATE_STRUCT) for DBF
TYPE (TIMESTAMP_STRUCT):: DATER(NF90sql,MDATEFIELD),DATES(NF90sql)
!
! String (Character) Fields
!
! Set NSTRINGFIELD to number or String (Character) Fields
INTEGER(KIND=4):: NSTRINGFIELD=2
!   Database Field (Column) Names
CHARACTER(LEN=FIELDLEN):: STRINGFIELDS(MSTRINGFIELD)
CHARACTER(LEN=255):: F90SQLSTRINGS(Nf90sql,MSTRINGFIELD)
character(len=255)::ConnStr
!
!  Logic or Integer Fields
!  Logic or Integer Fields are treated the same and the same arrray location
!  is used for both.
!  F90SQLLOGIC and F90SQLINTEGER may be are used to assign values to the array
!  F90SQLLOGIC for logical or boolean expressions
!  F90SQLINTEGER for integer expressions
!
!   Set NLOGICFIELD to number or total number of logic and integer Fields
   INTEGER(KIND=4):: NLOGICFIELD
!   Database Field (Column) Names
   logical (kind=1) OpenCloseConn
   CHARACTER(LEN=FIELDLEN):: LOGICFIELDS(MLOGICFIELD)
   LOGICAL(KIND=4):: F90SQLLOGIC(NF90SQL,MLOGICFIELD)
   INTEGER(KIND=4):: F90SQLINTEGER(NF90SQL,MLOGICFIELD)
   EQUIVALENCE (F90SQLLOGIC,F90SQLINTEGER)
!
!   Error Messages and Codes Returned
!
    CHARACTER(LEN=255):: ERROR10,ERROR_10,ERRORNATIVE(10),MESSAGE0
    integer(SQLSMALLINT_KIND):: iDiag
    INTEGER:: IRETURN
!
   CHARACTER(LEN=FIELDLEN):: RESID
   CHARACTER(LEN=256):: IndexString
   INTEGER(KIND=4):: KOUNTFSQL
!
END MODULE

!     ******************************************************************
!     AccessUpdate.f90
!     Copyright(c) State of Utah 2000
!
!     Created: 6/10/2009 4:58:41 PM
!     Author : DAVID B COLE
!     Last change: SOU 12/3/2012 12:08:18 PM
!     ******************************************************************
!*************************************************************************
   subroutine AccessRead()
!*************************************************************************
!load f90SQL modules
use f90SQLConstants
use f90SQL
USE FSQL_DATA
implicit none

! Subroutine Arguments
! Data Arguments
integer(kind=4):: kount
!
integer(kind=4):: ii,KK,NFIELDS
character(len=3):: CINTEGER
!
integer(SQLINTEGER_KIND),parameter:: MaxStringLen=255
integer(SQLHENV_KIND):: EnvHndl 
integer(SQLHDBC_KIND):: ConnHndl 
integer(SQLHSTMT_KIND):: StmtHndl 
integer(SQLRETURN_KIND)::iRet 
integer(SQLSMALLINT_KIND)::FIELDNumber,i,ConnStrLength
double precision XA(100)
character(len=MaxStringLen) SQLStmtStr, FMT
character(len=255) STRINGS(Nf90sql)
LOGICAL(KIND=4):: LLOGIC(Nf90sql)
INTEGER(KIND=4):: LINTEGER(Nf90sql),SELLEN
EQUIVALENCE (LLOGIC), (LINTEGER)
INTEGER(KIND=4):: STRINGS_LEN,MODE
character (len=255) :: ALLTRIM
character(len=MaxStringLen):: ConnStrOut,tempString

iRet=SQL_SUCCESS
if (WillOpen) then
    !allocate an environment handle
    call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)

    !Set ODBC version to use (3.x in this case)
    call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, iRet)

    !Allocate a connection handle
    call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl, iRet)

    !Note that we use a file-connection instead of a DSN connection
    !open a connection to the excel workbook
    call f90SQLDriverConnect(ConnHndl, f90SQL_NULL_PTR, ConnStr, &
        ConnStrOut, ConnStrLength, SQL_DRIVER_COMPLETE,iRet)
end if
!Set connection attributes to read
!(usually this is set before the connection is established, but there seems to be a bug
!in the excel ODBC driver that does not recognize this setting if done before connecting)
!   call f90SQLSetConnectAttr (ConnHndl, SQL_ATTR_ACCESS_MODE, &
!                              SQL_MODE_READ_ONLY, iRet)

! Open the table if requested

if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then 


    if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then 
  
         !Allocate a statement handle
         call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet)

         KOUNTFSQL=0
         SQLStmtStr='SELECT '
         NFIELDS=NDPFIELD+NDATEFIELD+NSTRINGFIELD+nlogicfield
         if (NFIELDS==0) then
            Error_10='No Fields Chosen'
            Return
         end if
         DO i=1,NDPFIELD
            if (len(trim(SQLStmtStr))>7) SQLStmtStr=Trim(SQLStmtStr) // ","
            SQLStmtStr=trim(SQLStmtStr) // " [" // Trim(Field(i)) // "]"
         END DO
         DO i=1,NSTRINGFIELD
            if (len(trim(SQLStmtStr))>7) SQLStmtStr=Trim(SQLStmtStr) // ","
            SQLStmtStr=trim(SQLStmtStr) // " [" // Trim(StringFields(i)) // "]"
         END DO
         DO i=1,NDATEFIELD
            if (len(trim(SQLStmtStr))>7) SQLStmtStr=Trim(SQLStmtStr) // ","
            SQLStmtStr=trim(SQLStmtStr) // " [" // Trim(DATEFIELDS(i)) // "]"
         END DO
         DO i=1,NLOGICFIELD
            if (len(trim(SQLStmtStr))>7) SQLStmtStr=Trim(SQLStmtStr) // ","
            SQLStmtStr=trim(SQLStmtStr) // " [" // Trim(logicfields(i)) // "]"
         END DO
         SQLStmtStr = trim(SQLStmtStr) // " FROM " // trim(Table)
         if (nblank(indexstring)>0) then
            SQLStmtStr = trim(SQLStmtStr) // " " // trim(indexstring)
         end if
         SQLStmtStr = trim(SQLStmtStr) // ";"
!--------------------------------------------------------------------------
! Tried the following and changed it 8/18/2010
!--------------------------------------------------------------------------
         !Prepare the SQL query
         !call f90SQLPrepare(StmtHndl, SQLStmtStr, iRet)

         !Prepare the SQL query and execute the query
         !call f90SQLExecute(StmtHndl,iRet)
!--------------------------------------------------------------------------
! Tried the following 8/18/2010
!--------------------------------------------------------------------------
         !Instruct the driver to execute the statement
         call f90SQLExecDirect(StmtHndl,SQLStmtStr,iRet)
!--------------------------------------------------------------------------
! End of changes
!--------------------------------------------------------------------------
         !bind SQL statement parameters to fortran variables
         FIELDNumber=0
         DO KK=1,NDPFIELD
            FIELDNumber=FIELDNumber+1
            call f90SQLBindCol (StmtHndl, FIELDNumber, SQL_F_DOUBLE, XA(KK), f90SQL_NULL_PTR, iRet)
         END DO
         DO KK=1,NSTRINGFIELD
           FIELDNumber=FIELDNumber+1
           STRINGS_LEN=LEN_TRIM(STRINGS(KK))
           call f90SQLBindCol (StmtHndl, FIELDNumber, SQL_CHAR, STRINGS(KK), f90SQL_NULL_PTR, iRet)
         END DO
         DO KK=1,NDATEFIELD
          FIELDNumber=FIELDNumber+1
          call f90SQLBindCol (StmtHndl, FIELDNumber, SQL_F_TYPE_TIMESTAMP, DATES(KK), f90SQL_NULL_PTR, iRet)
         END DO
         DO KK=1,NlogicFIELD
           FIELDNumber=FIELDNumber+1
           call f90SQLBindCol (StmtHndl, FIELDNumber, SQL_F_LONG, LINTEGER(KK), f90SQL_NULL_PTR, iRet)
         END DO

         !loop Load Values and insert them into Table


         if (iRet.eq.SQL_SUCCESS .or. iRet.eq.SQL_SUCCESS_WITH_INFO) then

            !Retrieve data
             KOUNTFSQL=0
             do while (.true.)
                call f90SQLFetch(StmtHndl,iRet)
                if (iRet.ne.SQL_SUCCESS .and. iRet.ne.SQL_SUCCESS_WITH_INFO) then
                    if (iRet.eq.SQL_NO_DATA) then
                        print *,'End of data set reached for '//trim(Table)
                    else
                        print *,'Error fetching data '//trim(Table)
                        call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
                        IRETURN=10
                    endif
                    exit
                endif
                KOUNTFSQL=KOUNTFSQL+1
                ii=KOUNTFSQL
                 do kk=1,NDPFIELD
                     XI(ii,kk)=XA(kk)
                 end do
                 DO KK=1,NSTRINGFIELD
                    F90SQLSTRINGS(ii,KK)=STRINGS(KK)
                 END DO
                 DO KK=1,NDATEFIELD
                    DATER(ii,KK)=DATES(KK)
                 END DO
                 DO KK=1,NLOGICFIELD
                    F90SQLINTEGER(ii,KK)=LINTEGER(KK)
                 END DO
            enddo
        else
            ERROR_10='Error Reading Records'
            call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
            IRETURN=-10
        endif
        !release table handle
        call f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndl,iRet)
   ENDIF
else
    call ShowDiags(SQL_HANDLE_DBC,ConnHndl)
    ERROR10='Error opening connection to datatable'
    IRETURN=10
endif
IF (WillClose) THEN
    !disconnect
    call f90SQLDisconnect(ConnHndl,iRet)
    !release connection handle
    call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
    !release environment handle
    call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
END IF
return
end


subroutine ShowDiags(HndlType,Hndl)

use f90SQLConstants
use f90SQL
USE FSQL_DATA
implicit none
integer(SQLHANDLE_KIND)::Hndl
integer(SQLSMALLINT_KIND)::HndlType
character(len=6):: SqlState
character(len= SQL_MAX_MESSAGE_LENGTH)::Msg 
integer(SQLINTEGER_KIND)::NativeError 
integer(SQLSMALLINT_KIND):: MsgLen
integer(SQLRETURN_KIND):: DiagRet
CHARACTER(LEN=255):: STRING

iDiag = 1 
do while (.true.)
   call f90SQLGetDiagRec(HndlType, Hndl, iDiag, SqlState, NativeError, Msg, MsgLen, DiagRet) 
   if (DiagRet.ne.SQL_SUCCESS.and.DiagRet.ne.SQL_SUCCESS_WITH_INFO) exit 
   string=' '
   WRITE(string,'(i6,2x,a,2x,a)')NativeError,Msg(1:MsgLen),trim(SqlState)
   ERRORNATIVE(iDiag)=string
   iDiag=iDiag+1
enddo
RETURN 
end subroutine ShowDiags
!*************************************************************************
   subroutine AccessUpdate (kount)
!*************************************************************************

!in an Excel workbook using f90SQL
!The program also demonstrates use of a direct file-connection (as opposed to a
!DSN connection) to access a datasource
!Note: User must provide an Excel workbook for this program to work
!      see additional comments below
!Copyright 1998, Canaima Software


!load f90SQL modules
use f90SQLConstants
use f90SQL
USE FSQL_DATA
implicit none

! Subroutine Arguments
! Data Arguments
integer(kind=4):: kount
!
integer(kind=4):: ii,KK,NFIELDS
character(len=3):: CINTEGER
!
integer(SQLINTEGER_KIND),parameter:: MaxStringLen=255 
integer(SQLHENV_KIND):: EnvHndl 
integer(SQLHDBC_KIND):: ConnHndl 
integer(SQLHSTMT_KIND):: StmtHndl 
integer(SQLRETURN_KIND)::iRet 
integer(SQLSMALLINT_KIND)::FIELDNumber,i,ConnStrLength
double precision XA(100)
character(len=MaxStringLen) SQLStmtStr, FMT
character(len=MaxStringLen) ConnStrOut
character(len=255) STRINGS(Nf90sql)
LOGICAL(KIND=4):: LLOGIC(Nf90sql)
INTEGER(KIND=4):: LINTEGER(Nf90sql)
EQUIVALENCE (LLOGIC), (LINTEGER)
INTEGER(KIND=4):: STRINGS_LEN,MODE

!Create a connection string
!Note that we use a file-connection instead of a DSN connection
!ConnStr='DSN=MS Access Database;DBQ='//trim(fname)//&
!------------------------------------------------------------------------------
!Connection strings are now handled in the main program
!------------------------------------------------------------------------------
! ';DefaultDir='//trim(DefaultDir)//';DriverId=25;FIL=MS Access;MaxBufferSize=2048;PageTimeout=5;UID=admin;'
!open a connection to the excel workbook
if (WillOpen) then
    !allocate an environment handle
    call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)

    !Set ODBC version to use (3.x in this case)
    call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, iRet)

    !Allocate a connection handle
    call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl, iRet)

    !Note that we use a file-connection instead of a DSN connection
    !open a connection to the excel workbook
    call f90SQLDriverConnect(ConnHndl, f90SQL_NULL_PTR, ConnStr, &
        ConnStrOut, ConnStrLength, SQL_DRIVER_COMPLETE,iRet)
end if

if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then

    !Set connection attributes to allow read/write
    !(usually this is set before the connection is established, but there seems to be a bug
    !in the excel ODBC driver that does not recognize this setting if done before connecting)
    call f90SQLSetConnectAttr (ConnHndl, SQL_ATTR_ACCESS_MODE, &
                               SQL_MODE_READ_WRITE, iRet) 

    if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then 
  
        !Allocate a statement handle 
        call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet) 

        !Create a parameterized UPDATE SQL query
        !one parameter for X and one for Y
!        SQLStmtStr='UPDATE [Table1$] SET X=?, Y=? WHERE ID=?'
         SQLStmtStr='UPDATE ['//trim(Table)//'] '
         NFIELDS=NDPFIELD+NDATEFIELD+NSTRINGFIELD+nlogicfield
         SELECT CASE (NFIELDS-1)
             CASE (0)
            FMT='(''UPDATE ['', A, ''] SET ['', A, '']=? '', '' WHERE ['',A,'']=?'')'
             CASE (1)
            FMT='(''UPDATE ['', A, ''] SET ['', A, '']=?,['', A, '']=? WHERE ['',A,'']=?'')'
             CASE DEFAULT
            FMT='(''UPDATE ['', A, ''] SET '',  2(''['',A, '']=?,''),''['', A, '']=? WHERE ['',A,'']=?'')'
            WRITE(CINTEGER,'(I3)') NFIELDS-1
            FMT(26:28)=CINTEGER
         END SELECT

!write(*,*) fmt

      IF(NLOGICFIELD.EQ.0) THEN
         IF(NDPFIELD.GT.0) THEN
            IF(NDATEFIELD.GT.0) THEN
               IF(NSTRINGFIELD.GT.0) THEN
                  MODE=1 ! all three options
               ELSE
                  MODE=2 ! Only Double and Date
               ENDIF
            ELSE
               IF(NSTRINGFIELD.GT.0) THEN
                  MODE=3 ! Only Double and String
               ELSE
                  MODE=4 ! Only Double
               ENDIF
            ENDIF
         ELSE
            IF(NDATEFIELD.GT.0) THEN
               IF(NSTRINGFIELD.GT.0) THEN
                  MODE=5 ! Only Date and String
               ELSE
                  MODE=6 ! Only Date
               ENDIF
            ELSE
               IF(NSTRINGFIELD.GT.0) THEN
                  MODE=7 ! Only String
               ELSE
                  MODE=0  ! Null No Opt
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IF(NDPFIELD.GT.0) THEN
            IF(NDATEFIELD.GT.0) THEN
               IF(NSTRINGFIELD.GT.0) THEN
                  MODE=8 ! all Four options
               ELSE
                  MODE=9 ! Only Double and Date and Logic
               ENDIF
            ELSE
               IF(NSTRINGFIELD.GT.0) THEN
                  MODE=10 ! Only Double and String and Logic
               ELSE
                  MODE=11 ! Only Double and Logic
               ENDIF
            ENDIF
         ELSE
            IF(NDATEFIELD.GT.0) THEN
               IF(NSTRINGFIELD.GT.0) THEN
                  MODE=12 ! Only Date and String and Logic
               ELSE
                  MODE=13 ! Only Date and Logic
               ENDIF
            ELSE
               IF(NSTRINGFIELD.GT.0) THEN
                  MODE=14 ! Only String and Logic
               ELSE
                  MODE=15  ! Only Logic
               ENDIF
            ENDIF
         ENDIF
      END IF
    SELECT CASE (MODE)
    CASE (1) ! all three options
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(Field(ii)),ii=1,NDPFIELD), &
              (TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD),(TRIM(DATEFIELDS(ii)),ii=1,NDATEFIELD),TRIM(IDFIELD)
    CASE (2) ! Only Double and Date
        write(SQLStmtStr,FMT)  trim(Table), (TRIM(Field(ii)),ii=1,NDPFIELD),&
              (TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD),(TRIM(DATEFIELDS(ii)),ii=1,NDATEFIELD),TRIM(IDFIELD)
    CASE (3) ! Only Double and String
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(Field(ii)),ii=1,NDPFIELD), &
              (TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD),  TRIM(IDFIELD)
    CASE (4) ! Only Double
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(Field(ii)),ii=1,NDPFIELD), &
              TRIM(IDFIELD)
    CASE (5) ! Only Date and String
        write(SQLStmtStr,FMT)  trim(Table), &
              (TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD),(TRIM(DATEFIELDS(ii)),ii=1,NDATEFIELD),TRIM(IDFIELD)
    CASE (6) ! Only Date
        write(SQLStmtStr,FMT)  trim(Table), &
              (TRIM(DATEFIELDS(ii)),ii=1,NDATEFIELD), TRIM(IDFIELD)
    CASE (7) ! Only String
        write(SQLStmtStr,FMT)  trim(Table), &
              (TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD), TRIM(IDFIELD)
    CASE (8) ! All four
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(Field(ii)),ii=1,NDPFIELD), &
              (TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD),(TRIM(DATEFIELDS(ii)),ii=1,NDATEFIELD),&
              (trim(logicfields(ii)),ii=1,nlogicfield),TRIM(IDFIELD)
    CASE (9) ! MODE=9 ! Only Double and Date and Logic
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(Field(ii)),ii=1,NDPFIELD), &
              (TRIM(DATEFIELDS(ii)),ii=1,NDATEFIELD),&
              (trim(logicfields(ii)),ii=1,nlogicfield),TRIM(IDFIELD)
   CASE (10)  ! MODE=10 ! Only Double and String and Logic
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(Field(ii)),ii=1,NDPFIELD), &
              (TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD),&
              (trim(logicfields(ii)),ii=1,nlogicfield),TRIM(IDFIELD)

   CASE (11)  ! MODE=11 ! Only Double and Logic
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(Field(ii)),ii=1,NDPFIELD), &
              (trim(logicfields(ii)),ii=1,nlogicfield),TRIM(IDFIELD)

   CASE (12)  ! MODE=12 ! Only Date and String and Logic
        write(SQLStmtStr,FMT)  trim(Table),&
              (TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD),(TRIM(DATEFIELDS(ii)),ii=1,NDATEFIELD),&
              (trim(logicfields(ii)),ii=1,nlogicfield),TRIM(IDFIELD)
      
   CASE (13)  ! MODE=13 ! Only Date and Logic
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(DATEFIELDS(ii)),ii=1,NDATEFIELD),&
              (trim(logicfields(ii)),ii=1,nlogicfield),TRIM(IDFIELD)
   
   CASE (14)  ! MODE=14 ! Only String and Logic
        write(SQLStmtStr,FMT)  trim(Table),(TRIM(STRINGFIELDS(ii)),ii=1,NSTRINGFIELD),&
              (trim(logicfields(ii)),ii=1,nlogicfield),TRIM(IDFIELD)
   
   CASE (15)  ! MODE=15  ! Only Logic
        write(SQLStmtStr,FMT)  trim(Table),(trim(logicfields(ii)),ii=1,nlogicfield),TRIM(IDFIELD)

    CASE DEFAULT ! Null No Opt
   !     Error='No Fields Chosen'
        Return
    END SELECT

  !temp print
  !      write(*,*)trim(SQLStmtStr)
  !     read(*,*)

        !Prepare the SQL query
        call f90SQLPrepare(StmtHndl, SQLStmtStr, iRet)

        !bind SQL statement parameters to fortran variables
        FIELDNumber=0
    IF(NDPFIELD.GT.0) THEN
      DO KK=1,NDPFIELD
            FIELDNumber=FIELDNumber+1
        call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_DOUBLE, &
                                 SQL_DOUBLE, int(15,SQLUINTEGER_KIND), int(5,SQLSMALLINT_KIND), &
                                 XA(KK), f90SQL_NULL_PTR, iRet)

      END DO
   END IF
   IF (NSTRINGFIELD.GT.0) THEN

        DO KK=1,NSTRINGFIELD
            FIELDNumber=FIELDNumber+1
            STRINGS_LEN=LEN_TRIM(STRINGS(KK))
            call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_CHAR, &
                                     SQL_VARCHAR, int(255,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
                                     STRINGS(KK), f90SQL_NULL_PTR, iRet)
        END DO
   END IF
   IF(NDATEFIELD.GT.0) THEN
        DO KK=1,NDATEFIELD
            FIELDNumber=FIELDNumber+1
           call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_TYPE_TIMESTAMP, &
                                    SQL_TYPE_TIMESTAMP, int(15,SQLUINTEGER_KIND), int(10,SQLSMALLINT_KIND), &
                                    DATES(KK), f90SQL_NULL_PTR, iRet)
        END DO
    END IF
    IF(NlogicFIELD.GT.0) THEN
      DO KK=1,NlogicFIELD
            FIELDNumber=FIELDNumber+1
        call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_LONG, &
                                 SQL_INTEGER, int(0,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
                                 LINTEGER(KK), f90SQL_NULL_PTR, iRet)
      END DO
   END IF
    FIELDNumber=FIELDNumber+1
    IF(NumericIndex) THEN
      call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_SSHORT, &
                             SQL_DOUBLE, int(15,SQLUINTEGER_KIND), int(5,SQLSMALLINT_KIND), &
                             f90sqlIndex, f90SQL_NULL_PTR, iRet)
    ELSE
      call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_CHAR, &
                          SQL_VARCHAR, int(255,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
                          f90sqlIndex, f90SQL_NULL_PTR, iRet)
    END IF

     !loop Load Values and insert them into Table

     do ii=1,kount

         do kk=1,NDPFIELD
             XA(kk)=XI(ii,kk)
         end do
         DO KK=1,NSTRINGFIELD
            STRINGS(KK)=F90SQLSTRINGS(ii,KK)
         END DO
         DO KK=1,NDATEFIELD
            DATES(KK)=DATER(ii,KK)
         END DO
         DO KK=1,NLOGICFIELD
            LINTEGER(KK)=F90SQLINTEGER(ii,KK)
         END DO
         f90sqlIndex=IDI(ii)
         call f90SQLExecute(StmtHndl,iRet)
         !Check for error when adding rows
         if (iRet.ne.SQL_SUCCESS .and. iRet.ne.SQL_SUCCESS_WITH_INFO) then
             ERROR_10='Error adding new records'
             call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
             IRETURN=-10
             exit
         endif
     enddo
        !release statement handle
        call f90SQLFreeHandle(SQL_HANDLE_STMT,StmtHndl,iRet)

    endif

    !Free connection
    call f90SQLDisconnect(ConnHndl,iRet)

else
    call ShowDiags(SQL_HANDLE_DBC,ConnHndl)
    ERROR10='Error opening connection to workbook'
    IRETURN=10
endif
!release connection handle
call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
!release environment handle
call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
return
end
!*************************************************************************
   subroutine AccessWrite (kount)
!*************************************************************************

!in an Excel workbook using f90SQL
!The program also demonstrates use of a direct file-connection (as opposed to a
!DSN connection) to access a datasource
!Note: User must provide an Excel workbook for this program to work
!      see additional comments below
!Copyright 1998, Canaima Software


!load f90SQL modules
use f90SQLConstants
use f90SQL
USE FSQL_DATA
implicit none

! Subroutine Arguments
! Data Arguments
integer(kind=4):: kount
!
integer(kind=4):: ii,KK,NFIELDS
character(len=3):: CINTEGER
!
integer(SQLINTEGER_KIND),parameter:: MaxStringLen=255 
integer(SQLHENV_KIND):: EnvHndl 
integer(SQLHDBC_KIND):: ConnHndl 
integer(SQLHSTMT_KIND):: StmtHndl 
integer(SQLRETURN_KIND)::iRet 
integer(SQLSMALLINT_KIND)::FIELDNumber,i,ConnStrLength,LenInit
double precision XA(100)
character(len=MaxStringLen) :: SQLStmtStr, FMT
character(len=255) STRINGS(Nf90sql),Question
LOGICAL(KIND=4):: LLOGIC(Nf90sql)
INTEGER(KIND=4):: LINTEGER(Nf90sql)
EQUIVALENCE (LLOGIC), (LINTEGER)
INTEGER(KIND=4):: STRINGS_LEN,MODE

!allocate an environment handle
!call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)
  
!Set ODBC version to use (3.x in this case) 
!call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, iRet)
  
!Allocate a connection handle 
!call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl, iRet)

!Create a connection string
!Note that we use a file-connection instead of a DSN connection
!------------------------------------------------------------------------------
!Changed 9/9/2012 to accomodate opening the database once for many sequential
!table reads and/or writes.  This speeds operations considerably.
!------------------------------------------------------------------------------
!This should be set outside the subroutine
  !ConnStr='DSN=MS Access Database;DBQ='//trim(fname)//&
  ! ';DefaultDir='//trim(DefaultDir)//';DriverId=25;FIL=MS Access;MaxBufferSize=2048;PageTimeout=5;UID=admin;'
  !open a connection to the excel workbook
iRet=SQL_SUCCESS
IF (WillOpen) THEN
  call f90SQLDriverConnect(ConnHndl, f90SQL_NULL_PTR, ConnStr, &
                         ConnStr,ConnStrLength,SQL_DRIVER_COMPLETE,iRet)
  if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then

      !Set connection attributes to allow read/write
      !(usually this is set before the connection is established, but there seems to be a bug
      !in the excel ODBC driver that does not recognize this setting if done before connecting)
      call f90SQLSetConnectAttr (ConnHndl, SQL_ATTR_ACCESS_MODE, &
                                 SQL_MODE_READ_WRITE, iRet) 

      if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then 
  
          !Set ODBC version we will be using (3.x in this case)
          call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, &
                      SQL_OV_ODBC3, iRet)
          
          !Allocate a statement handle
          call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet) 

      end if
  end if
END IF
if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then
   !Create an 'INSERT INTO' SQL query
   Question="VALUES ("
   SQLStmtStr='INSERT INTO ' // trim(Table) // ' ('
   LenInit=LEN(trim(SQLStmtStr))
   NFIELDS=NDPFIELD+NDATEFIELD+NSTRINGFIELD+nlogicfield
   if (NFIELDS.LE.0) THEN
      Error_10='No Fields Chosen'
      Return
   END IF
   do i=1,NDPFIELD
     if (LEN(trim(SQLStmtStr))>LenInit) SQLStmtStr=trim(SQLStmtStr)//","
     SQLStmtStr=trim(SQLStmtStr)//" ["//trim(Field(i))//"]"
     if (LEN(trim(Question))>8) then
       Question = trim(Question) // ",?"
     else
       Question = trim(Question) // "?"
     end if
   end do
   do i=1,NDATEFIELD
     if (LEN(trim(SQLStmtStr))>LenInit) SQLStmtStr=trim(SQLStmtStr)//","
     SQLStmtStr=trim(SQLStmtStr)//" ["//trim(DATEFIELDS(i))//"]"
     if (LEN(trim(Question))>8) then
       Question = trim(Question) // ",?"
     else
       Question = trim(Question) // "?"
     end if
   end do
   do i=1,NSTRINGFIELD
     if (LEN(trim(SQLStmtStr))>LenInit) SQLStmtStr=trim(SQLStmtStr)//","
     SQLStmtStr=trim(SQLStmtStr)//" ["//trim(STRINGFIELDS(i))//"]"
     if (LEN(trim(Question))>8) then
       Question = trim(Question) // ",?"
     else
       Question = trim(Question) // "?"
     end if
   end do
   do i=1,NLOGICFIELD
     if (LEN(trim(SQLStmtStr))>LenInit) SQLStmtStr=trim(SQLStmtStr)//","
     SQLStmtStr=trim(SQLStmtStr)//" ["//trim(LOGICFIELDS(i))//"]"
     if (LEN(trim(Question))>8) then
       Question = trim(Question) // ",?"
     else
       Question = trim(Question) // "?"
     end if
   end do
   SQLStmtStr = trim(SQLStmtStr)//") "//trim(Question)//");"

   !Prepare the SQL query
   call f90SQLPrepare(StmtHndl, SQLStmtStr, iRet)

   !bind SQL statement parameters to fortran variables
   FIELDNumber=0
   IF(NDPFIELD.GT.0) THEN
     DO KK=1,NDPFIELD
       FIELDNumber=FIELDNumber+1
       call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_DOUBLE, &
          SQL_DOUBLE, int(15,SQLUINTEGER_KIND), int(5,SQLSMALLINT_KIND), &
          XA(KK), f90SQL_NULL_PTR, iRet)
     END DO
   END IF
   IF (NSTRINGFIELD.GT.0) THEN
     DO KK=1,NSTRINGFIELD
       FIELDNumber=FIELDNumber+1
       STRINGS_LEN=LEN_TRIM(STRINGS(KK))
       call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_CHAR, &
           SQL_VARCHAR, int(255,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
           STRINGS(KK), f90SQL_NULL_PTR, iRet)
     END DO
   END IF
   IF(NDATEFIELD.GT.0) THEN
     DO KK=1,NDATEFIELD
       FIELDNumber=FIELDNumber+1
       call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_TYPE_TIMESTAMP, &
            SQL_TYPE_TIMESTAMP, int(15,SQLUINTEGER_KIND), int(10,SQLSMALLINT_KIND), &
            DATES(KK), f90SQL_NULL_PTR, iRet)
     END DO
   END IF
   IF(NlogicFIELD.GT.0) THEN
     DO KK=1,NlogicFIELD
       FIELDNumber=FIELDNumber+1
       call f90SQLBindParameter(StmtHndl, FIELDNumber, SQL_PARAM_INPUT, SQL_F_LONG, &
           SQL_INTEGER, int(0,SQLUINTEGER_KIND), int(0,SQLSMALLINT_KIND), &
           LINTEGER(KK), f90SQL_NULL_PTR, iRet)
      END DO
   END IF

     !loop Load Values and insert them into Table

   do ii=1,kount
       do kk=1,NDPFIELD
           XA(kk)=XI(ii,kk)
       end do
       DO KK=1,NSTRINGFIELD
          STRINGS(KK)=F90SQLSTRINGS(ii,KK)
       END DO
       DO KK=1,NDATEFIELD
          DATES(KK)=DATER(ii,KK)
       END DO
       DO KK=1,NLOGICFIELD
          LINTEGER(KK)=F90SQLINTEGER(ii,KK)
       END DO
       f90sqlIndex=IDI(ii)
       call f90SQLExecute(StmtHndl,iRet)
       !Check for error when adding rows
       if (iRet.ne.SQL_SUCCESS .and. iRet.ne.SQL_SUCCESS_WITH_INFO) then
           ERROR_10='Error reading records'
           call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
           IRETURN=-10
           exit
       endif
   enddo
else
    call ShowDiags(SQL_HANDLE_DBC,ConnHndl)
    ERROR10='Error opening connection to workbook'
    IRETURN=10
endif
IF (WillClose) THEN
  !Free connection
  call f90SQLDisconnect(ConnHndl,iRet)
  !release connection handle
  call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
  !release environment handle
  call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
END IF
return
end

subroutine AccessExecute(InSql)
!------------------------------------------------------------------------------
! This subroutine executes a Non-select query statement in a database.
! The original intent was to allow the execution
!load f90SQL modules
use f90SQLConstants
use f90SQL
USE FSQL_DATA
implicit none

! Subroutine Arguments
! Data Arguments
integer(kind=4):: kount
integer(kind=4):: ii,KK,NFIELDS
character(len=3):: CINTEGER
integer(SQLINTEGER_KIND),parameter:: MaxStringLen=255
integer(SQLHENV_KIND):: EnvHndl 
integer(SQLHDBC_KIND):: ConnHndl 
integer(SQLHSTMT_KIND):: StmtHndl 
integer(SQLRETURN_KIND)::iRet 
integer(SQLSMALLINT_KIND)::FIELDNumber,i,ConnStrLength
double precision XA(100)
character(len=MaxStringLen),INTENT(IN) :: InSql
character(len=MaxStringLen) SQLStmtStr, FMT
character(len=255) STRINGS(Nf90sql)
LOGICAL(KIND=4):: LLOGIC(Nf90sql)
INTEGER(KIND=4):: LINTEGER(Nf90sql),SELLEN
EQUIVALENCE (LLOGIC), (LINTEGER)
INTEGER(KIND=4):: STRINGS_LEN,MODE
character (len=255) :: ALLTRIM
character(len=MaxStringLen):: ConnStrOut,tempString
SQLStmtStr=InSql
!IF (CloseBefore) THEN
    !disconnect
!    call f90SQLDisconnect(ConnHndl,iRet)
    !release connection handle
!    call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
    !release environment handle
!    call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
!END IF
iRet=SQL_SUCCESS
if (iRet.EQ.SQL_SUCCESS.or.iRet.eq. SQL_SUCCESS_WITH_INFO) THEN
  if (WillOpen.AND.(iRet.EQ.SQL_SUCCESS.or.iRet.eq. SQL_SUCCESS_WITH_INFO)) then
      !allocate an environment handle
      call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)

      !Set ODBC version to use (3.x in this case)
      call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, iRet)

      !Allocate a connection handle
      call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl, iRet)

      !Note that we use a file-connection instead of a DSN connection
      !open a connection to the excel workbook
      call f90SQLDriverConnect(ConnHndl, f90SQL_NULL_PTR, ConnStr, &
          ConnStrOut, ConnStrLength, SQL_DRIVER_COMPLETE,iRet)

      !Set the connection up to be opened in Read/Write mode
      call f90SQLSetConnectAttr (ConnHndl, SQL_ATTR_ACCESS_MODE, &
                                 SQL_MODE_READ_WRITE, iRet) 
  end if
END IF
!Execute a non-Select SQL command
!This can be used to delete records before rewriting them
if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then
  !Allocate a statement handle
  call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet)
  !Instruct the driver to execute the statement
  !call f90SQLExecDirect(StmtHndl,SQLStmtStr,iRet)
  !Prepare the SQL query
  call f90SQLPrepare(StmtHndl,trim(SQLStmtStr),iRet)

  !Prepare the SQL query and execute the query
  call f90SQLExecute(StmtHndl,iRet)

  if (iRet.ne.SQL_SUCCESS .and. iRet.ne.SQL_SUCCESS_WITH_INFO) then
    IF (iRet.ne.SQL_NO_DATA) THEN
      Print *,"No data processed"
    ELSE
      ERROR_10='Error Executing Statement'
      call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
      IRETURN=-10
    END IF
  else
    print *,"Command executed"
  end if
ENDIF
IF (WillClose) THEN
    !disconnect
    call f90SQLDisconnect(ConnHndl,iRet)
    !release connection handle
    call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
    !release environment handle
    call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
END IF
return
end

subroutine AccessExeDSN(InSql)
!------------------------------------------------------------------------------
! This subroutine executes a Non-select query statement in a database.
! The original intent was to allow the execution
!load f90SQL modules
use f90SQLConstants
use f90SQL
USE FSQL_DATA
implicit none

! Subroutine Arguments
! Data Arguments
integer(kind=4):: kount
integer(kind=4):: ii,KK,NFIELDS
character(len=3):: CINTEGER
integer(SQLINTEGER_KIND),parameter:: MaxStringLen=255
integer(SQLHENV_KIND):: EnvHndl 
integer(SQLHDBC_KIND):: ConnHndl 
integer(SQLHSTMT_KIND):: StmtHndl 
integer(SQLRETURN_KIND)::iRet 
integer(SQLSMALLINT_KIND)::FIELDNumber,i,ConnStrLength
double precision XA(100)
character(len=MaxStringLen),INTENT(IN) :: InSql
character(len=MaxStringLen) SQLStmtStr, FMT
character(len=255) STRINGS(Nf90sql)
LOGICAL(KIND=4):: LLOGIC(Nf90sql)
INTEGER(KIND=4):: LINTEGER(Nf90sql),SELLEN
EQUIVALENCE (LLOGIC), (LINTEGER)
INTEGER(KIND=4):: STRINGS_LEN,MODE
character (len=255) :: ALLTRIM
character(len=MaxStringLen):: ConnStrOut,tempString
SQLStmtStr=InSql
IF (CloseBefore) THEN
    !disconnect
    call f90SQLDisconnect(ConnHndl,iRet)
    !release connection handle
    call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
    !release environment handle
    call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
END IF
iRet=SQL_SUCCESS
if (iRet.EQ.SQL_SUCCESS.or.iRet.eq. SQL_SUCCESS_WITH_INFO) THEN
  if (WillOpen.AND.(iRet.EQ.SQL_SUCCESS.or.iRet.eq. SQL_SUCCESS_WITH_INFO)) then
      !allocate an environment handle
      call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)

      !Set ODBC version to use (3.x in this case)
      call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, iRet)

      !Allocate a connection handle
      call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl, iRet)

      !Note that we use a file-connection instead of a DSN connection
      !open a connection to the excel workbook
      call f90SQLConnect(ConnHndl, 'Water Budget Output', 'Admin', '',iRet)

      !Set the connection up to be opened in Read/Write mode
      call f90SQLSetConnectAttr (ConnHndl, SQL_ATTR_ACCESS_MODE, &
                                 SQL_MODE_READ_WRITE, iRet) 
  end if
END IF
!Execute a non-Select SQL command
!This can be used to delete records before rewriting them
if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then
  !Allocate a statement handle
  call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet)
  !Instruct the driver to execute the statement
  call f90SQLExecDirect(StmtHndl,SQLStmtStr,iRet)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne.SQL_SUCCESS_WITH_INFO) then
    IF (iRet.ne.SQL_NO_DATA) THEN
      Print *,"No data processed"
    ELSE
      ERROR_10='Error Executing Statement'
      call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
      IRETURN=-10
    END IF
  else
    print *,"Command executed"
  end if
ENDIF
IF (WillClose) THEN
    !disconnect
    call f90SQLDisconnect(ConnHndl,iRet)
    !release connection handle
    call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
    !release environment handle
    call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
END IF
return
end


