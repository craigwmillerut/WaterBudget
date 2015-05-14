!-----------------------------------------------------------------------
!   BUDGET variable dictionary
!   EF------------------  PERCENT OF DIVERTED IRRIGATION WATER
!                         RETURNING TO THE STREAM (double)
!   EFM-----------------  RETURN PERCENTAGE OF M AMD I WATER (double)
!   FNYEARS-------------  NUMBER OF YEARS (double)
!   H1I-----------------  INITIAL HEAD (ELEVATION-TAIL WATER ELEVATION) (double)
!   IANPLT--------------  CREATE ANNUAL PLOT (logical)
!   IBUD----------------  Print crop water budget (logical)
!   IDISK---------------  SAVE QX'S ON DISK (logical)
!   QxFile(MQX)---------  QX, LAND AREA OR RESERVOIR NUMBERS FOR QX FILES
!   Qtype(MQX)----------  1 = QX, 6 = Land Area shortage, 2 = Reservoir
!   ResType(MQX,4)------  Logical (T/F), 1=Storage, 2=Area, 3=Elev, 4=Evap
!   IMNPLT--------------  CREATE MONTHLY PLOT (logical)
!   INEW----------------  No longer used (integer*4)
!   INYR----------------  Beginning year of the simulation (integer*4)
!   IPSH----------------  PRINT LAND AREA MONTH SHORTGS (logical)
!   IPST----------------  PRINT EOM STORAGE (logical)
!   IPEV----------------  PRINT MONTH EVAP (logical)
!   IPEL----------------  PRINT EOM ELEVATIONS (logical)
!   IPSA----------------  PRINT EOM SURF AREA (logical)
!   IPAS----------------  PRINT ANNUAL LAND AREA SHORTAGES (logical)
!   IQMN----------------  MINIMUM FLOW IN RIVER (logical)
!   IQMNL---------------  LIMIT MIN REL TO INF (logical)
!   IQIFLV--------------  READ QIN NAMES FROM DISK (logical)
!   IPQIN---------------  WRITE OUT QIN'S (logical)
!   ITERMX--------------  Max number of arsub iterations (integer*4)
!   IYEAR---------------  ARRAY OF THE YEARS OF SIMULATION (integer*4)
!   NFDT----------------  Number of flow duration QXs (integer*4)
!   NHPW----------------  Number of powerplants (integer*4)
!   NLND----------------  Number of land areas (integer*4)
!   NQIN----------------  Number of inflows to simulation (integer*4)
!   NQX-----------------  NUMBER OF QX'S (integer*4)
!   NQXMN---------------  Number of minimum flows (integer*4)
!   NRES----------------  Number of reservoirs in the simulation (integer*4)
!   NYRS----------------  Number of years (integer*4)
!   PLOTFL--------------  Plotfile (IANPLT=.TRUE.)
!   PLOTFL2-------------  Plotfile (IMNPLT=.TRUE.)
!   QDMI----------------  ARRAY WITH M AND I DEMAND BY MONTH IN THE LAND AREAS
!                         (double)
!   QDVR----------------  ARRAY WITH TOTAL DIVERSION REQUIREMENT FOR THE LAND
!                         AREAS (QDVRA+QDMI) (double)
!   QDVRA---------------  ARRAY WITH VOLUME OF WATER NEEDED AT DIVERSION TO LAND
!                         AREAS BY MONTH (double)
!   RAR-----------------  INITIAL RESERVOIR SURFACE AREAS (double)
!   REL-----------------  ARRAY WITH THE INITIAL RESERVOIR ELEVATIONS (double)
!   SPLOT---------------  Call Graph90 (logical)
!   STO-----------------  INITIAL STORAGE IN THE RESERVOIRS (double)
!   LinesPerPage--------  The number of printed lines per page
!   Landarea(M,I)-------  Estimated private domestic from landareas(M,I)
!                         associated with municipal area M
!   OutPutFile(M)-------  Model Output from QX(M) (Res, Ag area Etc.) to file M
!   OutType(M)----------  Type of output, 1=QX, 2=Res. Output, 6=Ag Sort
!   OutNumber(M)--------  Output for QX(M) (Res(M), Ag area(M), Etc.)
!   OutResSto(M)--------  Output Reservoir Storage for Res(M)? (1,0)
!   OutResArea(M)-------  Output Reservoir Area for Res(M)? (1,0)
!   OutResElev(M)-------  Output Reservoir Elev. for Res(M)? (1,0)
!   OutResEvap(M)-------  Output Reservoir Evap. for Res(M)? (1,0)
!   NOUTFL--------------  Number of output files in standard Division format
!   NssPTs(L,M)---------  Number of data years for SS Industry M in Municipal
!                         area L
!   SSyear(L,M,J)------   Self-supplied ind. M, municipal area L year J
!   SSin(L,M,J)---------  Self-supplied industry potable indoor use industry M,
!                         year J, for municipal area L
!   SSout(L,M,J)--------  Self-supplied industry outdoor potable use industry M,
!                         year J, for municipal area L
!   SSComm(L,M,J)-------  Self-supplied ind. M commercial potable use, year J,
!                         municipal area L
!   SSInd(L,M,J)--------  Self-supplied ind. M industrial potable use, year J,
!                         municipal area L
!   SSInst(L,M,J)-------  Self-supplied ind. M institutional potable use, year J,
!                         municipal area L
!   SSCout(L,M,J)-------  Self-supplied ind. M outdoor secondary use, year J,
!                         municipal area L
!   SSCcomm(L,M,J)------  Self-supplied ind. M commercial secondary use, year J,
!                         municipal area L
!   SSCInst(L,M,J)------  Self-supplied ind. M instituional secondary use, year J,
!                         municipal area L
!   SSCInd(L,M,J)-------  Self-supplied ind. M industrial secondary use, year J,
!                         municipal area L
!   BegSoilMatch--------  Match beginning and ending soil moisture (logical)
!   Calibrate-----------  Calibrate a landarea parameter (logical)
!   CalType-------------  Calibration type 0-On-Farm Efficiency, 1-Convenance Eff,
!                           2-Return Flow Factors
!   CalibArea-----------  Land area ID for the calibrated parameter (integer)
!   CalibDesc-----------  Land area description for the calibrated parameter (character)
!   MaxCal--------------  Maximum value for calibrated parameter
!   MinCal--------------  Minimum value for the calibrated parameter
!   InitialCal----------  Initial value for the calibrated parameter
!   NotNegative(L)------  Logical variable indicating whether QIN(L) should have
!                         negative values filled in with zeroes.
!   LRESROW(L,M)--------  For QXin(M) of land area L, reservoir number serving area.
!   RetUse(L)-----------  Use of Ag Area L return flow to supply sub-irrigated
!                         land in Area L
!   Pumped(L)-----------  Pumped groundwater delivered to ag areas L in this
!                         month and year, Used for summary page
!   StudyDeplete(J,K)---  Model depletion for all municipal and agricultural areas,
!                         includes man-caused riparian depletion
!   AgDeplete(L,J,K)----  Agricultural area L depletions for month K and year J
!   GWMining(L)---------  Average amount of groundwater mining per year for Ag
!                         area L
!   Subareas(L)---------  Subarea code L for each of the subareas in the current
!                         water budget.
!   Basin(L)------------  Basin code L for each subarea
!   BasinName(L)--------  Basin name L for each subarea
!   NSUBS---------------  Number of subareas read for this model
!   IgnoreRiparian------  Ignore Riparian demands in this water budget model
!   RipPro(L)-----------  Proportion of Landarea L riparian ET to include
!-----------------------------------------------------------------------
!   NmonQXPlt-----------  The number of monthly QXs to plot (integer)
!   NannQXplt-----------  The number of annual QXs to plot (integer)
!   CorrArr(J,13)-------  Temp array with base correlation station data (double)
!   CorrCoeff(L,13,2)---
!   TypeYear------------  The type of year used in study, 0=Water Year, 1=
!                         Water Year starts November 1, 2=Calendar year
!   ReadIt(J,K)---------  Logical array shwoing whether a variable was read in
!                         for year J month K.
!   theOrder(M)---------  The order of printed QX(M).
!   OptMiz--------------  Optimize model for a variable
!-----------------------------------------------------------------------
!   MINIMUM FLOW
!   IQNUM(L)------------  QX number of minimum flow L (integer)
!   IQRES(L)------------  Reservoir number serving minimum flow L (integer)
!   QXMN(K,L)-----------  Minimum flow for IQNUM(L) month K (double)
!   QXmnID(L)-----------  Unique ID of QXMN(L) in database system (integer)
!   NumMinQXup(L)-------  Num. of min. flow QXs upstream of IQNUM(L) (integer)
!-----------------------------------------------------------------------
!   PRESV(M)------------  Description of reservoir M (string)
!   QMNLMT(M)-----------  Limit release to inflow, reservoir M (logical)
!   SEEPTB(M)-----------  Seepage tabele, reservoir M (logical)
!   ISEPQ(M)------------  Seepage rounted through ISEPQ(M) (integer)
!   NSEEP(M)------------  Points in seepage table, reservoir M (integer)
!   ESEEP(I,M)----------  Seepage table elev. (feet), reservoir M (double)
!   SSEEP(I,M)----------  Seepage table flow (cfs), reservoir M (double)
!   STAGFIL-------------  File containing record of stage data
!   (A32,2L1,I5,16F8.2,A80)
!   DAM_NUMBER(M)-------  Water Rights dam ID, reservoir M (string*50)
!   DAM_NAME(M)---------  Water Rights dam name, reservoir M (string*50)
!   EvapOpt(M)----------  Evaporation Option 0=Use Model Evaporation, 1-Enter
!                         Monthly Factors, 2-Use QIN defined rate in ft/month,
!                         3-Use QIN defined rate in AF/month.
!   EvapFac(M,K)--------  Monthly evaporation factor in feet/month for Reservoir
!                         M for month K.
!   EvapFactor(M)-------  Factor to be applied to model default evaportion.
!                         Default value is 1.0.
!-----------------------------------------------------------------------
!(I5,3F10.0)
!   NP(M)
!   SMX(M)
!   SMN(M)
!   STOIC(M)
!-----------------------------------------------------------------------
!(13F5.2)
!   EVRTRead(M,J)-------  Evaporation read for res. M year J (logical)
!   EVRT(M,J,K)---------  Evaporation for reservoir M year J month K (double)
!   AvEvap(K)-----------  Evaporation for month K for period of record
!                         This is used to fill in missing records
!   NumEvap-------------  Number of recorded evaporation for month K
!-----------------------------------------------------------------------
!(13F5.2,L1)
!   QSM(M,K),K=1,13-----  Monthly seepage for reservoir M (double)
!   IPQN(M)-------------  Inflow QX to reservoir M
!-----------------------------------------------------------------------
!(13F5.2)
!   QRMN(M,K),K=1,13----  Monthly minimum flows (double)
!   E(M,N),N=1,NP(M)----  Elevation of reservoir M (double)
!   A(M,N),N=1,NP(M)----  Area of reservoir M (double)
!   V(M,N),N=1,NP(M)----  Volume of reservoir M (double)
!   QTG(M,K),K=1,13-----  Target (minimum) flows below reservoir M (double)
!   IQTG(M,I)-----------  QXs downstream of reservoir M (integer)
!   NQTG(M)-------------  Number of downstream QXs of reservoir M (integer)
!   IQXIN(M,I)----------  Inflow QXs for reservoir M
!   NQXIN(M)------------  Number of inflow QXs for reservoir M
!   IREST(M)------------  Reservoir # downstream of reservoir M (integer)
!-----------------------------------------------------------------------
!(12F5.0,F6.0)
!   OFMN(M,I),I=1,13)---  Offstream reservoir M flows
!-----------------------------------------------------------------------
!(19I3)
!   IQXAD(M,I),I=1,19)--  QX numbers downstream of reservoir release (integer)
!-----------------------------------------------------------------------
!(4L1)
!   ISTOR(M)------------  Read monthly storage from a file?
!   IREL(M)-------------  Read monthly release from a file?
!   PRNRS(M)------------  Print stor., cap., and elev. data for reservoir M?
!   IRSRED(M,3)---------  Is data read in for 1) Release 2) Storage 3)Evap
!-----------------------------------------------------------------------
!   TargetRelOpt(M)-----  Target release option for reservoir M (integer)
!                         0="No specified release",1="Monthly Release Table"
!                         2="Year and Month (requires QIN)
!   SeepQXOpt(M)--------  Seepage option for reservoir M (integer)
!                         0="No seepage",1="Stage-Seepage table"
!                         2="Monthly Seepage Table"
!   TargetStorOpt(M)----  Target storage option reservoir M (integer)
!                         0="No specified target storage"
!                         1="Monthly Target Storage Table",
!                         2="Year and Month (requires QIN)"
!   TargetStorQIN(M)----  Target storage QIN for reservoir M (integer)
!   EvapQIN(M)----------  QIN number for year/month reservoir evap. (integer)
!   StorTarg(M,K)-------  Storage taget for reservoir M month K (double)
!   AVRESET(M,K)--------  Average monthly ET for reservoir M month K (double)
!   TargetRelQIN(M)-----  QIN number of target release for reservoir M (integer)
!   QxMap(M)------------  The index number of QX M which will be printed.
!   StoDiff(M,J,K)------  Difference in storage for Res M, Year J, Month K
!-----------------------------------------------------------------------
!(A80)
!   STOFIL(M)-----------  File for storage of reservoir M by month/year(string)
!-----------------------------------------------------------------------
!(A80)
!   RELFIL(M)-----------  File for release of reservoir M month/year (string)
!-----------------------------------------------------------------------
!(A80)
!   EVAFIL(M)
!   TargetOpt(M)--------  0-No target release,1-Monthly, 2-Month, Year (integer)
!   PrintStage(M)-------  Print stage for reservoir M (logical)
!   PrintArea(M)--------  Print area for reservoir M (logical)
!   PrintCapacity(M)----  Print volume for reservoir M (logical)
!   ReserID(M)------------  ID of reservoir used in other datatables (integer)
!-----------------------------------------------------------------------
!(A32,2L1,F3.0,2F2.0,F5.1,6F2.0,2F5.1,2L1)
!   PLAND(L)------------  The printer heading for land area L (string)
!   LAVPRE(L)-----------  Use average precipitation for land area L (logical)
!   LAVTEM(L)-----------  Use average temperature for land area L (logical)
!   PRDEG(L)------------  Project latitude degrees for land area L (double)
!   PRMIN(L)------------  Project latitude minutes for land area L (double)
!   PRSEC(L)------------  Project latitude seconds for land area L (double)
!   EFPRE(L)------------  Effective precipitation for land area L (double)
!   BMN28(L)------------  Spring 28 deg frost month for land L (integer)
!   BDY28(L)------------  Spring 28 deg frost day for land L (integer)
!   MON28(L)------------  Fall 28 deg frost month for land L (integer)
!   DAY28(L)------------  Fall 28 deg frost day for land L (integer)
!   MON32(L)------------  Fall 32 deg frost month for land L (integer)
!   DAY32(L)------------  Fall 32 deg frost day for land L (integer)
!   CEFF(L,J)-----------  Canal efficiency for land area L, year J (double)
!   IEFF(L,J)-----------  Irrigation efficiency for land area L, year J (double)
!   PRNLND(L)-----------  Print land area L? (logical)
!   LCALEN(L)-----------  Calculate ET from year/month temperature (logical)
!-----------------------------------------------------------------------
!   LandID(L)-----------  The ID for land area L (integer)
!   GWQX(L)-------------  Groundwater QX land area L (integer)
!   LandAreaCode(L)-----  Land area code area L (string)
!   SubArea(L)----------  Subarea for land area L (string)
!   LandAreaName(L)-----  Printed name for land area L (string)
!   NLndQX(L)-----------  The number of QXs serving area L (integer)
!   IQ------------------  QX counter for operations on land QXs (integer)
!   NILNQ(L,IQ)---------  Number of upstream QXs connecting QX IQ to
!                         upstream reservoir
!   CONUSE(K,J,ILND)----  Potential consumptive use for land area ILND,
!                         for year J and month K
!   WCUSE(ILND,J,K)-----  Potential consumptive use for sub-irrigated
!                         crops area ILND, year J, month K
!   PERCO(K,J,L)--------  Effective irrig. cropland precipitation (acre-feet)
!                         in excess of consumptive use.
!   WPERC(K,J,L)--------  Effective sub irrig. cropland precipitation (acre-feet)
!                         in excess of consumptive use.
!   WETUSE(K,J,L)-------  Pot. riparian consumptive use area L year J month K
!   WETPRE(K,J,L)-------  Rainfall over riparian area L year J month K
!   RTUSE(L,K)----------
!   GWUSE(L,K)----------  The estimated use of ag groundwater month K ag area L
!                         proportioned as CONUSE.
!   SBUSE(L,K)----------
!   SHORT(L,J,K)--------
!   FBYPS(L,J,K)-------   Min flow for QX L, year J, month K
!   CANAL---------------
!   SubPrecip(L,J,K)----  Precipitation in inches land area L, Year J, Month K
!   SubCropAcres(L,J,M)-  Crop acres land area L, year J, for crop M defined as
!                         1-Alfalfa,2-Pasture,3-Hay,4-Grain,5-Corn,6-Orchard,
!                         7-Sorghum,8-Turf,9-Onions,10-Other Hort,11-Potatoes,
!                         12-Berries,13-Other Veg,14-Tomatoes,15-Beans,
!                         16-Vineyard,17-Subirrigated Hay,18-Subirr. Pasture
!   SubCropAv(M)--------  Average crop acres for crop M (defined above)
!   SubCropAc(L,M)------  Average subarea crop M acreage (as defined above)
!   TotalAcres(L)-------  Total cropland in area L
!   Alfalfa(L)----------  Acreage of alfalfa in area L
!   Pasture(L)----------  Acreage of pasture in area L
!   Hay(L)--------------  Acreage if hay in area L
!   Grain(L)------------  Acreage of grain in area L
!   Corn(L)-------------  Acreage of corn in area L
!   Orchard(L)----------  Acreage of orchard in area L
!   Sorghum(L)----------  Acreage of sorghum in area L
!   Turf(L)-------------  Acreage of turf in area L
!   Onions(L)-----------  Acreage of onions in area L
!   OtherHort(L)--------  Acreage of other horticulture in area L
!   Potatoes(L)---------  Acreage of potatoes in area L
!   Berries(L)----------  Acreage of berries in area L
!   OterhVeg(L)---------  Acreage of other vegetation in area L
!   Totmatoes(L)--------  Acreage of tomatoes in area L
!   Beans(L)------------  Acreage of beans in area L
!   Vineyard(L)---------  Acreage of vineyard in area L
!   SubHay(L)-----------  Acreage of subirrigated hay area L
!   SubPast(L)----------  Acreage of subirrigated pasture in area L
!   SM(L,I)-------------  Soil moisture holding capacity in foot I below
!                         surface for land area L
!   SubIrrPro-----------  The proportion subirrigated ET lake area L
!   SubIrrET------------  The amount of ET from subirrigated land area L
!   AnnGW(L,J)----------  Groundwater use area L year J in acre-feet (double)
!   AnnGWread(L,J)------  Logical value indicating whether year has been read
!   NMUN----------------  The number of municipal demand areas (integer)
!   MunGroup(L)---------  Municipal Group name (string 32)
!   MUNID(L)------------  Municipal ID municipal group L
!   OutDoorFac(L,K)-----  Month K factor for outdoor use for municipal group L
!   MunProv(L,M)--------  Community provider M WatResID in municipal group L
!   MunName(L,M)--------  Community provider M name in municipal group L
!   NCOMM(L)------------  Number of community systems in mun. grp. L
!   NumNonComm(L)-------  Number of non-community systems in mun. grp. L
!   NonComm(L,M)--------  NonCommunity provider M WatResID in Mun. Grl. L
!   NonName(L,M)--------  NonCommunity provider M name in Mun. Grp. L
!   NQXMun(L)-----------  The number of QX inflows to mun. grp. L
!   MunQXid(L,M)--------  Database ID of MunQXin(L,M)
!   MunQXin(L,M)--------  QX M into Mun. Grp. L
!   MunQXup(L,M)--------  QX M upstream of Mun. Grp. L
!   MunQXdn(L,M)--------  QX M downstream of Mun. Grp. L
!   MDamID(L,M)---------  Dam ID providing water for MunQXin(L,M)
!   MResID(L,M)---------  Reservoir ID serving MunQXin(L,M)
!   NMunSS(L)-----------  Number of self-supplied industries in Mun. Grp. L
!   MSSWresID(L,J)------  Water Res. ID for self-supplied industry J in Mun.
!                         Grp. L
!   MSSname(L,J)--------  Name of self-supplied industry J in Mun. Grp. L
!   NQXup(L,M)----------  Number of upstream QXs from MunQXin(L,M) to connect
!                         to upstream reservoir MResID(L,M)
!   MQXup(L,M,N)--------  QX N upstream of MunQXin(L,M) connecting it to
!                         to upstream reservoir MResID(L,M)
!   NumOutflow----------  The number of model outflows
!   NumOutEvap----------  The number of SubBasin outflows that are completely
!                         evaporated.
!   NOUEVFL(J)----------  The QX numbers of SubBasin outflows that are
!                         completely evaporated
!   NumMainStem---------  The number of mainstem inflows
!   NumTributary--------  The number of tributary inflows
!   NumImport-----------  The number of import inflows
!   NumExport-----------  The number of export flows
!   NumUngauged---------  The number of ungauged inflows
!   NumOutflow----------  The number of outflows
!   NOUFL(J)------------  Outflow number J for the model
!   TranNum-------------  The number of Transbasin Imports or Exports
!   TranQX(J)-----------  The QX number of Tranbasin Import or Export J
!   TranDir(J)----------  Whether TranQX(J) is an Import or Export
!   TranBasin(J)--------  Basin code of other basin for export or import J
!   TranName(J)---------  Basin name of other basin for export or import J
!   WSOIL(L)------------  Subirrigated soil moisture capacity ag area L
!   PRE(L,J,K)----------  Precipitation (inches) ag area L year J month K
!   EffPre(L,J,K)-------  Effective Precipitation (acre-feet) ag area L
!                         year J month K
!   WTPRE(L,J,K)--------  Precipitation (inches) rip area L year J month K
!   TEM(L,J,K)----------  Temperature (Fahrenheit) ag area L year J month K
!   WTEM(L,J,K)---------  Temperature (Fahrenheit) rip area L year J month K
!   WPRO(L,J)-----------  The proportion of subirrigated water use area L year J
!   PropUse(L)----------  The proportion of Land Area used for Land Area L calcs
!   MunGW(L,J,K)--------  The acre-feet of groundwater diverted for demands
!                         municipal area L for year J month K
!   MunSurf(L,J,K)------  The acre-feet of surface water diverted to satisfy
!                         demands municipal area L year J month K
!   PondArea(L)---------  The area in acres of area L sewage pond
!   PondET(L)-----------  The ET in inches of area L sewage pond
!   RetIndoor(M,J,K)----  Return flow from Municipal Area M indoor M&I use year
!                         J, month K
!   RetOutdoor(M,J,K)---  Return flow from outdoor Mun. Area M M&I use year
!                         J month K
!   DivIndoor(M,J,K)----  Indoor diversion for provider M year J month K
!   DivOutdoor(M,J,K)---  Outdoor diversion for provider M year J month K
!   DepIndoor(L,J,K)----  Indoor depletion from Mun. Area L M&I use year J
!                         month K
!   DepOutdoor(L,J,K)---  Depletion from outdoor M&I use Mun. Area L year J
!                         month K
!   ResReturn-----------  The proportion of return flow from residential use
!   ComReturn-----------  The return flow proportion from commercial use
!   InstReturn----------  Return flow proportion from institutional use
!   IndReturn-----------  Return flow proportion from industrial use
!   SewageReturn--------  Return flow proportion from sewage systems
!   SepticReturn--------  Return flow proportion from septic systems
!   OutDoorReturn(L,J)--  Return flow proportion from lawn and garden use,
!                         municipal area L, year J
!   MunReturn(L,J,K)----  Return flow from municipal area L year J month K
!   MunRetOut(L,J,K)----  OutDoor return flow mun. area L year J month K (not used)
!   MunDep(L,J,K)-------  Depletion municipal area L year J month K
!   MunQXRet(L)---------  Return flow QX number Land area L
!   MResNum(L,M)--------  Reservoir number supplying flow to inflow M to area L
!   MunDemIn(L,J,K)-----  Indoor municipal demand for area L year J month K
!   MunDemOut(L,J,K)----  Outdoor municipal demand for area L year J month K
!   PrivDomestic(L)-----  Private domestic demand municipal area L
!   MonYield(J,K)-------  Study area yield for year J month K
!   SubNatUse(J,K)------  Study area natural vegetation use year J month K
!   MunDepPot(M,J,K)----  Municipal potable depletions area M, year J month K
!   MunDepSec(L,J,K)----  Municipal secondary depletions Mun area L year J month K
!   MunDepPTot(J,K)-----  Municipal potable depletion all areas year J month K
!   MunDepSTot(J,K)-----  Municipal secondary depletion all areas year J month K
!   MResCal(M,N)--------  The amount of reservoir diversions to Mun Area M Inflow N
!   Needed(J)-----------  Initially .TRUE. and set to false as each required year
!                         to be used for QIN is either read or correlated
!-----------------------------------------------------------------------
!(A80)
!   PREFIL(L)-----------  Precipitation file land area L (string)
!-----------------------------------------------------------------------
!(A80)
!   TEMFIL(L)-----------  Temperature file land area L (string)
!-----------------------------------------------------------------------
!(12F6.4,F6.1)
!   EFMIP(L,K)----------  Municipal efficiency area L month K (double)
!   LGEFF(L)------------  Lawn and garden efficiency area L (double)
!-----------------------------------------------------------------------
!(F9.1,16X,F6.4,L1,2F5.2)
!   MUNDEM(L)-----------  Muncipal demand area L (double)
!   RETI(L)-------------  Deep percolation percent return area L (double)
!   ISOL(L)-------------  Use soil moisture area L (logical)
!   SOIL(L)-------------  Soil holding capacity area L in acre-feet (double)
!   BEGSOL(L)-----------  Beginnning soil moisture area L in acre-feet (double)
!-----------------------------------------------------------------------
!(12F6.4)
!   QMIP(L,K)-----------  Municipal demand factors area L month K (double)
!-----------------------------------------------------------------------
!(12F5.2,4I2)
!   PR(L,K)-------------  Monthly precip. area L month K (double),I=1,12)
!   IPH(L)--------------  QX number serving wetlands area L (double)
!   IPHU(L)-------------  QX number upstream of wetlands area L (double)
!   IPHD(L)-------------  QX number downstream of wetlands L (double)
!   NQLIN(L)------------  Number of inflows area L (integer)
!-----------------------------------------------------------------------
!(4I3,L1)
!   IDV(L,I)------------  Diversion I to land area L (integer)
!   IBY(L,I)------------  Bypass I land area L (integer)
!   ILQIN(L,I)----------  QIN # to load into IDV(L,I) (integer)
!   ILUP(L,I)-----------  QX number upstream of diversion I land area L (integer)
!   ISBY(L,I)-----------  Are minimum flows required downstream of
!                         IDV(L,I)? (logical)
!-----------------------------------------------------------------------
!(12F6.0,F7.0)
!   FBYPS(L,J,K)--------  Bypass flows area L diversion I month K (double)
!-----------------------------------------------------------------------
!(12F5.1,6X,4I3)
!   TM(L,K)-------------  Average monthly temperature area L month K (double)
!   IRV(L)--------------  QX number to wet pasture area L (integer)
!   IFT(L)--------------  Bypass QX to wet pasture area L (integer)
!   IRT(L)--------------  Return QX for area L (integer)
!   IUR(L)--------------  QX upstream wet pasture area L (integer)
!-----------------------------------------------------------------------
!(10F4.2,6X,I3,I3)
!   PCRF(L,I)-----------  Lag factors (10) for area L (double)
!   IGRND(L,K)----------  Month K pro. mun. demand L from groundwater (double)
!   LNRS(L)-------------  Number reservoirs serving ag area L (integer)
!   NCRP(L)-------------  Number of crops area L (integer)
!-----------------------------------------------------------------------
!   MLNRS(L)------------  Number reservoirs serving municipal area L (integer)
!   MRESUP(L,I)---------  The reservoir number for the Ith reservoir to serve
!                         municipal area L
!   MunGWQx(L)----------  QX number for groundwater supply, municipal area L
!-----------------------------------------------------------------------
!(A6,F10.1,F6.2,A40,A13)
!   CPCODE(L,I)---------  Crop code I area L (string)
!   CPAC(L,I)-----------  Acres crop I area L (double)
!   CPRO(L)-------------  Proporion GW use subirr. by crops area L (double)
!   CPNAM(L,I)----------  Crop I name area L (string)
!   LNTYP(L,I)----------  Crop I type (sub-Irr, etc.) area L (string)
!                         Types are "Cropland", "Lawn&Garden",
!                         "Sub-Irrigated"
!-----------------------------------------------------------------------
!(12F7.3)
!   CPLOVR(L,I,K)-------  Mon. K factor crop I area L (double)
!-----------------------------------------------------------------------
! IF(LNRS(L).GT.0)------  If the number of reservoirs serving aree L>0
!-----------------------------------------------------------------------
!(33I3)
!   LRES(L,I)-----------  Reservoir I serving area L (integer)
!   BEGDEL(L,I)---------  Beginning delivery month res. I area L (integer)
!   ENDDEL(L,I)---------  Ending delivery month res. I area L (integer)
!-----------------------------------------------------------------------
!(20I3)
!   ILNQ(L,M,MM)--------  QX (1-MM) to reservoir MM (integer), This is the
!                         corresponding QX number for land area L for inflow
!                         M and upstream QX ILNQ(L,M,MM)
!-----------------------------------------------------------------------
!   Municipal variables
!   ResInUse(M,J,K)-----  Residential indoor use municipal area M year J month K
!   ResOutUse(M,J,K)----  Res. outdoor use municipal area M year J month K
!   CommUse(M,J,K)------  Commercial potable use municipal area M year J month K
!   InstUse(M,J,K)------  Institutional use area M year J month K
!   IndUse(M,J,K)-------  Industrial use area M year J month K
!   Secondary(M,J,K)----  Secondary water use area M year J month K
!   NonResIn(M,J,K------  Non-Community indoor water use area M, year J month K
!   NonResOut(M,J,K)----  Non-Community outdoor water use area M, year J month K
!   NonCommUse(M,J,K)---  Non-Community commercial water use area M,
!                         year J month K
!   NonInst(M,J,K)------  Non-Community institutional water use area M,
!                         year J, month K
!   NonInd(M,J,K)-------  Non-Community industrial water use area M, year J
!   NonSecondary(M,J,K)-  Non-Community secondary water use area M, year J
!   SSIDep(L,J,K)-------  For municipal area M, depletion for Self-supplied industry
!                         in month K, year J.
!   SSIDepM(L,K)--------  For municipal area M, average depletion for Self-supplied
!                         industry in month K.
!   SSIDeplete(J,K)-----  Depletion for Self-supplied industry in month K, year J.
!   SSIDepleteM(K)------  Average depletion for Self-supplied industry in month K.
!   BasinIn(J,K)--------  Basin inflow for year J month K
!   BasinOut(J,K)-------  Basin outflow for year J month K
!-----------------------------------------------------------------------
!   NRIP----------------  Number of riparian areas (integer)
!-----------------------------------------------------------------------
!     PARAMETER FILE FOR DIMENSIONING THE FORTRAN PART OF THE RESERVOIR
!     SIMULATION PROGRAM
!
!     MRES--------------  MAXIMUM NUMBER OF RESERVOIRS (parameter integer*4)
!     MLAND-------------  MAXIMUM NUMBER OF LAND AREAS (parameter integer*4)
!     MQX---------------  MAXIMUM NUMBER OF QX'S (pa
!     MYEAR ------------  MAXIMUM NUMBER OF YEARS IN THE SIMULATION
!     MPP --------------  MAXIMUM NUMBER OF POWER PLANTS
!     MQSTA ------------  MAXIMUM NUMBER OF INPUT STREAMFLOW STATIONS
!     MGRN -------------  DIMENSION FOR GRNC
!     MSPC -------------  DIMENSION FOR WINSPC
!     MFLC -------------  DIMENSION FOR WINFLC
!     MMIN--------------  MAXIMUM NUMBER OF MINIMUM FLOWS
!     MCRPS-------------  MAXIMUM NUMBER OF CROPS
!     MRIPA-------------  NUMBER OF RIPERIAN LANDS SIMULATED
!-----------------------------------------------------------------------
!     UniqAgDivQX(L,M)--  For Ag Area L, QX(M,J,K) is listed once in this array
!     UniqMIDiv(L,M)----  For M&I area L, QX(M,J,K) is listed once in this array
!     NumAGQX(L)--------  The number of unique QXs serving ag area L
!     NumMIQX(L)--------  The number of unique QXs serving M&I area L
!     FileData----------  Text file name to be written and read by a VB.NET
!                         program and written to output.accdb
!     RetFlow-----------  Land area return flow RetFlow(K) written to LADATA

Module PARAMDIM
      INTEGER*4 MRES,MLAND,MQX,MYEAR,MPP,MQSTA,MGRN,MSPC,MFLC,MMIN
      INTEGER*4 MCRPS,MRIPA,MPTS,MUNPRV,MCROP
      PARAMETER (MRES=10)
      PARAMETER (MLAND=10)
      PARAMETER (MQX=150)
      PARAMETER (MYEAR=80)
      PARAMETER (MPP=5)
      PARAMETER (MQSTA=80)
      PARAMETER (MGRN=21)
      PARAMETER (MSPC=10)
      PARAMETER (MFLC=10)
      PARAMETER (MMIN=10)
      PARAMETER (MCROP=20)
      PARAMETER (MCRPS=60)
      PARAMETER (MRIPA=11)
      PARAMETER (MUNPRV=MLAND*15)
      PARAMETER (MPTS=12*MYEAR)
      integer (KIND=4),PARAMETER :: NFLO=19
      INTEGER(KIND=4),PARAMETER:: IZERO=0
      INTEGER (KIND=4),PARAMETER :: MIZER=MLAND*13
      integer (KIND=4),PARAMETER :: MLTWL=MLAND*12
      integer (KIND=4),PARAMETER :: MCRP=20
      integer (KIND=4),PARAMETER :: NIN=20
      integer (KIND=4),PARAMETER :: QXZER=MQX*MYEAR*13
      integer (KIND=4),PARAMETER :: EVZER=MRES*MYEAR*13
      INTEGER (KIND=4) :: J,K,L
End Module PARAMDIM

MODULE COMMO
use PARAMDIM
      INTEGER (KIND=4) :: NP(MRES)
      REAL (KIND=8) :: QX(MQX,MYEAR,13),DMD,QDVM,EVRT(MRES,MYEAR,13), &
            QRMN(MRES,13),SMX(MRES),SMN(MRES),STO(MRES), &
            RAR(MRES),REL(MRES),ST(MRES,MYEAR,13), &
            EVAP(MRES,MYEAR,13)
      REAL (KIND=8) :: GWUSE(MLAND,13),RTUSE(MLAND,13),SBUSE(MLAND,13)
      REAL (KIND=8) :: V(MRES,3000),A(MRES,3000),E(MRES,3000)
      DATA ((GWUSE(L,K),K=1,13),L=1,MLAND)/MIZER*0.0/
      DATA ((RTUSE(L,K),K=1,13),L=1,MLAND)/MIZER*0.0/
      DATA ((SBUSE(L,K),K=1,13),L=1,MLAND)/MIZER*0.0/
      DATA (((QX(L,J,K),L=1,MQX),J=1,MYEAR),K=1,13) /QXZER*0.0/
      DATA (((EVRT(L,J,K),L=1,MRES),J=1,MYEAR),K=1,13) /EVZER*0.0/
      DATA (((ST(L,J,K),L=1,MRES),J=1,MYEAR),K=1,13) /EVZER*0.0/
      DATA NP/MRES*0/
END MODULE

MODULE SubVars
use PARAMDIM
      REAL (KIND=8) :: SubPrecip(MLAND,MYEAR,13)
END MODULE

Module PrintStuff
!     INTEGER*4 VARIABLES
use PARAMDIM
!     INTEGER*4 VARIABLES
      INTEGER (KIND=4) :: NRES,INYR,NHPW,KPS,KCE,NQIN, &
            KPE,KSS,NLND,KBS,IHS(MPP),IHP(MPP),I, &
            IHB(MPP),IHR(MPP),IHNP(MPP),IHPQX(MPP,5),QxMap(MQX), &
            KSI(MPP),KEI(MPP),NCRP(MLAND),KSE,NYRS, &
            KBE,NQS,KCS,LINE,NEXPO,NUNG,NOUFL(NFLO),ITERMX, &
            IQTG(MRES,19),IOFQX(MRES,19),IQXAD(MRES,19), &
            ICKIN(MQX),ICKOU(MQX),ITYIN(MQX), &
            ITYOU(MQX),ICKBY(MQX),ITYBY(MQX),ICKGW(MQX),ITYGW(MQX), &
            ICKUP(MQX),ITYUP(MQX),ICKRS(MQX,MRES),ITYRS(MQX), &
            NDEML(MLAND,MRES),LMRES(MLAND,MRES),IYEAR(MYEAR),IPQX(MQX), &
            IPAGE,NQX,IQH(MRES),IINFL(NFLO),NINFL,ITRIB(NFLO),NTRIB, &
            IIMP(NFLO),NIMP,IEXPO(NFLO),IUNG(NFLO),IQXN(MQSTA), &
            IREST(MRES),IOFBY(MRES),IOFUP(MRES),IQNUM(MMIN), &
            IQRES(MMIN),IPH(MLAND),IPHU(MLAND),IPHD(MLAND), &
            NQLIN(MLAND),IRV(MLAND),IFT(MLAND),IRT(MLAND), &
            LNRS(MLAND),KI(MPP),KU(MPP),KB(MPP),IHYUP(MPP), &
            ISEPQ(MRES), BEGDEL(MRES,11),ENDDEL(MRES,11), &
            IUR(MLAND),NFDT,TargOpt(MRES),TargRelQIN(MRES), &
            ReserID(MRES),GWQX(MLAND),SeepOpt(MLAND),NLndQX(MLAND), &
            QXid(MLAND,NIN),NILNQ(MLAND,NIN),MunProv(MLAND,MUNPRV), &
            NonComm(MLAND,MUNPRV),MunQXin(MLAND,MUNPRV), &
            MunQXdn(MLAND,MUNPRV),MunQXup(MLAND,MUNPRV),MResID(MLAND,MUNPRV), &
            NMunSS(MLAND),MSSWresID(MLAND,MUNPRV),TargRelOpt(MRES), &
            SeepQXOpt(MRES),TargetStorOpt(MRES),TargetStorQIN(MRES)
      INTEGER (KIND=4) :: IBASIN(NFLO),NUMBASIN,IBASOUT(NFLO),NUMBASOUT
      REAL (KIND=8) :: GWMiningQ(MYEAR,13)
      INTEGER (KIND=4) :: UniqAgDivQX(MLAND,40),UniqMIDiv(MLAND,40)
      INTEGER (KIND=4) :: NumAGQX(MLAND),NumMIQX(MLAND)
      INTEGER (KIND=4) :: NQXIN(MRES),IRESQX(MRES,NIN),TargetRelQIN(MRES), &
            TargetRelOpt(MRES),MunGWQx(MLAND),NssPTs(MLAND,MUNPRV), &
            LRESROW(MLAND,MRES),EvapOpt(MRES)
      INTEGER (KIND=4) :: EvapQIN(MRES),NSEEP(MRES),NRIP,NMUN,NCOMM(MLAND), &
            NumNonComm(MLAND),NQXMun(MLAND),MunQXid(MLAND,NIN), &
            NQXup(MLAND,NIN),MQXup(MLAND,NIN,NIN),QXmnID(MMIN), &
            NumMinQXup(MMIN),NmonQXPlt,NannQXplt,CalType,CalibArea,TranNum, &
            TranQX(NFLO)
      DATA IPAGE,NQS  /0,MQX/
      REAL (KIND=8) :: StudyDeplete(MYEAR,13),AgDeplete(MLAND,MYEAR,13), &
          EvapFac(MRES,13),EvapFactor(MRES),StoDiff(MRES,MYEAR,13), &
          SubInflow(MYEAR,13),SubOutflow(MYEAR,13),SubExport(MYEAR,13), &
          SubImport(MYEAR,13),GWMiningQM(13),YieldWMining(MYEAR,13)
      REAL*8 AvPre(13),AvRainAF(13),AvNatuse(13)
      REAL (KIND=8) :: TermOutEvap(MYEAR,13),TermOutEvapM(13)
      REAL*8 AreaPrecip(MYEAR,13),AreaPrAF(MYEAR,13)
      REAL (KIND=8) :: ResInUse(MUNPRV,MYEAR,13),ResOutUse(MUNPRV,MYEAR,13), &
          CommUse(MUNPRV,MYEAR,13), InstUse(MUNPRV,MYEAR,13), &
          IndUse(MUNPRV,MYEAR,13), &
          Secondary(MUNPRV,MYEAR,13),NonResIn(MUNPRV,MYEAR,13), &
          NonResOut(MUNPRV,MYEAR,13),NonCommUse(MUNPRV,MYEAR,13), &
          NonInst(MUNPRV,MYEAR,13),NonInd(MUNPRV,MYEAR,13), &
          NonSecondary(MUNPRV,MYEAR,13),PondArea(MLAND),PondET(MLAND), &
          RetUse(MLAND),Pumped(MLAND),GWMining(MLAND),TotInflow(13), &
          RetFlow(13)
      REAL (KIND=8) :: MandIDep(MYEAR,13),WetDep(MYEAR,13),BasinIn(MYEAR,13)
      REAL (KIND=8) :: BasinOut(MYEAR,13)
      REAL (KIND=8) :: SSIDep(MLAND,MYEAR,13),SSIDeplete(MYEAR,13),SSIDepleteM(13)
      REAL (KIND=8) :: SSIDepM(MLAND,13)
      REAL (KIND=8) :: SSin(MLAND,MUNPRV,MYEAR),SSout(MLAND,MUNPRV,MYEAR), &
          SSComm(MLAND,MUNPRV,MYEAR), &
          SSInd(MLAND,MUNPRV,MYEAR),SSInst(MLAND,MUNPRV,MYEAR), &
          SSCout(MLAND,MUNPRV,MYEAR), SSCcomm(MLAND,MUNPRV,MYEAR), &
          SSCInst(MLAND,MUNPRV,MYEAR),SSCInd(MLAND,MUNPRV,MYEAR)
      REAL (KIND=8) :: MaxCal,MinCal,InitialCal
      REAL (KIND=8) :: SubCropAcres(MLAND,MYEAR,MCROP), &
          SubCropAv(MCROP), SubCropAc(MLAND,MCROP),SubAcres(MLAND,MYEAR), &
          MonYield(MYEAR,13),SubNatUse(MYEAR,13),RipPro(MLAND)
      REAL (KIND=8) :: MunDepPot(MUNPRV,MYEAR,13),MunDepSec(MUNPRV,MYEAR,13), &
        MunDepPTot(MYEAR,13),MunDepSTot(MYEAR,13),RetProp(10)
      REAL (KIND=8) :: MunGW(MLAND,MYEAR,13),MunSurf(MLAND,MYEAR,13), &
        MunReturn(MLAND,MYEAR,13),MunDep(MLAND,MYEAR,13), &
        MunDemIn(MLAND,MYEAR,13), MunDemOut(MLAND,MYEAR,13), &
        PrivDomestic(MLAND),DivIndoor(MUNPRV,MYEAR,13), &
        DivOutdoor(MUNPRV,MYEAR,13),DepIndoor(MLAND,MYEAR,13), &
        DepOutdoor(MLAND,MYEAR,13),DivGW(MUNPRV,MYEAR,13), &
        DivSurf(MUNPRV,MYEAR,13)
      REAL (KIND=8) :: QXTOT(13),TFLO(13),QXM(MQX,13),QXFL(13)
      INTEGER (KIND=4) :: IQXMIN(MQX),MunQXRet(MLAND),NOUTFL, &
        SSYear(MLAND,MUNPRV,MYEAR),TypeCalib
      DATA (IPQX(I),I=1,MQX) /MQX*0/
      INTEGER (KIND=4) :: ITERA,NumOutflow,MLNRS(MRES),MRESUP(MLAND,MRES)
      INTEGER (KIND=4) :: NumOutEvap,NOUEVFL(NFLO)
!     REAL*8 VARIABLES
      REAL (KIND=8) :: AGPUMP(13)
      REAL (KIND=8) :: SHORT(MLAND,MYEAR,13),CPAC(MLAND,MCRPS), &
            CPACFT(MLAND,MYEAR,13),ELV(MRES,MYEAR,13), &
            LGEFPRE(MLAND,13), CorrArr(MYEAR,13), &
            SAR(MPP,MYEAR,13),HEAD(MPP,MYEAR,13),ENER(MPP,MYEAR,13), &
            WETUSE(13,MYEAR,MLAND),BCS,FNYRS,HPLEN(MPP,5), &
            WETPRE(13,MYEAR,MLAND),PropUse(MLAND), &
            HPDIA(MPP,5),HPRGH(MPP,5),QPXMI(MPP),QPMNI(MPP), &
            HDMNI(MPP),ELTWI(MPP),E1I(MPP),CPRO(MLAND), &
            PRACFT(MLAND,13),TM(MLAND,13),PR(MLAND,13),QDVR(MLAND,13), &
            WTUS(MLAND,13),CPUS(MLAND,13),QDMI(MLAND,13), &
            QDVRA(MLAND,13),OFMN(MRES,13),QSM(MRES,13),QTG(MRES,13), &
            CONUSE(13,MYEAR,MLAND),HLOSS(MPP,5),WSOIL(MLAND), &
            WMOIS(MLAND),WPRO(MLAND,MYEAR),CPMUL(MLAND),QIN(MQSTA,MYEAR,13), &
            WTACFT(MLAND,MYEAR,13),ACRAG(MLAND),WACRE(MLAND), &
            RETI(MLAND),QDMIY(MLAND),TOTSO(13),STOIC(MRES), &
            HYHD(MPP),WSHOR(13),HYFLMN(MRES,13),LGUSE(MLAND,MYEAR,13), &
            ACRAGR(MLAND),LGQRTF(MLAND), &
            LGPOT(MLAND,MYEAR,13),ESEEP(10,MRES),SSEEP(10,MRES), &
            NQTG(MRES),OutDoorFac(MLAND,12),TargStor(MRES,12)
      REAL (KIND=8) :: AvGW(MLAND),SubTranExpo(MYEAR,13),SubTranImp(MYEAR,13)
      INTEGER (KIND=4),PARAMETER :: QINZER=MQSTA*MYEAR*13
      INTEGER (KIND=4),PARAMETER :: WaterYear=0
      INTEGER (KIND=4),PARAMETER :: WaterYearNov=1
      INTEGER (KIND=4),PARAMETER :: CalendarYear=2
      integer (KIND=4),PARAMETER :: SHZER=MLAND*MYEAR*13
      DATA (((SHORT(L,J,K),L=1,MLAND),J=1,MYEAR),K=1,13) /SHZER*0.0/
      DATA (((QIN(L,J,K),L=1,MQSTA),J=1,MYEAR),K=1,13) /QINZER*0.0/
      DATA ((QDMI(L,K),K=1,13),L=1,MLAND)/MIZER*0.0/
!     LOGICAL*1 VARIABLES
      LOGICAL (KIND=4) :: IOFF(MRES),IPST,IPEV,IPEL,IPAS,IPSH,IPSA,IPQIN, &
            IREL(MRES),ISTOR(MRES),PRNRS(MRES),ISTOP, &
            PRNLND(MLAND),SPLOT,LCALEN(MLAND),IPQN(MRES),CALLED(MRES), &
            SEEPTB(MRES),QMNLMT(MRES),PrintStage(MRES), &
            PrintArea(MRES),PrintCapacity(MRES),EVRTRead(MRES,MYEAR), &
            BegSoilMatch,Calibrate,NotNegative(MQSTA),Needed(MYEAR), &
            WriteDatabase,IgnoreRiparian
      LOGICAL (KIND=1) :: DRAWN(MRES),AnnGWRead(MLAND,MYEAR)
      DATA ISTOP/.FALSE./
      REAL (KIND=8) :: QXMN(13,MMIN),CumError
      INTEGER (KIND=4) :: IQXMN(20,MMIN),IDV(MLAND,40),MUNID(MLAND), &
            IBY(MLAND,40),ILQIN(MLAND,40),ILUP(MLAND,40), &
            LRES(MLAND,11),NumGW(MLAND)
      REAL (KIND=8) :: FBYPS(MQX,MYEAR,13),YIELD(13)
      INTEGER (KIND=4) :: ILNQ(MLAND,MRES,20),theOrder(MQX),ISQX(MQX)
      REAL (KIND=8) :: RELEAS(MRES,MYEAR,13)
      REAL (KIND=8) :: RSTORE(MRES,MYEAR,13)
      REAL (KIND=8) :: RSEVAP(MRES,MYEAR,13)
      REAL (KIND=8) :: WCUSE(MLAND,MYEAR,13)
      REAL (KIND=8) :: CPLOVR(MLAND,21,13)
      REAL (KIND=8) :: WULOVR(MLAND,MRIPA,13),CEFF(MLAND,MYEAR)
      INTEGER (KIND=4) :: IGRND(MLAND,12),PrintAllResInt,OutNumber(MQX), &
          OutResSto(MQX),OutResArea(MQX),OutResElev(MQX),OutResEvap(MQX), &
          OutType(MQX)
      CHARACTER :: QXNAM(MQX)*75,TITLE*200,DATE2*20,PRESV(MRES)*32, &
           PLAND(MLAND)*32,PHYPW(MPP)*32,LandAreaCode(MLAND)*50, &
           VAR(14)*4,QFILE(MQSTA)*200,CDESCR(MQSTA)*90, &
           QINAM(MQSTA)*80,STOFIL(MRES)*80,SubArea(MLAND)*50, &
           RELFIL(MRES)*80,EVAFIL(MRES)*80,QXFIL*80, &
           DAM_NUMBER(MRES)*50,DAM_NAME(MRES)*50,LandAreaName(MLAND)*50, &
           RipAreaNum(MLAND)*50,RipAreaName(MLAND)*50, &
           RipHeading(MLAND)*100,MunName(MLAND,MUNPRV)*50, &
           NonName(MLAND,MUNPRV)*50,MDamID(MLAND,MUNPRV)*50, &
           MSSname(MLAND,MUNPRV)*50,BasePath*256,MunGroup(MLAND)*32
      CHARACTER (LEN=80) FileData
      CHARACTER :: Subareas(MLAND)*50,TranDir(NFLO)*10,TranBasin(NFLO)*10, &
        TranName(NFLO)*50,Basin(MLAND)*5,BasinName(MLAND)*50
      INTEGER (KIND=4) :: NSUBS
      CHARACTER :: Landarea(MUNPRV,3)*15,OutPutFile(MQX)*100,CalibDesc*50
      REAL (KIND=8) :: PCRF(MLAND,10),SSTO(MLAND,11),LGSSTO(MLAND,11)
      REAL (KIND=8) :: PRE(MLAND,MYEAR,13),TEM(MLAND,MYEAR,13), &
            CCOEF(MCRPS,13), WTPRE(MLAND,MYEAR,13),WTEM(MLAND,MYEAR,13), &
            EFPRE(MLAND),DAYS(12),GWMI(13),EFMIP(MLAND,12),IEFF(MLAND,MYEAR)
      REAL (KIND=8) :: EffPRE(MLAND,MYEAR,13)
      DATA (DAYS(I),I=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/
      REAL (KIND=8) :: CRID,MOIST(MLAND),SOIL(MLAND),BEGSOL(MLAND)
      REAL (KIND=8) :: CROPS(MCRPS,MYEAR,13),LGEFF(MLAND)
      REAL (KIND=8) :: PERCO(13,MYEAR,MLAND),WPERC(13,MYEAR,MLAND)
      REAL (KIND=8) :: TDIVR(13),RZ(13),RFLO(13),EFCR(13),RSSP(13), &
       CPCU(13),ACUSM(13),CHSM(13),CUDEF(13),CACU(13),RFGW(13), &
       TOTRF(13),DPUM(13),DSUP(13),DRET(13),DDEF(13),WSUP(13), &
       WPRE(13),WCONS(13),WOPN(13),TOUTF(13),CHGW(13),TGAGE(13), &
       TMUN(13),WPCUM(13),WPCHG(13),BEFP(MLAND),DDIV(13), &
       TSUB(13),RZSUP(13),GWLOS(13),SMSPL(13),GWRTUS(13),STDIF(MRES,13) &
       ,RIVGW(13),TOTLS(13),ADIVR(MLAND,13),ASHOR(MLAND,13), &
              AUSE(MLAND,13),ASOIL(MLAND,13),ARETFL(MLAND,13), &
              LGEFPR(13),LGOUT(13),DMUSE(13),DMACU(13),WTDEP(13), &
              CPDEP(13),AnnGW(MLAND,MYEAR),StorTarg(MRES,13)
      REAL (KIND=8) :: BUG(30,13),BUGT(30,13),AverVal(13),AverNum(13)
      REAL (KIND=8) :: AverVal2(13)
      LOGICAL (KIND=4) :: ReadIt(MYEAR,13)
      CHARACTER :: BUGTITL(30)*20,QxFile(MQX)*200
      DATA (QINAM(I),I=1,MQSTA) /MQSTA*' '/
      DATA ((IGRND(I,K),I=1,MLAND),K=1,12)/MLTWL*1/
      REAL (KIND=8) :: AGDEL(MLAND),DMDEL(MLAND),DMPUM(MLAND), &
              DMPUMA(MLAND), &
       AGWAT(MLAND),RETFL(MLAND),SPILL(MLAND),GWRIV(MLAND), &
       RTSOI(MLAND),RTMOI(MLAND),GWQDU(MLAND),QDVA(MLAND), &
       GWMIRT(MLAND),BEFM(MLAND),TEF(MLAND,MYEAR),DEF(MLAND,12), &
       AGDEM(MLAND),DMDEM(MLAND),GWMOI(MLAND),QRTF(MLAND), &
       AGOUT(MLAND),STEM(MLAND,11), &
       SBSOI(MLAND),SBMOI(MLAND),LGDEL(MLAND),LGRET(MLAND), &
       OutDoorReturn(MLAND,MYEAR)
      CHARACTER :: CPCODE(MLAND,MCRPS)*6,CPNAM(MLAND,MCRPS)*40 &
            ,LNTYP(MLAND,MCRPS)*13,TMPFIL(MLAND)*80,CorrWith(MQSTA)*200
      CHARACTER :: OtherBasinIn(NFLO)*10,OtherNameIn(NFLO)*50, &
        OtherBasinOut(NFLO)*10,OtherNameOut(NFLO)*50
      REAL (KIND=8) :: TransBasinIn(NFLO,MYEAR,13),TransBasinOut(NFLO,MYEAR,13)
      REAL (KIND=8) :: RESCAL(MLAND,MRES),MRESCAL(MLAND,MRES)
      LOGICAL*1 ISBY(MLAND,10),ResType(MQX,4)
      LOGICAL*4 PrintAllRes,OptMiz
      LOGICAL*1 ISOL(MLAND),IRSRED(MRES,3)
      INTEGER (KIND=4),PARAMETER :: MREAD=3*MRES
      INTEGER (KIND=4) :: NumMainStem,NumTributary,MResNum(MLAND,MRES), &
        NumImport, NumExport, NumUngauged, TypeYear, LinesPerPage, &
        PrintCapacityInt,PrintStageInt,Qtype(MQX),BasinInNum,BasinOutNum
      LOGICAL (KIND=2) :: Correlated(MQSTA),isLogLog(MQSTA), &
        FileExists(MQSTA),AtHome
      REAL (KIND=8) :: CorrCoef(MQSTA,13,2)
      REAL (KIND=8) :: TotalAcres(MLAND),Alfalfa(MLAND),Pasture(MLAND), &
        Hay(MLAND),Grain(MLAND),Corn(MLAND),Orchard(MLAND),Sorghum(MLAND), &
        Turf(MLAND),Onions(MLAND),OtherHort(MLAND),Potatoes(MLAND), &
        Berries(MLAND),OtherVeg(MLAND),Tomatoes(MLAND),Beans(MLAND), &
        Vineyard(MLAND),SubHay(MLAND),SubPast(MLAND)
      REAL (KIND=8) :: SM(MLAND,5)
      DATA ((IRSRED(I,J),I=1,MRES),J=1,3)/MREAD*.FALSE./
      integer (KIND=2) :: intIsLogLog(MQSTA),intCorrelated(MQSTA)
      !equivalence (intIsLogLog), (isLogLog)
      !equivalence (intCorrelated), (Correlated)
END MODULE PrintStuff

!-----------------------------------------------------------------------

MODULE F90SQLVARIABLES
use f90SQLConstants
use f90SQL
integer(SQLINTEGER_KIND),parameter:: MaxStringLen=255
integer(SQLHSTMT_KIND):: StmtHndl
integer(KIND=4),PARAMETER:: MAXRECORDS=20000
integer(KIND=4):: IDFSQL(MAXRECORDS)
END MODULE

!     Last change: SOU 4/9/2015 10:03:10 AM
      PROGRAM BUDGET
!NAME AAMAIN
!-----------------------------------------------------------------------
!
!     GENERALIZED RESERVOIR OPERATION SIMULATION
!
!
!  NOTE:
!     THIS PROGRAM MUST BE LINKED TO USERSUBS ON SYSPAK
!
!     PARAMETER FILE PARAMDIM MUST BE PLACED BEFORE ANY OTHER INCLUDE
!     FILES
!
!-----------------------------------------------------------------------
      use PARAMDIM
      use COMMO
      use PrintStuff
      use F90SQLConstants
      use F90SQL
      use F90SQLVARIABLES
      use FSQL_DATA
      use SubVars
      IMPLICIT NONE
      COMMON /IARQI/IARQ(MQX)
      INTEGER*4 IARQ,ILEN
      COMMON /FLOW/ Y
      INTEGER*4 NQXMN,INEW,IQTGNM,IDEG,IMIN,IY
      INTEGER*4 ISEC,IM1,ID1,IM2,ID2,IB,IYR,IEXP,IXP
      INTEGER*4 IFQ(15),IA,IXQX,IQXI,IQXNUM,IENYR,IFQX
      INTEGER*4 IQXPLT(23),IRSPLT(MRES),IEVPLT(MRES),ISAPLT(MLAND)
      INTEGER*4 IQX(13),INQX,INLND,INDIV,KK
      INTEGER*4 IX,M,N,MM,IP,IND,IN,II,KS,KE,NPTS
      INTEGER*4 NMONTH,IFIL,IL,ISUM,IDAYS(12)
      DATA IDAYS/31,30,31,31,28,31,30,31,30,31,31,30/
      CHARACTER*48 TITL,COMMND*128,GETPAT*80,JUSTEM*8,PRNTIT*10
      CHARACTER*8 QXLS,DFILE*80,OFILE*80,ANS*80,STAGFIL*80
      CHARACTER*80 PREFIL(MLAND),MONTH(3)*4,HEADC*80,OUTVAL*10(13)
      CHARACTER*80 TEMFIL(MLAND),PLOTFL,PLOTFL2,OUTPAT,NONULL
      CHARACTER REC*200
      LOGICAL*2 LAVPRE(MLAND),LAVTEM(MLAND),IANPLT,IMNPLT,IDISK
      LOGICAL*2 ISDIV,IBUD,ISREAD(MYEAR,13),IFIRST
      integer*2 IANPLTINT,IMNPLTINT
      EQUIVALENCE (IANPLT), (IANPLTINT)
      EQUIVALENCE (IMNPLT), (IMNPLTINT)
      REAL*8 WETLNDPR(MLAND,13),WETLNDTM(MLAND,13),WETLNDET(MLAND,13)
      REAL*8 PER(13),ROUND,TOTPRE,TOTEM,TOT,TULES(12),XMIN
      REAL*8 MUNDEM(MLAND),QI(13),TLAT,QMR,QPMX,QPMN
      REAL*8 HDMN,SUM,HGL,H1,H2,HA,HP1,CHP1,HMN,AH12
      REAL*8 PRDEG(MLAND),PRMIN(MLAND),PRSEC(MLAND)
      REAL*8 MON28(MLAND),DAY28(MLAND),MON32(MLAND),DAY32(MLAND)
      REAL*8 BMN28(MLAND),BDY28(MLAND),FMONTH,FINYR,FYEAR
      REAL*8 M1IRR(MLAND),VALU,DAT(13),AVRESET(MRES,13)
      REAL*8 MBEF,MAFT,RV,RA,RE,XMAX,SubIrrPro,SubIrrET
      REAL*8 CN(13),PRC(13),CON(13),BEG50,FIRDAT,BEG60,BEG55
!      REAL*8 BEG45,END45,END50,BEG28,FROST28,FROST,FROST32,RM
      REAL*8 USE,BEGSP,ENDSEA,BGSM,GBEF,GAFT,LGBEF,LGAFT
      REAL*8 CORNC(11),STBEG(MRES),QINM(13),LPRC(13)
!     REAL*8 CRNGFC(21),DRYPFC(21)
      REAL*8 WBEG(MLAND),ORCHWO(12)
      REAL*8 ICHK(MQX),XII(Nf90sql,MDPFIELD),TotPout,TotPret
      REAL*8 GRPFC(21),CRNSFC(21)
      REAL*8 SVGFC(21),SNPBFC(21),TOMFC(21)
      INTEGER*4 IRSCK(MQX)
!     REAL*8 GRNPFC(21),BEETC(11),SALC60(12),SALC36(12)
!     REAL*8 ORCHW(12)
!      REAL*8 SALC12 (12)
!      REAL*8 PASC(12),POTC(11)
!      REAL*8 OWATER (12)
!      REAL*8 COTTC (12)
!      REAL*8 CEDAR (12)
!      REAL*8 GRNC(MGRN)
!      REAL*8 ALFC(12),FLODC(12)
      REAL*8 QMIP(MLAND,13)
      REAL*8 QPMXI(MPP)
!      HYDROPOWER CONSTANT MEGAWATT-HOURS = CP1I * AC-FT/MONTH * HEAD
      REAL*8  CP1I/.0010244/
      REAL*8 H1I(MPP),H2I(MPP),QHPI(MPP)
!        DIMENSIONS FOR FLOWDUR TABLES
      REAL*8 Y(MPTS)
      REAL*8 QQ(23)
      REAL*8 DAYLI(8,12)
      CHARACTER(LEN=10):: DATE9,TIME9
integer(SQLHENV_KIND):: EnvHndl
integer(SQLHDBC_KIND):: ConnHndl
integer(SQLRETURN_KIND)::iRet
character (len=5) JUSTEXT
REAL (KIND=8) :: GWdiv(13),SurDiv(13)
!---------------------------------------------------------------------------
!  ModelBudget parameters
integer (KIND=4) :: ModelID,EndYear,Iterations
character :: ModelName*200,ProgName*50,ExeName*50,PrintName*50
character :: FileName*100,LastUser*50,CreatedBy*50
integer (KIND=4) :: intTemp
character (len=255) :: ALLTRIM
logical (KIND=2) :: PrintResData,PrintInflows
logical (KIND=2) :: PrintAllArea,PrintAllCapacity,PrintAllInflows
logical (KIND=2) :: PrintMinFlows,PrintLandArea,PrintInOut
logical (KIND=2) :: PrintAnnYield,PrintAllStage
logical (KIND=2) :: PrintPerYield,CalcPrivDomestic,IsOpen
integer (KIND=4) :: BegSoilMatchInt
integer (KIND=2) :: PrintResDataInt
integer (KIND=2) :: PrintAreaInt,PrintInflowsInt
integer (KIND=2) :: PrintMinFlowsInt,PrintLandAreaInt,PrintInOutInt
integer (KIND=2) :: PrintAnnYieldInt
integer (KIND=2) :: PrintPerYieldInt,CalcPrivDomesticInt,IsOpenInt
equivalence (PrintResData), (PrintResDataInt)
equivalence (PrintAllArea), (PrintAreaInt)
equivalence (PrintInflows), (PrintInflowsInt)
equivalence (PrintMinFlows), (PrintMinFlowsInt)
equivalence (PrintLandArea), (PrintLandAreaInt)
equivalence (PrintInOut), (PrintInOutInt)
equivalence (PrintAnnYield), (PrintAnnYieldInt)
equivalence (PrintPerYield), (PrintPerYieldInt)
equivalence (CalcPrivDomestic), (CalcPrivDomesticInt)
equivalence (IsOpen), (IsOpenInt)
type (TIMESTAMP_STRUCT) :: LastOpened,LastClosed
character(len=MaxStringLen):: SQLStmtStr,ConnStrOut
character(len=MaxStringLen):: tempStr,ModelWhere
logical (kind=4) :: StrComp
!---------------------------------------------------------------------------
integer(SQLSMALLINT_KIND)::ColNumber,ConnStrLength,NumEvap
integer(Kind=2)::IgnoreRipInt
integer(kind=4)::ioerror
integer (kind=4) :: LandID(MLAND),IQ,ITEMP,locComma(5),posComma,charLen
character(len=MaxStringLen)::ModelFilter
character(len=MaxStringLen)::ResFilter, TempStr2,TempStr3
character(len=MaxStringLen)::LandFilter
character(len=MaxStringLen)::PrintFile
character(len=255),EXTERNAL :: LeftAdj
character(len=20) :: UsedAreaID(MLAND),RipAreaUsed(MLAND),PourKind
logical (kind=4) :: HasReservoir,UseMunicipal,FoundIt,DebugIt,LowAcres
character(len=256)::ModelFile,MandIFile
!      CHARACTER (LEN=20),DIMENSION(18) :: AcreTitles=(/ "Alfalfa  ", &
!        "Pasture  ","Hay      ","Grain    ","Corn     ","Orchard  ", &
!        "Sorghum  ","Turf     ","Onions   ","OtherHort","Potatoes ", &
!        "Berries  ","OtherVeg ","Tomatoes ","Beans    ","Vineyard ", &
!        "SubPast  ","SubHay   "/)
!      CHARACTER (LEN=20) :: CropTitles(20)
!      INTEGER (KIND=4) :: iCropMap(20),nCrops,iFirst,iLast
real (kind=8) :: AvEvap(12),Proportion,TempArr(MYEAR,15),TempVar(MYEAR,15)
INTEGER (KIND=4) :: TempInt(MYEAR)
logical (kind=1) :: IsGW(MLAND,MYEAR),PrintEffLine
REAL (KIND=8) :: ThisTemp(13)
REAL (KIND=8) :: TotInd
REAL (KIND=8) :: Simul
External Simul
CHARACTER (LEN=255) :: NoComma
External NoComma
real (kind=8) :: TempNon(MYEAR,11,13),RetIndoor(MUNPRV,MYEAR,13)
REAL (KIND=8) :: EffRainFall(13)
real (kind=8) :: RetOutdoor(MUNPRV,MYEAR,13),Total,theZero
real (kind=8) :: TotFlow,Diff,GWprop,TOL,PotET(13)
real (kind=8) :: TotIn,TotOut,TotOutInd,pondacres,et,RetIn,RetOut
real (kind=8) :: ResReturn,ComReturn,InstReturn,IndReturn,SewageReturn, &
      SepticReturn
REAL (KIND=8) :: AlfWet,PastWet,WetET,CalibVal,Retn
TYPE (TIMESTAMP_STRUCT):: rptDate,deleteDate
INTEGER (KIND=4) :: NumRead,Ind1,Ind2,TreatmentCode,Ires,JJ,JJJ
INTEGER (KIND=4) :: RptYear,RptMonth,RptDay
CHARACTER :: strRptYear*4,strRptMonth*2,strRptDay*2,strModelID*5,strRptDate*12
CHARACTER (len=MaxStringLen) :: ModelCmd
CHARACTER (len=20) :: VarTitles(21)
data (VarTitles(i),i=1,15)/ &
  "Years Simulated",    "Initial Year",       "Number Reservoirs",  &
  "Number Ag. Areas",   "Number Inflows",     "Number QXs"        , &
  "Print Ag. Shortage", "Print EOM Storage",  "Print EOM Res. Area",&
  "Print EOM Evap.",    "Print EOM Res. Ele.","Write QINs",         &
  "Num. Min. Flows",    "Max Num. Iterations","Year Type"           /
INTEGER (KIND=4) :: SecYear(MYEAR),NumSecData,PrevYear
REAL (KIND=8) :: SecRes(MYEAR),SecCom(MYEAR),SecInst(MYEAR),SecInd(MYEAR)
AtHome=.FALSE.
WriteDatabase=.FALSE.
! This will print some information about the municipal systems
! being modeled.
DebugIt=.FALSE.
UseMunicipal=.TRUE.
IBUD=.TRUE.
IDISK=.FALSE.
IMNPLT=.FALSE.
IANPLT=.FALSE.
IPSH=.TRUE.
PLOTFL=""
PLOTFL2=""
IF (AtHome) THEN
  BasePath="c:\work\STREAMFL\MONTH\"
else
  BasePath="O:\STREAMFL\MONTH\"
END IF
!
!     READ INPUT DATA
!
!   PAGE HEADING
!*******************************************************************
!   Debug print titles follow
!   In this case, values are loaded in ARSUB
!   Results are printed in Simula in BUDSUB
!*******************************************************************
     BUGTITL(1)='OCT'
     BUGTITL(2)='NOV'
     BUGTITL(3)='DEC'
     BUGTITL(4)='JAN'
     BUGTITL(5)='FEB'
     BUGTITL(6)='MAR'
     BUGTITL(7)='APR'
     BUGTITL(8)='MAY'
     BUGTITL(9)='JUN'
     BUGTITL(10)='JUL'
     BUGTITL(11)='AUG'
     BUGTITL(12)='SEP'
     BUGTITL(13)='ANN'
!     BUGTITL(14)='SHORTAGE x EFF'
!     BUGTITL(15)='POS ERROR'
!     BUGTITL(16)='ROUTED RETFL'
!*******************************************************************
!   End of Debug statements
!*******************************************************************
if (AtHome) then
  ModelFile='C:\WORK\WATBUDG\Models\ModelData.mdb'
  MandIFile='C:\WORK\M&I\M&I.mdb'
else
  ModelFile='O:\DATABASE\WATBUDG\Models\ModelData.mdb'
  MandIFile='O:\DATABASE\M&I\M&I.mdb'
end if
fname=ModelFile
ModelID=0
CALL GETCL(COMMND)
WriteDatabase=.FALSE.
IF(trim(AllTrim(COMMND)).GT.' ')THEN
  IF (INDEX(COMMND,",")>0) THEN
    READ(COMMND,*,END=10)ModelID,WriteDatabase
  ELSE
    READ(COMMND,*,END=10)ModelID
  END IF
ELSE
  DO WHILE (ModelID == 0)
      WRITE(*,*)'TYPE Model ID for simulation'
      WRITE(*,*)' '
      READ(*,"(A)")COMMND
      IF (INDEX(COMMND,",")>0) THEN
        READ(COMMND,*,END=10)ModelID,WriteDatabase
      ELSE
        READ(COMMND,*,END=10)ModelID
      ENDIF
  END DO
ENDIF
 10   CONTINUE
call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)
!Set ODBC version we will be using (3.x in this case)
call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, &
                      SQL_OV_ODBC3, iRet)

!Allocate a connection handle
call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl, iRet)

!ConnStr='DBQ='//trim(fname)//';DRIVER={Microsoft Access Driver (*.mdb)}'
ConnStr='Driver={SQL Server};Server=172.23.166.139\SQLEXPRESS;' // &
  'Database=waterdata;Uid=cmiller;Pwd=waterdb;'

call f90SQLDriverConnect(ConnHndl, f90SQL_NULL_PTR, ConnStr, &
    ConnStrOut, ConnStrLength, SQL_DRIVER_COMPLETE,iRet)

if (iRet.eq.SQL_SUCCESS .or. iRet.eq. SQL_SUCCESS_WITH_INFO) then

    !Allocate statement handdle
    call f90SQLAllocHandle(SQL_HANDLE_STMT,ConnHndl, StmtHndl, iRet)

    !Create a parameterized SQL query
    WRITE(tempStr,"(I6)")ModelID
    SQLStmtStr="SELECT * FROM ModelBudget "// &
               "WHERE ModelID = "// Trim(ALLTRIM(TempStr)) // ";"

    call f90SQLPrepare(StmtHndl,trim(SQLStmtStr),iRet)

    print *,'Query prepared',iRet

    !Prepare the SQL query and execute the query (
    call f90SQLExecute(StmtHndl,iRet) 
    PrintResDataInt=0
    PrintAllResInt=0
    if (iRet.eq.SQL_SUCCESS .or. iRet.eq.SQL_SUCCESS_WITH_INFO) then

        !Retrieve data
         KOUNTFSQL=0
        !bind SQL statement parameters to fortran variables
        call f90SQLBindCol (StmtHndl, int(1,SQLUSMALLINT_KIND), SQL_INTEGER, &
            ModelID, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(2,SQLUSMALLINT_KIND), SQL_CHAR,  &
            ModelName, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(3,SQLUSMALLINT_KIND), SQL_CHAR, &
            TITLE, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(4,SQLUSMALLINT_KIND), SQL_INTEGER, &
            INYR, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(5,SQLUSMALLINT_KIND), SQL_INTEGER, &
            EndYear, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(6,SQLUSMALLINT_KIND), SQL_INTEGER, &
            BegSoilMatchInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(7,SQLUSMALLINT_KIND), SQL_INTEGER, &
            Iterations, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(8,SQLUSMALLINT_KIND), SQL_CHAR, &
            ProgName, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(9,SQLUSMALLINT_KIND), SQL_CHAR, &
            ExeName, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(10,SQLUSMALLINT_KIND), SQL_CHAR, &
            PrintName, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(11,SQLUSMALLINT_KIND), SQL_INTEGER, &
            TypeYear, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(12,SQLUSMALLINT_KIND), &
            SQL_SMALLINT, PrintResDataInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(13,SQLUSMALLINT_KIND), &
            SQL_SMALLINT, PrintAllResInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(14,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintStageInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(15,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintAreaInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(16,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintCapacityInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(17,SQLUSMALLINT_KIND), SQL_INTEGER, &
            LinesPerPage, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(18,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintInflowsInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(19,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintMinFlowsInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(20,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintLandAreaInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(21,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintInOutInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(22,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintAnnYieldInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(23,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            PrintPerYieldInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(24,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            CalcPrivDomesticInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(25,SQLUSMALLINT_KIND), SQL_SMALLINT, &
            IsOpenInt, f90SQL_NULL_PTR, iRet)
        call f90SQLBindCol (StmtHndl, int(26,SQLUSMALLINT_KIND), SQL_CHAR, &
            FileName, f90SQL_NULL_PTR, iRet)
!        call f90SQLBindCol (StmtHndl, int(27,SQLUSMALLINT_KIND), SQL_CHAR, &
!            LastUser, f90SQL_NULL_PTR, iRet)
!        call f90SQLBindCol (StmtHndl, int(28,SQLUSMALLINT_KIND), SQL_CHAR, &
!            CreatedBy, f90SQL_NULL_PTR, iRet)
!        call f90SQLBindCol (StmtHndl, int(27,SQLUSMALLINT_KIND), &
!            SQL_TYPE_TIMESTAMP, LastOpened, f90SQL_NULL_PTR, iRet)
!        call f90SQLBindCol (StmtHndl, int(28,SQLUSMALLINT_KIND), &
!            SQL_TYPE_TIMESTAMP, LastClosed, f90SQL_NULL_PTR, iRet)
!        call f90SQLBindCol (StmtHndl, int(29,SQLUSMALLINT_KIND), SQL_SMALLINT, &
!            IANPLTINT, f90SQL_NULL_PTR, iRet)
!        call f90SQLBindCol (StmtHndl, int(30,SQLUSMALLINT_KIND), SQL_SMALLINT, &
!            IMNPLTINT, f90SQL_NULL_PTR, iRet)
         do while (.true.)
            call f90SQLFetch(StmtHndl,iRet)
            if (iRet.ne.SQL_SUCCESS .and. iRet.ne.SQL_SUCCESS_WITH_INFO) then
                if (iRet.eq.SQL_NO_DATA) then
                    print *,'End of data set reached'
                else
                    print *,'Error fetching data' 
!                   call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
                endif
                exit
            endif
            KOUNTFSQL=KOUNTFSQL+1
        enddo
    else
        print *,'Error executing SQL query'
!       call ShowDiags(SQL_HANDLE_STMT,StmtHndl)
    endif
endif
!release connection handle
call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
!disconnect
call f90SQLDisconnect(ConnHndl,iRet)
!release environment handle 
call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet) 
if (PrintResDataInt<>0) then
  PrintResData=.TRUE.
end if
if (PrintAllResInt<>0) then
  PrintAllRes=.TRUE.
end if
IF (BegSoilMatchInt<>0) THEN
  BegSoilMatch=.TRUE.
ELSE
  BegSoilMatch=.FALSE.
END IF
IF (PrintLandAreaInt.NE.0) THEN
  IPSH=.TRUE.
ELSE
  IPSH=.FALSE.
END IF
NYRS = EndYear - INYR + 1
IENYR = EndYear
if (LEN_TRIM(PrintName)>0) then
  if (.not. index(PrintName,"\")>0) then
    IF (AtHome) THEN
      printfile="c:\work\watbudg\models\printout\" // trim(PrintName)
    else
      printfile="O:\database\watbudg\models\printout\" // trim(PrintName)
    END IF
  else
    printfile=PrintName
  end if
  if (.not. index(printfile,":")>0) then
    if (AtHome) then
      printfile="C:" // printfile
    else
      printfile="O:" // printfile
    end if
  end if
  DO I=1,len(Trim(AllTrim(printfile)))
    if (lge(printfile(I:I),'A') .AND. LLE(printfile(I:I),'Z')) then
      printfile(I:I)=achar(iachar(printfile(I:I)) + 32)
    END IF
  END DO
  if (.not. index(printfile,".out")>0) then
    printfile = trim(AllTrim(printfile)) // ".out"
  end if
end if
OPEN (UNIT=6,FILE=printfile,STATUS='REPLACE',ACTION='WRITE', &
  CARRIAGECONTROL='FORTRAN',IOSTAT=ioerror,BLOCKSIZE=10000)
ITERMX=Iterations
!----------------------------------------------------------------------------
!Read in ModelBudget table to read in a few more variables
!Open the database and table
DefaultDir=GETPAT(FileName)
QXNAM=""
call initializeaccessvars()
Table="ModelBudget"
NLOGICFIELD=1
LOGICFIELDS(1)="IgnoreRiparian"
WillOpen = .TRUE.
! Leave it open for further queries
WillClose = .FALSE.
WRITE(TempStr,"(I6)")ModelID
indexstring="WHERE ModelID="// trim(ALLTRIM(TempStr))
ModelFilter=indexstring
call AccessRead()
DO I=1,KOUNTFSQL
  IF (F90SQLINTEGER(I,1).EQ.1) THEN
    IgnoreRiparian=.TRUE.
  ELSE
    IgnoreRiparian=.FALSE.
  END IF
END DO
!----------------------------------------------------------------------------
!Read in ModelQXPrint table
!These can be in a separate database
!Open the database and table
call initializeaccessvars()
!Specify table to open
Table="ModelQXPrint"
!String Fields
NSTRINGFIELD=1
STRINGFIELDS(1)="QXDesc"
!Double Precision Fields
NLOGICFIELD=2
LOGICFIELDS(1)="QXnum"
LOGICFIELDS(2)="theOrder"
! Open the connection
!WillClose = .TRUE.
! Create a filter for this model only
!WRITE(TempStr,"(I6)")ModelID
!indexstring="WHERE ModelID="// trim(ALLTRIM(TempStr))
indexstring=trim(ModelFilter) // " ORDER BY [theOrder]"
call AccessRead()
NQX=KOUNTFSQL
do i=1,KOUNTFSQL
   IPQX(i)=F90SQLINTEGER(i,1)
   theOrder(i)=F90SQLINTEGER(i,2)
   QxMap(IPQX(i))=i
   QXNAM(i)=F90SQLSTRINGS(i,1)
end do
do i=1,nqx
  QxMap(IPQX(i))=i
end do
SELECT CASE (TypeYear)
    CASE (0)
        VAR(1:14)=(/"YEAR"," OCT"," NOV"," DEC"," JAN"," FEB", &
            " MAR"," APR"," MAY"," JUN"," JUL"," AUG"," SEP"," ANN"/)
    CASE (1)
        VAR(1:14)=(/"YEAR"," NOV"," DEC"," JAN"," FEB"," MAR", &
            " APR"," MAY"," JUN"," JUL"," AUG"," SEP"," OCT"," ANN"/)
    CASE (2)
        VAR(1:14)=(/"YEAR"," JAN"," FEB"," MAR"," APR"," MAY", &
            " JUN"," JUL"," AUG"," SEP"," OCT"," NOV"," DEC"," ANN"/)
END SELECT
!----------------------------------------------------------------------------
!Read in ModelQIN table
!----------------------------------------------------------------------------
WillOpen=.FALSE.
CDESCR=""
F90SQLSTRINGS=""
call initializeaccessvars()
Table="ModelQIN"
indexstring=Trim(AllTrim(ModelFilter)) // " ORDER BY QINnum"
NLOGICFIELD=4
LOGICFIELDS(1)="QX"
!LOGICFIELDS(2)="FileExists"
LOGICFIELDS(2)="Correlated"
LOGICFIELDS(3)="isLogLog"
LOGICFIELDS(4)="NotNegative"
NSTRINGFIELD=3
STRINGFIELDS(1)="QINfile"
STRINGFIELDS(2)="QINdesc"
STRINGFIELDS(3)="CorrWith"
F90SQLSTRINGS=""
call AccessRead()
!Indexstring remains the same
NQIN=KOUNTFSQL
  do i=1,KOUNTFSQL
      QFILE(i)=F90SQLSTRINGS(i,1)
      IQXN(i)=F90SQLINTEGER(i,1)
      IF(IQXN(i).GT.0)ICHK(IQXN(i))=i
      CDESCR(i)=F90SQLSTRINGS(i,2)
      CorrWith(i)=F90SQLSTRINGS(i,3)
      intCorrelated(i)=F90SQLINTEGER(i,2)
      intIsLogLog(i)=F90SQLINTEGER(i,3)
      if (F90SQLINTEGER(i,4).ne.0) then
        NotNegative(i)=.TRUE.
      ELSE
        NotNegative(i)=.FALSE.
      end if
      if (intCorrelated(i)>0) then
        Correlated(i)=.TRUE.
      end if
      if (intIsLogLog(i)>0) then
        isLogLog(i)=.TRUE.
      end if
  end do
if (NQIN>0) then
  call initializeaccessvars()
  WillClose = .TRUE.
  NDPFIELD=26
  FIELD(1)="a1"
  FIELD(2)="a2"
  FIELD(3)="a3"
  FIELD(4)="a4"
  FIELD(5)="a5"
  FIELD(6)="a6"
  FIELD(7)="a7"
  FIELD(8)="a8"
  FIELD(9)="a9"
  FIELD(10)="a10"
  FIELD(11)="a11"
  FIELD(12)="a12"
  FIELD(13)="a13"
  FIELD(14)="b1"
  FIELD(15)="b2"
  FIELD(16)="b3"
  FIELD(17)="b4"
  FIELD(18)="b5"
  FIELD(19)="b6"
  FIELD(20)="b7"
  FIELD(21)="b8"
  FIELD(22)="b9"
  FIELD(23)="b10"
  FIELD(24)="b11"
  FIELD(25)="b12"
  FIELD(26)="b13"
  call AccessRead()
  do i=1,KOUNTFSQL
      do j=1,13
        CorrCoef(i,j,1)=XI(i,j)
        CorrCoef(i,j,2)=XI(i,j+13)
      end do
  end do
  CALL GETQIN()
  WillOpen=.TRUE.
end if

!----------------------------------------------------------------------------
!RESERVOIR DATA - Read in all Reservoir data and related tables
!----------------------------------------------------------------------------
!Read in ModelResData table
!----------------------------------------------------------------------------
! Intialize a few variables
IRSCK=0
WillClose=.FALSE.
ConnStr='Driver={SQL Server};Server=172.23.166.139\SQLEXPRESS;' // &
  'Database=waterdata;Uid=cmiller;Pwd=waterdb;'
call initializeaccessvars()
PRNRS=.FALSE.
IRSRED=.FALSE.
IREL=.FALSE.
TargRelQIN=0
!Read in Options
Table="ModelResData"
NLOGICFIELD=10
logicfields(1)="TargetRelOpt"
logicfields(2)="TargetRelQIN"
logicfields(3)="SeepQXOpt"
logicfields(4)="SeepQX"
logicfields(5)="TargetStorOpt"
logicfields(6)="TargetStorQIN"
logicfields(7)="EvapQIN"
logicfields(8)="LimitMinReltoInflow"
logicfields(9)="TerminusRes"
logicfields(10)="PrintResData"
indexstring=trim(AllTrim(ModelFilter)) // ' ORDER BY RowNumber'
call AccessRead()
WillOpen=.FALSE.
NRES=KOUNTFSQL
do i=1,NRES
  TargetRelOpt(i)=F90SQLINTEGER(i,1)
  IF (TargetRelOpt(i).EQ.2) THEN
    IREL(i)=.TRUE.
  ELSE
    IREL(i)=.FALSE.
  END IF
  TargetRelQIN(i)=F90SQLINTEGER(i,2)
  SeepQXOpt(i)=F90SQLINTEGER(i,3)
  ISEPQ(i)=F90SQLINTEGER(i,4)
  TargetStorOpt(i)=F90SQLINTEGER(i,5)
  IF (TargetStorOpt(i).EQ.2) THEN
    ISTOR(i)=.TRUE.
  ELSE
    ISTOR(i)=.FALSE.
  END IF
  TargetStorQIN(i)=F90SQLINTEGER(i,6)
  EvapQIN(i)=F90SQLINTEGER(i,7)
  if (F90SQLINTEGER(i,8)>0) then
    QMNLMT(i)=.TRUE.
  end if
  IREST(i)=F90SQLINTEGER(i,9)
  if (F90SQLINTEGER(i,10)>0) then
    PRNRS(i)=.TRUE.
  end if
end do
IF (NRES>0) THEN
  call initializeaccessvars()
  NLOGICFIELD=5
  logicfields(1)="PrintStage"
  logicfields(2)="PrintArea"
  logicfields(3)="PrintCapacity"
  logicfields(4)="ID"
  logicfields(5)="EvapOpt"
  NSTRINGFIELD=3
  STRINGFIELDS(1)="DAM_NUMBER"
  STRINGFIELDS(2)="DAM_NAME"
  STRINGFIELDS(3)="PrintedName"
  NDPFIELD=1
  Field(1)="EvapFactor"
  call AccessRead()
  do i=1,NRES
    if (F90SQLINTEGER(i,1)>0) then
      PrintStage(i)=.TRUE.
    end if
    if (F90SQLINTEGER(i,2)>0) then
      PrintArea(i)=.TRUE.
    end if
    if (F90SQLINTEGER(i,3)>0) then
      PrintCapacity(i)=.TRUE.
    end if
    ReserID(i)=F90SQLINTEGER(i,4)
    DAM_NUMBER(i)=F90SQLSTRINGS(i,1)
    DAM_NAME(i)=F90SQLSTRINGS(i,2)
    PRESV(i)=F90SQLSTRINGS(i,3)
    EvapOpt(i)=F90SQLINTEGER(i,5)
    EvapFactor(i)=XI(i,1)
    IF (TargRelOpt(i)==2) Then
        IREL(i)=.TRUE.
    END IF
    IPQN(i)=.FALSE.
    SEEPTB(i)=.FALSE.
    SeepQXOpt(i)=F90SQLINTEGER(i,3)
    select case (SeepQXOpt(i))
      CASE (2)
          IPQN(i)=.TRUE.
      case (1)
          SEEPTB(i)=.TRUE.
    end select
    ISTOR(i)=.FALSE.
    IF (TargetStorOpt(i)==2) THEN
        ISTOR(i)=.TRUE.
    END IF
    QMNLMT(i)=F90SQLLOGIC(i,8)
    PRNRS(i)=F90SQLLOGIC(i,10)
    PrintStage(i)=F90SQLLOGIC(i,11)
    PrintArea(i)=F90SQLLOGIC(i,12)
    PrintCapacity(i)=F90SQLLOGIC(i,13)
    DAM_NUMBER(i)=trim(ALLTRIM(F90SQLSTRINGS(i,1)))
    DAM_NAME(i)=trim(ALLTRIM(F90SQLSTRINGS(i,2)))
    PRESV(i)=trim(ALLTRIM(F90SQLSTRINGS(i,3)))
  end do
  call initializeaccessvars()
  !----------------------------------------------------------------------------
  !Read in a reservoir capacity and target release (minimum downstream flow)
  call initializeaccessvars()
  NDPFIELD=15
  Field(1)="MaxCap"
  Field(2)="MinCap"
  Field(3)="InitCap"
  Field(4)="TargRel1"
  Field(5)="TargRel2"
  Field(6)="TargRel3"
  Field(7)="TargRel4"
  Field(8)="TargRel5"
  Field(9)="TargRel6"
  Field(10)="TargRel7"
  Field(11)="TargRel8"
  Field(12)="TargRel9"
  Field(13)="TargRel10"
  Field(14)="TargRel11"
  Field(15)="TargRel12"
  call AccessRead()
  do i=1,KOUNTFSQL
      SMX(i)=XI(i,1)
      SMN(i)=XI(i,2)
      STOIC(i)=XI(i,3)
      QRMN(i,13)=0.0
      do k=1,12
          QRMN(i,k)=XI(i,3+k)
          QRMN(i,13)=QRMN(i,13)+XI(i,3+k)
      end do
  end do
    !----------------------------------------------------------------------------
    !Read in a monthly seepage
  DO I=1,NRES
    IF (SeepQXOpt(i)==2) THEN
      call initializeaccessvars()
      NDPFIELD=12
      Field(1)="MonSeep1"
      Field(2)="MonSeep2"
      Field(3)="MonSeep3"
      Field(4)="MonSeep4"
      Field(5)="MonSeep5"
      Field(6)="MonSeep6"
      Field(7)="MonSeep7"
      Field(8)="MonSeep8"
      Field(9)="MonSeep9"
      Field(10)="MonSeep10"
      Field(11)="MonSeep11"
      Field(12)="MonSeep12"
      WRITE(TempStr,"(I6)")ReserID(I)
      ResFilter=trim(ModelFilter) // " AND ID=" // trim(AllTrim(TempStr))
      indexstring=trim(ResFilter)
      call AccessRead()
      do II=1,KOUNTFSQL
          QSM(i,13)=0.0
          do k=1,12
            IF (TypeYear==0) THEN
              IF (K.GE.10) THEN
                KK=K-9
              ELSE
                KK=K+3
              END IF
            ELSE IF(TypeYear==1) THEN
              IF (K.GE.11) THEN
                KK=K-10
              ELSE
                KK=K+2
              END IF
            END IF
            QSM(i,kk)=XI(ii,k)
            QSM(i,13)=QSM(i,13)+XI(ii,k)
          end do
      end do
    end if
  END DO
    !----------------------------------------------------------------------------
    !Read in a monthly evaporation factors
    !----------------------------------------------------------------------------
  DO I=1,NRES
    IF (EvapOpt(i)==1) THEN
      call initializeaccessvars()
      NDPFIELD=12
      Field(1)="ResEvap1"
      Field(2)="ResEvap2"
      Field(3)="ResEvap3"
      Field(4)="ResEvap4"
      Field(5)="ResEvap5"
      Field(6)="ResEvap6"
      Field(7)="ResEvap7"
      Field(8)="ResEvap8"
      Field(9)="ResEvap9"
      Field(10)="ResEvap10"
      Field(11)="ResEvap11"
      Field(12)="ResEvap12"
      WRITE(TempStr,"(I6)")ReserID(I)
      ResFilter=trim(ModelFilter) // " AND ID=" // trim(AllTrim(TempStr))
      indexstring=trim(ResFilter)
      call AccessRead()
      DO II=1,KOUNTFSQL
        EvapFac(i,13)=0.0
        DO K=1,12
          IF (TypeYear==0) THEN
            IF (K.GE.10) THEN
              KK=K-9
            ELSE
              KK=K+3
            END IF
          ELSE IF(TypeYear==1) THEN
            IF (K.GE.11) THEN
              KK=K-10
            ELSE
              KK=K+2
            END IF
          END IF
          EvapFac(i,KK)=XI(ii,K)
          EvapFac(i,13)=EvapFac(i,13)+XI(ii,K)
        END DO
      END DO
    END IF
  END DO
  !----------------------------------------------------------------------------
  !Read in a target monthly storage
  !----------------------------------------------------------------------------
  DO I=1,NRES
    IF (TargetStorOpt(i).EQ.1) THEN
        call initializeaccessvars()
        NDPFIELD=12
        Field(1)="TargStor1"
        Field(2)="TargStor2"
        Field(3)="TargStor3"
        Field(4)="TargStor4"
        Field(5)="TargStor5"
        Field(6)="TargStor6"
        Field(7)="TargStor7"
        Field(8)="TargStor8"
        Field(9)="TargStor9"
        Field(10)="TargStor10"
        Field(11)="TargStor11"
        Field(12)="TargStor12"
        WRITE(TempStr,"(I6)")ReserID(I)
        ResFilter=trim(ModelFilter) // " AND ID=" // trim(AllTrim(TempStr))
        indexstring=trim(ResFilter)
        call AccessRead()
        do ii=1,KOUNTFSQL
            StorTarg(i,13)=0.0
            do k=1,12
              IF (TypeYear==0) THEN
                IF (K.GE.10) THEN
                  KK=K-9
                ELSE
                  KK=K+3
                END IF
              ELSE IF(TypeYear==1) THEN
                IF (K.GE.11) THEN
                  KK=K-10
                ELSE
                  KK=K+2
                END IF
              END IF
              StorTarg(i,kk)=XI(ii,k)
              StorTarg(i,13)=StorTarg(i,13)+XI(ii,k)/12.0
            end do
        end do
    END IF
  END DO
  !----------------------------------------------------------------------------
  !Read in ModelResSAC table
  !----------------------------------------------------------------------------
  !Specify which table to open
  Table="ModelResSAC"
  !Read in tables for each reservoir
  do M=1,NRES
      !Initialize fields and numbers of fields
      call initializeaccessvars()
      !Set the filter for these records
      Write(TempStr,"(I6)")ReserID(M)
      ResFilter=trim(ModelFilter) // " AND ResID=" // trim(AllTrim(TempStr)) &
        // " ORDER BY Stage"
      indexstring=ResFilter
      !Double Precision Fields
      NDPFIELD=3
      Field(1)="Stage"
      Field(2)="Area"
      Field(3)="Capacity"
      ! Create a filter for this model only
      call AccessRead()
      NP(M)=KOUNTFSQL
      do i=1,NP(M)
          E(M,i)=XI(i,1)
          A(M,i)=XI(i,2)
          V(M,i)=XI(i,3)
      end do
      !Sort the array
      do i=1,NP(M)-1
          do j=i+1,NP(M)
              if (E(M,i)>E(M,j)) then
                call swap(E(M,i),E(M,j))
                call swap(A(M,i),A(M,j))
                call swap(V(M,i),V(M,j))
              end if
          end do
      end do
  end do

  !----------------------------------------------------------------------------
  !Read in ModelResQXout table
  !----------------------------------------------------------------------------
  !Specify which table to open
  Table="ModelResQXout"
  !Read in tables for each reservoir
  do M=1,NRES
      !Initialize fields and numbers of fields
      call initializeaccessvars()
      !Set the filter for these records
      Write(TempStr,"(I6)")ReserID(M)
      ResFilter=trim(ModelFilter) // " AND ResID=" // trim(AllTrim(TempStr))
      ! Create a filter for this model and reservoir only
      indexstring=ResFilter
      !Double Precision Fields
      NLOGICFIELD=1
      LOGICFIELDS(1)="QXout"
      call AccessRead()
      NQTG(M)=KOUNTFSQL
      do i=1,KOUNTFSQL
          IQTG(M,i)=F90SQLINTEGER(i,1)
          CALL IOU(IQTG(M,i),2,M) ! Sets ICKOU(QXno) to M for data checking
          ! Also sets ITYOU(QXno) to 2 (means it is outflow from a reservoir)
      end do
  end do

  !----------------------------------------------------------------------------
  !Read in ModelResQXin table
  !----------------------------------------------------------------------------
  !Specify which table to open
  Table="ModelResQXin"
  !Read in tables for each reservoir
  do M=1,NRES
      !Initialize fields and numbers of fields
      call initializeaccessvars()
      !Set the filter for these records
      Write(TempStr,"(I6)")ReserID(M)
      ResFilter=trim(ModelFilter) // " AND ResID=" // trim(AllTrim(TempStr))
      indexstring=ResFilter
      !Double Precision Fields
      NLOGICFIELD=1
      LOGICFIELDS(1)="QXin"
      ! Create a filter for this model only
      call AccessRead()
      NQXIN(M)=KOUNTFSQL
      do i=1,NQXIN(M)
          IRESQX(M,i)=F90SQLINTEGER(i,1)
          IQXAD(M,i)=F90SQLINTEGER(i,1)
      end do
  end do

  !----------------------------------------------------------------------------
  !Read in ModelResSeep table
  !----------------------------------------------------------------------------
  !Specify which table to open
  Table="ModelResSeep"
  !Read in tables for each reservoir
  do M=1,NRES
      !Initialize fields and numbers of fields
      call initializeaccessvars()
      !Set the filter for these records
      Write(TempStr,"(I6)")ReserID(M)
      ResFilter=trim(ModelFilter) // " AND ResID=" // trim(AllTrim(TempStr))
      indexstring=ResFilter
      !Double Precision Fields
      NDPFIELD=2
      Field(1)="Stage"
      Field(2)="Seepage"
      ! Create a filter for this model only
      call AccessRead()
      NSEEP(M)=KOUNTFSQL
      do i=1,KOUNTFSQL
          ESEEP(M,i)=XI(i,1)
          SSEEP(M,i)=XI(i,2)
      end do
  end do

  !----------------------------------------------------------------------------
  !Read in ResWeather table
  !----------------------------------------------------------------------------
  !Specify which table to open
  Table="ResET"
  !Read in tables for each reservoir
!    if (.NOT.StrComp(fname,FileName)) then
!        WillClose=.TRUE.
!        WillOpen=.TRUE.
!        ConnStr='DBQ='//trim(ALLTRIM(fname))// &
!            ';DRIVER={Microsoft Access Driver (*.mdb)}'
!    end if
  EVRT=0.0
  EVRTRead=.FALSE.
  do M=1,NRES
      !Initialize fields and numbers of fields
      call initializeaccessvars()
      !Set the filter for these records
      write(TempStr,"(A)")Dam_Number(M)
      ResFilter="WHERE [Dam_Number]='" // &
          trim(ALLTRIM(TempStr)) // "' ORDER BY [Year]"
      indexstring=ResFilter
      !Double Precision Fields
      NDPFIELD=12
      Field(1)="ET01"
      Field(2)="ET02"
      Field(3)="ET03"
      Field(4)="ET04"
      Field(5)="ET05"
      Field(6)="ET06"
      Field(7)="ET07"
      Field(8)="ET08"
      Field(9)="ET09"
      Field(10)="ET10"
      Field(11)="ET11"
      Field(12)="ET12"
      NLOGICFIELD=1
      LOGICFIELDS(1)="Year"
      NSTRINGFIELD=1
      STRINGFIELDS(1)="Dam_Number"
      ! Create a filter for this model only
      call AccessRead()
      if(KOUNTFSQL.LT.NYRS)THEN
          print *,'Reservoir data not complete for reservoir ',trim(TempStr)
      else if (KOUNTFSQL.EQ.0) then
          print *,'No weather data for reservoir ',trim(TempStr)
          STOP
      end if
      AvEvap=0.0
      NumEvap=0
      CALL StoreMonVarXI(EVRT,MRES,MYEAR,13,M,1,1,1)
      DO J=1,NYRS
        DO K=1,13
          EVRT(M,J,K)=EVRT(M,J,K)/12.0
        END DO
      END DO
  end do
END IF
!----------------------------------------------------------------------------
!Read in ModelSubarea table to determine which subareas to read for
!average area precipitation.
!This table contains the average precipitation over the entire subarea
!----------------------------------------------------------------------------
ILNQ=0
LNRS=0
NILNQ=0
SubCropAv=0.0
SubCropAc=0.0
SubCropAcres=0.0
M1IRR=0.0
PRE=0.0
PERCO=0.0
CONUSE=0.0
SM=0.0
WPERC=0.0
TEM=0.0
SUBACRES=0.0
SUBCROPACRES=0.0
SUBCROPAV=0.0
SUBCROPAC=0.0
Alfalfa=0.0
Pasture=0.0
Hay=0.0
Grain=0.0
Corn=0.0
Orchard=0.0
Sorghum=0.0
Turf=0.0
Onions=0.0
OtherHort=0.0
Potatoes=0.0
Berries=0.0
OtherVeg=0.0
Tomatoes=0.0
Beans=0.0
Vineyard=0.0
SubHay=0.0
SubPast=0.0
TotalAcres=0.0
Alfalfa=0.0
Pasture=0.0
Hay=0.0
Grain=0.0
Corn=0.0
Orchard=0.0
Sorghum=0.0
Turf=0.0
Onions=0.0
OtherHort=0.0
Potatoes=0.0
Berries=0.0
OtherVeg=0.0
Tomatoes=0.0
Beans=0.0
Vineyard=0.0
SubHay=0.0
SubPast=0.0
call initializeaccessvars()
Table="ModelSubarea"
NSTRINGFIELD=3
STRINGFIELDS(1)="Subarea"
STRINGFIELDS(2)="BasinNum"
STRINGFIELDS(3)="BasinName"
indexstring=trim(AllTrim(ModelFilter))
call AccessRead()
IF (KOUNTFSQL.GT.0) THEN
  NSUBS=KOUNTFSQL
  DO I=1,NSUBS
    Subareas(I)=trim(ALLTRIM(F90SQLSTRINGS(I,1)))
    Basin(I)=trim(AllTrim(F90SQLSTRINGS(I,2)))
    BasinName(I)=trim(AllTrim(F90SQLSTRINGS(I,3)))
  ENDDO
  DO I=1,NSUBS
    call initializeaccessvars()
    Table="AreaPrecip"
    NSTRINGFIELD=1
    STRINGFIELDS(1)="Subarea"
    NLOGICFIELD=1
    LOGICFIELDS(1)="preYear"
    NDPFIELD=14
    XI=0.0
    FIELD(14)="Acres"
    FIELD(1)="PR1"
    FIELD(2)="PR2"
    FIELD(3)="PR3"
    FIELD(4)="PR4"
    FIELD(5)="PR5"
    FIELD(6)="PR6"
    FIELD(7)="PR7"
    FIELD(8)="PR8"
    FIELD(9)="PR9"
    FIELD(10)="PR10"
    FIELD(11)="PR11"
    FIELD(12)="PR12"
    FIELD(13)="PRann"
    WRITE(tempStr,"(I4)")INYR
    WRITE(tempStr2,"(I4)")INYR+NYRS-1
    indexstring="WHERE (Subarea ='" // trim(AllTrim(Subareas(I))) //  &
      "' AND preYear BETWEEN " // trim(tempStr) // " AND " // &
      trim(tempStr2) // ") ORDER BY preYear"
    call AccessRead()
    LowAcres=.False.
    IF (KOUNTFSQL>0) THEN
      DO J=1,KOUNTFSQL
        SubAcres(I,J)=XI(J,14)
        !IF (SubAcres(I,J).LE.115000.) THEN
        !  LowAcres=.True.
        !  Table="AreaPrecip"
        !  CALL AccessRead()
        !  IF (KOUNTFSQL>0) THEN
        !    DO JJ=1,KOUNTFSQL
        !      SubAcres(I,JJ)=XI(JJ,14)
        !    END DO
        !  END IF
        !  EXIT
        !END IF
      END DO
      CALL StoreMon(I,1,1,1)
    ELSE
      WRITE(6,*)'No subareas precip defined for '//trim(Subarea(I))
      WRITE(*,*)'No subareas precip defined for '//trim(Subarea(I))
    END IF
  END DO
ELSE
   WRITE(6,*)'No subareas defined for this model '
   WRITE(*,*)'No subareas defined for this model '
END IF

AvGW=0.0
IsGW=.False.
NumGW=0

!----------------------------------------------------------------------------
!LAND AREA DATA - Read in all Land Area data and related tables
!----------------------------------------------------------------------------
!Read in ModelLandarea table
!----------------------------------------------------------------------------
!Read in Options
call initializeaccessvars()
Table="ModelLandarea"
NLOGICFIELD=7
LOGICFIELDS(1)="LandID"
LOGICFIELDS(2)="ReturnQX"
LOGICFIELDS(3)="SubIrrQX"
LOGICFIELDS(4)="SubIrrQXUp"
LOGICFIELDS(5)="SubIrrQXDn"
LOGICFIELDS(6)="UseSmStor"
LOGICFIELDS(7)="GWQX"
NSTRINGFIELD=4
STRINGFIELDS(1)="LandArea"
STRINGFIELDS(2)="SubArea"
STRINGFIELDS(3)="LandAreaName"
STRINGFIELDS(4)="PrintedName"
NDPFIELD=2
FIELD(1)="PropUse"
FIELD(2)="GWMining"
indexstring=trim(AllTrim(ModelFilter)) // " ORDER BY RowNum"
call AccessRead()
WillOpen=.FALSE.
NLND=KOUNTFSQL
SSTO=0.0
MOIST=0.0
WMOIS=0.0
do i=1,KOUNTFSQL
   LandID(i)=F90SQLINTEGER(i,1)
   IRT(i)=F90SQLINTEGER(i,2)
   IRV(i)=F90SQLINTEGER(i,3)
   IUR(i)=F90SQLINTEGER(i,4)
   IFT(i)=F90SQLINTEGER(i,5)
   ISOL(i)=F90SQLLOGIC(i,6)
   GWQX(i)=F90SQLINTEGER(i,7)
   LandAreaCode(i)=F90SQLSTRINGS(i,1)
   SubArea(i)=F90SQLSTRINGS(i,2)
   PLAND(i)=F90SQLSTRINGS(i,3)
   LandAreaName(i)=F90SQLSTRINGS(i,4)
   EFPRE(i)=.80
   PropUse(i)=XI(i,1)
   GWMining(i)=XI(i,2)
end do
IF (NLND>0) THEN
  call initializeaccessvars()
  NDPFIELD=11
  !Field(1)="IrrEff"
  Field(1)="SubIrrGWProp"
  !Field(3)="ConveyEff"
  Field(2)="RetFac0"
  Field(3)="RetFac1"
  Field(4)="RetFac2"
  Field(5)="RetFac3"
  Field(6)="RetFac4"
  Field(7)="RetFac5"
  Field(8)="RetFac6"
  Field(9)="RetFac7"
  Field(10)="RetFac8"
  Field(11)="RetFac9"
!  Field(12)="RetFac10"
  call AccessRead()
  do i=1,KOUNTFSQL
    !IEFF(i)=XI(i,1)
    CPRO(i)=XI(i,1)
    !CEFF(i)=XI(i,3)
    do MM=1,10
      PCRF(i,MM)=XI(i,1+MM)
    end do
  end do

  DO L=1,NLND
    !----------------------------------------------------------------------------
    !Read in Landarea Total Acres
    !----------------------------------------------------------------------------
    !call initializeaccessvars()
    !Table="AreaDef"
    !NDPFIELD=1
    !FIELD(1)="acres"
    !indexstring="WHERE lanum='" // trim(AllTrim(LandAreaCode(L))) // "'"
    !call AccessRead()
    !IF (KOUNTFSQL.GT.0) THEN
    !  DO I=1,KOUNTFSQL
    !    DO J=1,NYRS
    !      SubAcres(L,J)=XI(i,1)
    !    END DO
    !  END DO
    !END IF
    !----------------------------------------------------------------------------
    !Read in ModelLandRet
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="ModelLandRet"
    NLOGICFIELD=1
    LOGICFIELDS(1)="YEAR"
    NDPFIELD=2
    FIELD(1)="CanalEff"
    FIELD(2)="OnFarmIrrEff"
    WRITE(TempStr,"(I6)")LandID(L)
    indexstring=trim(ModelFilter) // " AND LandID=" // trim(AllTrim(TempStr)) &
      // " ORDER BY [Year]"
    call AccessRead()
    NumRead=KOUNTFSQL
    Ind1=1
    Ind2=MIN(NumRead,2)
    DO J=1,NYRS
      IND=J+INYR-1
      IF (Ind2.LE.0) THEN
        JJJ=1
      END IF
      IF (IND.GE.F90SQLINTEGER(Ind2,1)) THEN
        DO
          IF (Ind2.EQ.NumRead.AND.Ind1.EQ.Ind2) THEN
            EXIT
          END IF
          IF (IND.LT.F90SQLINTEGER(Ind2,1)) THEN
            EXIT
          END IF
          IF(Ind2.LT.NumRead)THEN
            Ind1=Ind2
            Ind2=Ind2+1
          ELSE
            Ind2=NumRead
            Ind1=Ind2
          END IF
        END DO
      ENDIF
      CEFF(L,J)=XI(Ind1,1)
      IEFF(L,J)=XI(Ind1,2)
      TEF(L,J)=CEFF(L,J)*IEFF(L,J)
    END DO
    !----------------------------------------------------------------------------
    !Read in LandUseByAreaYear table
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="LandUseByAreaYear"
    NLOGICFIELD=1
    LOGICFIELDS(1)="YEAR"
    NDPFIELD=9
    FIELD(1)="Alfalfa"
    FIELD(2)="Pasture"
    FIELD(3)="Hay"
    FIELD(4)="Grain"
    FIELD(5)="Corn"
    FIELD(6)="Orchard"
    FIELD(7)="Sorghum"
    FIELD(8)="Turf"
    FIELD(9)="Onions"
    WRITE(tempStr,"(I4)")INYR
    WRITE(tempStr2,"(I4)")INYR+NYRS-1
    indexstring="WHERE Lanum='" // trim(AllTrim(LandAreaCode(L))) // &
      "' AND [YEAR]>=" // trim(tempStr) // " AND [YEAR]<=" // &
      trim(tempStr2) // " ORDER BY [YEAR]"
    call AccessRead()
    DO i=1,KOUNTFSQL
      Ind1=F90SQLINTEGER(i,1)-INYR+1
      DO M=1,9
        SubCropAcres(L,Ind1,M)=XI(i,M)*PropUse(L)
        SubCropAv(M)=SubCropAv(M)+XI(i,M)/REAL(NYRS)*PropUse(L)
        SubCropAc(L,M)=SubCropAc(L,M)+XI(i,M)/REAL(NYRS)*PropUse(L)
      END DO
    END DO
    !----------------------------------------------------------------------------
    !Read in LandUseByAreaYear table
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="LandUseByAreaYear"
    NLOGICFIELD=1
    LOGICFIELDS(1)="YEAR"
    NDPFIELD=9
    FIELD(1)="OtherHort"
    FIELD(2)="Potatoes"
    FIELD(3)="Berries"
    FIELD(4)="OtherVeg"
    FIELD(5)="Tomatoes"
    FIELD(6)="Beans"
    FIELD(7)="Vineyard"
    FIELD(8)="SubPast"
    FIELD(9)="SubHay"
    WRITE(tempStr,"(I4)")INYR
    WRITE(tempStr2,"(I4)")INYR+NYRS-1
    indexstring="WHERE Lanum='" // trim(AllTrim(LandAreaCode(L))) // &
      "' AND [YEAR]>=" // trim(tempStr) // " AND [YEAR]<=" // &
      trim(tempStr2) // " ORDER BY [YEAR]"
    call AccessRead()
    DO i=1,KOUNTFSQL
      Ind1=F90SQLINTEGER(i,1)-INYR+1
      DO M=1,9
        SubCropAcres(L,Ind1,M+9)=XI(i,M)*PropUse(L)
        SubCropAv(M+9)=SubCropAv(M+9)+XI(i,M)/REAL(NYRS)*PropUse(L)
        SubCropAc(L,M+9)=SubCropAc(L,M+9)+XI(i,M)/REAL(NYRS)*PropUse(L)
      END DO
    END DO
    SubCropAc(L,19)=0.0
    DO M=1,18
      SubCropAc(L,19)=SubCropAc(L,19)+SubCropAc(L,M)
    END DO
    SubCropAv(19)=0.0
    DO M=1,18
      SubCropAv(19)=SubCropAv(19)+SubCropAv(M)
      DO J=1,NYRS
        SubCropAcres(L,J,19)=SubCropAcres(L,J,19)+SubCropAcres(L,J,M)
      END DO
    END DO
    !Add subirrigated pasture and hayland together
    M1IRR(L)=SubCropAC(L,17)+SubCropAc(L,18)
    !----------------------------------------------------------------------------
    !Create arrays of average crops acres
    !----------------------------------------------------------------------------
    DO J=1,NYRS
      DO M=1,16
        TotalAcres(L)=TotalAcres(L)+SubCropAcres(L,J,M)/FLOAT(NYRS)
      END DO
      Alfalfa(L)  =Alfalfa(L)+SubCropAcres(L,J,1)/FLOAT(NYRS)
      Pasture(L)  =Pasture(L)+SubCropAcres(L,J,2)/FLOAT(NYRS)
      Hay(L)      =Hay(L)+SubCropAcres(L,J,3)/FLOAT(NYRS)
      Grain(L)    =Grain(L)+SubCropAcres(L,J,4)/FLOAT(NYRS)
      Corn(L)     =Corn(L)+SubCropAcres(L,J,5)/FLOAT(NYRS)
      Orchard(L)  =Orchard(L)+SubCropAcres(L,J,6)/FLOAT(NYRS)
      Sorghum(L)  =Sorghum(L)+SubCropAcres(L,J,7)/FLOAT(NYRS)
      Turf(L)     =Turf(L)+SubCropAcres(L,J,8)/FLOAT(NYRS)
      Onions(L)   =Onions(L)+SubCropAcres(L,J,9)/FLOAT(NYRS)
      OtherHort(L)=OtherHort(L)+SubCropAcres(L,J,10)/FLOAT(NYRS)
      Potatoes(L) =Potatoes(L)+SubCropAcres(L,J,11)/FLOAT(NYRS)
      Berries(L)  =Berries(L)+SubCropAcres(L,J,12)/FLOAT(NYRS)
      OtherVeg(L) =OtherVeg(L)+SubCropAcres(L,J,13)/FLOAT(NYRS)
      Tomatoes(L) =Tomatoes(L)+SubCropAcres(L,J,14)/FLOAT(NYRS)
      Beans(L)    =Beans(L)+SubCropAcres(L,J,15)/FLOAT(NYRS)
      Vineyard(L) =Vineyard(L)+SubCropAcres(L,J,16)/FLOAT(NYRS)
      SubHay(L)   =SubHay(L)+SubCropAcres(L,J,17)/FLOAT(NYRS)
      SubPast(L)  =SubPast(L)+SubCropAcres(L,J,18)/FLOAT(NYRS)
    END DO
    IF (TotalAcres(L).GT.0.0) THEN
      M1IRR(L)=(SubHay(L)+SubPast(L))/TotalAcres(L)
    ELSE
      M1IRR(L)=0.0
    END IF
    ACRAG(L)=TotalAcres(L)
    !----------------------------------------------------------------------------
    !Read in ModelLandQX table
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="ModelLandQX"
    NLOGICFIELD=6
    LOGICFIELDS(1)="QXin"
    LOGICFIELDS(2)="QXup"
    LOGICFIELDS(3)="QXdn"
    LOGICFIELDS(4)="ResID"
    LOGICFIELDS(5)="ID"
    LOGICFIELDS(6)="ResRow"
    WRITE(TempStr,"(I6)")LandID(L)
    indexstring=trim(ModelFilter) // " AND LandID=" // trim(AllTrim(TempStr))
    LandFilter=indexstring
    HasReservoir=.FALSE.
    call AccessRead()
    NLndQX(L)=KOUNTFSQL
    NQLIN(L)=KOUNTFSQL
    DO i=1,KOUNTFSQL
      IDV(L,i)=F90SQLINTEGER(i,1)
      ILUP(L,i)=F90SQLINTEGER(i,2)
      IBY(L,i)=F90SQLINTEGER(i,3)
      LRES(L,i)=F90SQLINTEGER(i,4)
      QXid(L,i)=F90SQLINTEGER(i,5)
      LRESROW(L,i)=F90SQLINTEGER(i,6)
      if (LRES(L,i)>0) then
        HasReservoir=.TRUE.
        LNRS(L)=LNRS(L)+1
      end if
    END DO
    IF (HasReservoir) THEN
      Table="ModelLandQXup"
      DO IQ=1,NLndQX(L)
        IF (LRES(L,IQ)>0) THEN
          !-----------------------------------------------------------
          !Read in ModelLandQX table
          !-----------------------------------------------------------
          call initializeaccessvars()
          NLOGICFIELD=1
          LOGICFIELDS(1)="QXup"
          WRITE(TempStr,"(I6)")QXid(L,IQ)
          indexstring=trim(LandFilter) // " AND QX=" // &
            trim(AllTrim(TempStr))
          call AccessRead()
          NILNQ(L,IQ)=KOUNTFSQL
          DO mm=1,KOUNTFSQL
            ILNQ(L,IQ,MM)=F90SQLINTEGER(mm,1)
          END DO
        END IF
      END DO
    END IF
    !----------------------------------------------------------------------------
    !Read in AreaInfo table (Canal, irrig. efficiency, subirrigation proportion
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="AreaInfo"
    UsedAreaID(L)=LandAreaCode(L)
    TempStr2=AllTrim(LandAreaCode(L))
    TempStr2=TempStr2(1:8)
    indexstring = "WHERE AreaID IN ('" // trim(AllTrim(LandAreaCode(L))) // "'"
    if (Len_Trim(AllTrim(LandAreaCode(L)))>8) then
      indexstring=trim(indexstring) // ",'" // &
      Trim(TempStr2) // "'"
    end if
    indexstring=trim(indexstring) // ")"
    NDPFIELD=1
    !Field(1)="CanalEff"
    !Field(2)="OnFarmIrrEff"
    Field(1)="SubIrrPro"
    call AccessRead()
    DO J=1,KOUNTFSQL
      !if (ABS(CEFF(L))<0.01) then
      !  CEFF(L)=XI(J,1)
      !end if
      !IF (CEFF(L).GT.2.0) THEN
      !  CEFF(L)=CEFF(L)/100.
      !END IF
      !if (ABS(IEFF(L))<0.01) then
      !  IEFF(L)=XI(J,2)
      !end if
      !IF (IEFF(L).GT.2.0) THEN
      !  IEFF(L)=IEFF(L)/100.
      !END IF
      CPRO(L)=XI(J,1)
    END DO
    IF (KOUNTFSQL.EQ.0) THEN
      ITEMP=INDEX(LandAreaCode(L),"-",.TRUE.)
      if (len(trim(LandAreaCode(L)))>8) then
        call initializeaccessvars()
        indexstring="WHERE AreaID='" // LandAreaCode(L)(1:8) // "'"
        UsedAreaID(L)=LandAreaCode(L)(1:8)
        NDPFIELD=1
        !Field(1)="CanalEff"
        !Field(2)="OnFarmIrrEff"
        Field(1)="SubIrrPro"
        call AccessRead()
        DO J=1,KOUNTFSQL
          !if (ABS(CEFF(L))<0.01) then
          !  CEFF(L)=XI(J,1)
          !END IF
          !IF (CEFF(L).GT.2.0) THEN
          !  CEFF(L)=CEFF(L)/100.
          !END IF
          !if (ABS(IEFF(L))<0.01) then
          !  IEFF(L)=XI(J,2)
          !END IF
          !IF (IEFF(L).GT.2.0) THEN
          !  IEFF(L)=IEFF(L)/100.
          !END IF
          CPRO(L)=XI(J,1)
        END DO
      end if
    END IF
    !----------------------------------------------------------------------------
    !Read in AreaSoilMoisture to calculate soil moisture storage
    !----------------------------------------------------------------------------
    Table="AreaSoilMoisture"
    call initializeaccessvars()
    NDPFIELD=5
    indexstring="WHERE AreaID='" // Trim(AllTrim(LandAreaCode(L))) // "'"
    Field(1)="SW1"
    Field(2)="SW2"
    Field(3)="SW3"
    Field(4)="SW4"
    Field(5)="SW5"
    call AccessRead()
    if (KOUNTFSQL.EQ.0) then
      TempStr2=Trim(AllTrim(LandAreaCode(L)))
      Ind1=Len_Trim(TempStr2) - 1
      indexstring="WHERE AreaID='" // &
        TempStr2(1:Ind1) // "'"
      call AccessRead()
    end if
    DO J=1,KOUNTFSQL
      SM(L,1)=XI(J,1)
      SM(L,2)=XI(J,2)
      SM(L,3)=XI(J,3)
      SM(L,4)=XI(J,4)
      SM(L,5)=XI(J,5)
    END DO
    !Old code 3/23/2011
    !SOIL(L)=((Alfalfa(L)-SubHay(L))*(SM(L,5)+SM(L,4))/2.0 +  &
    !  (Potatoes(L)+Pasture(L)-SubPast(L)+ Hay(L))* &
    !  (SM(L,3)+SM(L,2))/2.0+ (Grain(L)+Corn(L)+Sorghum(L)+Beans(L))* &
    !  SM(L,3) +  &
    !  (Berries(L)+Vineyard(L))*(SM(L,3)+SM(L,2))/2. + (OtherHort(L)) * &
    !  SM(L,2) + (Onions(L)+OtherVeg(L)+Tomatoes(L))* &
    !  (SM(L,1)+SM(L,2))/2. +Turf(L)*SM(L,1)+ Orchard(L)*(SM(L,4)+ &
    !  SM(L,3))/2.0)/12.0
    !WSOIL(L)=(SubHay(L)+SubPast(L))*(SM(L,3)+SM(L,2))/2.0 /12.0
    !Replaced with new 3/23/2011
    SOIL(L)=((SubCropAc(L,1)-SubCropAc(L,18))*(SM(L,5)+SM(L,4))/2.0 +  &
      (SubCropAc(L,11)+SubCropAc(L,2)-SubCropAc(L,17)+ SubCropAc(L,3))* &
      (SM(L,3)+SM(L,2))/2.0+ (SubCropAc(L,4)+SubCropAc(L,5)+SubCropAc(L,7)+ &
      SubCropAc(L,15))* SM(L,3) +  &
      (SubCropAc(L,12)+SubCropAc(L,16))*(SM(L,3)+SM(L,2))/2. + &
      (SubCropAc(L,10)) * SM(L,2) + (SubCropAc(L,9)+SubCropAc(L,13)+ &
      SubCropAc(L,14))* (SM(L,1)+SM(L,2))/2. +SubCropAc(L,8)*SM(L,1)+ &
      SubCropAc(L,6)*(SM(L,4)+ SM(L,3))/2.0)/12.0
    WSOIL(L)=(SubCropAc(L,17)+SubCropAc(L,18))*(SM(L,3)+SM(L,2))/2.0 /12.0
    !----------------------------------------------------------------------------
    !Read in AreaET table to read potential irrigated ET
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="AreaET"
    tempstr=""
    tempstr2=""
    WRITE(tempStr,"(I4)")INYR-1
    WRITE(tempstr2,"(I4)")INYR+NYRS-1
    indexstring = "WHERE AreaID='" // Trim(Alltrim(LandAreaCode(L))) // "'" &
      // " AND [Year]>=" // trim(tempstr) // " AND [Year]<=" // trim(tempstr2) &
      // " ORDER BY [Year]"
    NLOGICFIELD=1
    LOGICFIELDS(1)="Year"
    NDPFIELD=15
    Field(1)="ET01"
    Field(2)="ET02"
    Field(3)="ET03"
    Field(4)="ET04"
    Field(5)="ET05"
    Field(6)="ET06"
    Field(7)="ET07"
    Field(8)="ET08"
    Field(9)="ET09"
    Field(10)="ET10"
    Field(11)="ET11"
    Field(12)="ET12"
    Field(13)="ETann"
    Field(14)="Alfalfa"
    Field(15)="Pasture"
    Field(16)="Hay"
    WRITE(TempStr,"(I6)")INYR
    WRITE(TempStr2,"(I6)")INYR+NYRS-1
    !indexstring="WHERE AreaID='" // trim(AllTrim(UsedAreaID(L))) // &
    !  "' AND [Year]>=" // trim(Alltrim(TempStr)) // " AND [Year]<= " &
    !  // trim(Alltrim(TempStr2)) // " ORDER BY Year"
    call AccessRead()
    ReadIt=.FALSE.
    TempVar=0.0
    if (KOUNTFSQL<=0) then
      WRITE(TempStr2,'(I2)')L
      print *,'No AreaET data for Land Area ' // Trim(TempStr2)
    ELSE
      DO J=1,KOUNTFSQL
        IF (SubCropAcres(L,J,1)+SubCropAcres(L,J,3).LE.0.0) THEN
          AlfWet=0.0
        ELSE
          AlfWet=SubCropAcres(L,J,18)/(SubCropAcres(L,J,1)+SubCropAcres(L,J,3))
        END IF
        IF (SubCropAcres(L,J,2).LE.0.0) THEN
          PastWet=0.0
        ELSE
          PastWet=SubCropAcres(L,J,17)/SubCropAcres(L,J,2)
        END IF
        IF (XI(J,13).LE.0.0) THEN
          WPRO(L,J)=0.0
        ELSE
          WPRO(L,J)=(AlfWet*(XI(J,14)+XI(J,16))+PastWet*XI(J,15))/XI(J,13)
        END IF
        !SubIrrET=SubHay(L)/(Alfalfa(L)+Hay(L))*(XI(J,14)+XI(J,16))/XI(J,13) + &
        !  SubPast(L)/Pasture(L)*XI(J,15)/XI(J,13)
        !WPRO(L,J)=1.0-(XI(J,13)-SubIrrET)/XI(J,13)
        DO K=1,12
          TempVar(j,k)=(1.0-WPRO(L,J))*XI(j,k)*PropUse(L)
        END DO
      Enddo
      CALL StoreMonVar(CONUSE,13,MYEAR,MLAND,L,2,TempVar,MYEAR,13,1,1)
      DO J=1,KOUNTFSQL
        DO K=1,12
          TempVar(j,k)=WPRO(L,J)*XI(j,k)*PropUse(L)
        END DO
      Enddo
      CALL StoreMonVar(WCUSE,MLAND,MYEAR,13,L,1,TempVar,MYEAR,13,1,1)
    end if
    !----------------------------------------------------------------------------
    !Read in AreaET table for Precipitation in excess of ET
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    XII=0.0
    NLOGICFIELD=1
    LOGICFIELDS(1)="Year"
    NDPFIELD=12
    Field(1)="PR01"
    Field(2)="PR02"
    Field(3)="PR03"
    Field(4)="PR04"
    Field(5)="PR05"
    Field(6)="PR06"
    Field(7)="PR07"
    Field(8)="PR08"
    Field(9)="PR09"
    Field(10)="PR10"
    Field(11)="PR11"
    Field(12)="PR12"
    call AccessRead()
    DO i=1,KOUNTFSQL
      DO K=1,12
        XII(i,K)=XI(i,K)*(1.0-M1IRR(L))*PropUse(L)
      END DO
    END DO
    IF (KOUNTFSQL.GT.0) THEN
      CALL StoreMonVar(PERCO,13,MYEAR,MLAND,L,2,XII,Nf90sql,MDPFIELD,1,1)
    END IF
    DO i=1,KOUNTFSQL
      DO K=1,12
        XII(i,K)=XI(i,K)*(M1IRR(L))*PropUse(L)
      END DO
    END DO
    IF (KOUNTFSQL.GT.0) THEN
      CALL StoreMonVar(WPERC,13,MYEAR,MLAND,L,2,XII,Nf90sql,MDPFIELD,1,1)
    END IF
    !----------------------------------------------------------------------------
    !Read in AreaET table for Effective Precipitation in acre-feet
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    NLOGICFIELD=1
    LOGICFIELDS(1)="Year"
    NDPFIELD=12
    Field(1)="ePR01"
    Field(2)="ePR02"
    Field(3)="ePR03"
    Field(4)="ePR04"
    Field(5)="ePR05"
    Field(6)="ePR06"
    Field(7)="ePR07"
    Field(8)="ePR08"
    Field(9)="ePR09"
    Field(10)="ePR10"
    Field(11)="ePR11"
    Field(12)="ePR12"
    call AccessRead()
    DO i=1,KOUNTFSQL
      DO K=1,12
        XI(i,K)=XI(i,K)*PropUse(L)
      END DO
    END DO
    IF (KOUNTFSQL.GT.0.0) THEN
      CALL StoreMonVar(EffPRE,MLAND,MYEAR,13,L,1,XI,Nf90sql,MDPFIELD,1,1)
    END IF
    !----------------------------------------------------------------------------
    !Read in AreaET table for Precipitation in inches
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    NLOGICFIELD=1
    LOGICFIELDS(1)="Year"
    NDPFIELD=12
    Field(1)="TPR01"
    Field(2)="TPR02"
    Field(3)="TPR03"
    Field(4)="TPR04"
    Field(5)="TPR05"
    Field(6)="TPR06"
    Field(7)="TPR07"
    Field(8)="TPR08"
    Field(9)="TPR09"
    Field(10)="TPR10"
    Field(11)="TPR11"
    Field(12)="TPR12"
    call AccessRead()
    IF (KOUNTFSQL.GT.0) THEN
      CALL StoreMonVar(PRE,MLAND,MYEAR,13,L,1,XI,Nf90sql,MDPFIELD,1,1)
    END IF
    if (KOUNTFSQL>0) then
      DO K=1,12
        PR(L,K)=AverVal(K)
        PR(L,13)=PR(L,13)+PR(L,K)
      END DO
    end if
    !----------------------------------------------------------------------------
    !Read in AreaET table for Temperature in degrees Fahrenheit
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    NLOGICFIELD=1
    LOGICFIELDS(1)="Year"
    NDPFIELD=13
    Field(1)="TM01"
    Field(2)="TM02"
    Field(3)="TM03"
    Field(4)="TM04"
    Field(5)="TM05"
    Field(6)="TM06"
    Field(7)="TM07"
    Field(8)="TM08"
    Field(9)="TM09"
    Field(10)="TM10"
    Field(11)="TM11"
    Field(12)="TM12"
    Field(13)="TMann"
    call AccessRead()
    IF (KOUNTFSQL.GT.0) THEN
      CALL StoreMonVar(TEM,MLAND,MYEAR,13,L,1,XI,Nf90sql,MDPFIELD,1,1)
    END IF
    DO J=1,NYRS
      TEM(L,J,13)=TEM(L,J,13)/12.
    END DO
    if (KOUNTFSQL>0) then
      DO K=1,12
        TM(L,K)=AverVal(K)
        TM(L,13)=TM(L,13)+TM(L,K)/12.0
      END DO
    end if
    !----------------------------------------------------------------------------
    !Read in AreaGW table for pumped groundwater
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="AreaGW"
    NLOGICFIELD=1
    LOGICFIELDS(1)="Year"
    NDPFIELD=2
    Field(1)="IrrGW"
    Field(2)="IrrEst"
    NSTRINGFIELD=2
    STRINGFIELDS(1)="AreaID"
    STRINGFIELDS(2)="Name"
    WRITE(TempStr,"(I6)")INYR
    WRITE(TempStr2,"(I6)")INYR+NYRS-1
    indexstring="WHERE [AreaID]='" // trim(AllTrim(LandAreaCode(L))) // &
      "' AND [Year] >=" // trim(Alltrim(TempStr)) // &
      " AND [Year] <= " // trim(ALLTRIM(TempStr2)) // " ORDER BY [Year]"
    call AccessRead()
    IF (KOUNTFSQL>0) THEN
      IF (GWQX(L).LT.1) THEN
        print *,'No Groundqater QX for Land Area',L
      END IF
      DO J=1,KOUNTFSQL
        IND=F90SQLINTEGER(J,1)-INYR+1
        !~~When we feel more comfortable with the IrrEst values AnnGW should be
        !AnnGW(L,IND)=XI(J,1)+XI(J,2)
        AnnGw(L,IND)=(XI(J,1)+XI(J,2))*PropUse(L)
        NumGW(L)=NumGW(L)+1
        IsGW(L,IND)=.True.
        AvGW(L)=AvGW(L)+AnnGW(L,IND)
      END DO
      AvGW(L)=AvGW(L)/FLOAT(NumGW(L))
      DO J=1,NYRS
        IF (.NOT.IsGW(L,J)) THEN
          AnnGW(L,J)=AvGW(L)
        END IF
      END DO
    ELSE
      WRITE(TempStr2,'(I2)')L
      Print *,'No Groundwater data for Land Area ' // Trim(TempStr2)
      DO J=1,NYRS
        AnnGw(L,J)=0.0
      END DO
    END IF
  END DO
END IF
ripareanum=""
!----------------------------------------------------------------------------
!RIPARIAN DATA - Read in riparian area data and related tables
!----------------------------------------------------------------------------
!Read in ModelRIP data
!----------------------------------------------------------------------------
!Read in Options
IF (.NOT.IgnoreRiparian) THEN
  call initializeaccessvars()
  Table="ModelRIP"
  indexstring=Trim(AllTrim(ModelFilter)) // " ORDER BY RowNumber"
  NLOGICFIELD=3
  LOGICFIELDS(1)="QXin"
  LOGICFIELDS(2)="QXup"
  LOGICFIELDS(3)="QXdn"
  NSTRINGFIELD=3
  STRINGFIELDS(1)="LaNum"
  STRINGFIELDS(2)="LaName"
  STRINGFIELDS(3)="PrinterHeading"
  NDPFIELD=1
  FIELD(1)="Proportion"
  call AccessRead()
  NRIP=KOUNTFSQL
  DO J=1,KOUNTFSQL
    IPH(J)=F90SQLINTEGER(J,1)
    IPHU(J)=F90SQLINTEGER(J,2)
    IPHD(J)=F90SQLINTEGER(J,3)
    RipAreaNum(J)=trim(AllTrim(F90SQLSTRINGS(J,1)))
    RipAreaName(J)=trim(AllTrim(F90SQLSTRINGS(J,2)))
    RipHeading(J)=trim(AllTrim(F90SQLSTRINGS(J,3)))
    RipPro(J)=XI(J,1)
  END DO
  IF (NRIP>0) THEN
    WTPRE=0.0
    WETPRE=0.0
    WTEM=0.0
    WETUSE=0.0
    WACRE=0.0
  !----------------------------------------------------------------------------
  !Read in AreaRip Potential ET (acre-feet) data
  !----------------------------------------------------------------------------
    DO L=1,NRIP
      call initializeaccessvars()
      Table="AreaRip"
      WRITE(TempStr,"(I6)")INYR
      WRITE(TempStr2,"(I6)")INYR+NYRS-1
      RipAreaUsed(L)=RipAreaNum(L)
      indexstring=" "
      indexstring="WHERE [AreaID]='" // trim(AllTrim(RipAreaUsed(L))) // "'"
      indexstring=trim(indexstring) // " ORDER BY [Year]"
      NDPFIELD=13
      Field(1)="ET01"
      Field(2)="ET02"
      Field(3)="ET03"
      Field(4)="ET04"
      Field(5)="ET05"
      Field(6)="ET06"
      Field(7)="ET07"
      Field(8)="ET08"
      Field(9)="ET09"
      Field(10)="ET10"
      Field(11)="ET11"
      Field(12)="ET12"
      Field(13)="ETann"
      NLOGICFIELD=1
      LOGICFIELDS(1)="Year"
      call AccessRead()
      IF (KOUNTFSQL.EQ.0) THEN
        if (len(trim(RipAreaNum(L)))>8) then
          RipAreaUsed(L)=RipAreaUsed(L)(1:8)
          indexstring="WHERE [AreaID]='" // &
            trim(AllTrim(RipAreaUsed(L))) // "'" // " ORDER BY [Year]"
          call initializeaccessvars()
          NDPFIELD=13
          Field(1)="ET01"
          Field(2)="ET02"
          Field(3)="ET03"
          Field(4)="ET04"
          Field(5)="ET05"
          Field(6)="ET06"
          Field(7)="ET07"
          Field(8)="ET08"
          Field(9)="ET09"
          Field(10)="ET10"
          Field(11)="ET11"
          Field(12)="ET12"
          Field(13)="ETann"
          NLOGICFIELD=1
          LOGICFIELDS(1)="Year"
          RipAreaUsed(L)=RipAreaNum(L)(1:8)
          indexstring="WHERE [AreaID]='" // trim(RipAreaUsed(L)) // &
            "' ORDER BY [Year]"
          call AccessRead()
          CALL StoreMonVar(WETUSE,13,MYEAR,MLAND,L,2,XI,Nf90sql,MDPFIELD,1,1)
        end if
      else
        CALL StoreMonVar(WETUSE,13,MYEAR,MLAND,L,2,XI,Nf90sql,MDPFIELD,1,1)
      END IF
      DO J=1,NYRS
        DO K=1,13
          WETUSE(K,J,L)=WETUSE(K,J,L)*RipPro(L)
        END DO
      END DO
    !----------------------------------------------------------------------------
    !Read in AreaRip Rainfall (acre-feet) data
    !----------------------------------------------------------------------------
      call initializeaccessvars()
      Table="AreaRip"
      NDPFIELD=13
      Field(1)="PR01"
      Field(2)="PR02"
      Field(3)="PR03"
      Field(4)="PR04"
      Field(5)="PR05"
      Field(6)="PR06"
      Field(7)="PR07"
      Field(8)="PR08"
      Field(9)="PR09"
      Field(10)="PR10"
      Field(11)="PR11"
      Field(12)="PR12"
      Field(13)="PRAnn"
      NLOGICFIELD=1
      LOGICFIELDS(1)="Year"
      call AccessRead()
      CALL StoreMonVar(WETPRE,13,MYEAR,MLAND,L,2,XI,Nf90sql,MDPFIELD,1,1)
      DO J=1,NYRS
        DO K=1,12
          WETPRE(K,J,L)=WETPRE(K,J,L)*RipPro(L)
          WETUSE(K,J,L)=MAX(0.0,WETUSE(K,J,L)-0.8*WETPRE(K,J,L))
          WETUSE(13,J,L)=MAX(0.0,WETUSE(13,J,L)-0.8*WETPRE(K,J,L))
        END DO
      END DO
    !----------------------------------------------------------------------------
    !Read in AreaRip Rainfall (inches) data
    !----------------------------------------------------------------------------
      call initializeaccessvars()
      Table="AreaRip"
      NDPFIELD=13
      Field(1)="TPR01"
      Field(2)="TPR02"
      Field(3)="TPR03"
      Field(4)="TPR04"
      Field(5)="TPR05"
      Field(6)="TPR06"
      Field(7)="TPR07"
      Field(8)="TPR08"
      Field(9)="TPR09"
      Field(10)="TPR10"
      Field(11)="TPR11"
      Field(12)="TPR12"
      Field(13)="TPRAnn"
      NLOGICFIELD=1
      LOGICFIELDS(1)="Year"
      call AccessRead()
      CALL StoreMonVar(WTPRE,MLAND,MYEAR,13,L,1,XI,Nf90sql,MDPFIELD,1,1)
    !----------------------------------------------------------------------------
    !Read in AreaRip Temperature (degrees Fahrenheit)
    !----------------------------------------------------------------------------
      call initializeaccessvars()
      Table="AreaRip"
      NDPFIELD=13
      Field(1)="TM01"
      Field(2)="TM02"
      Field(3)="TM03"
      Field(4)="TM04"
      Field(5)="TM05"
      Field(6)="TM06"
      Field(7)="TM07"
      Field(8)="TM08"
      Field(9)="TM09"
      Field(10)="TM10"
      Field(11)="TM11"
      Field(12)="TM12"
      Field(13)="TMAnn"
      NLOGICFIELD=1
      LOGICFIELDS(1)="Year"
      call AccessRead()
      CALL StoreMonVar(WTEM,MLAND,MYEAR,13,L,1,XI,Nf90sql,MDPFIELD,1,1)
      DO J=1,NYRS
        WTEM(L,J,13)=WTEM(L,J,13)/12.0
      END DO
    !----------------------------------------------------------------------------
    !Read in AreaRipAcres data
    !----------------------------------------------------------------------------
      call initializeaccessvars()
      indexstring="WHERE [AreaID]='" // trim(RipAreaUsed(L)) // "'"
      Table="AreaRipAcres"
      NDPFIELD=1
      Field(1)="RipAcres"
      call AccessRead()
      DO J=1,KOUNTFSQL
        WACRE(L)=XI(J,1)*RipPro(L)
      END DO
    END DO
  END IF
ELSE
  NRIP=0
END IF
if (UseMunicipal) then
  MunGW=0.0
  MunSurf=0.0
  MunDepPot=0.0
  MunDepPTot=0.0
  ResInUse=0.0
  ResOutUse=0.0
  MunDemIn=0.0
  MunDemOut=0.0
  DepIndoor=0.0
  DepOutdoor=0.0
  MunReturn=0.0
  MunSurf=0.0
  MunDep=0.0
  !----------------------------------------------------------------------------
  !MUNICIPAL DATA
  !----------------------------------------------------------------------------
  !Read in ModelMunGroup table - Indoor factors
  !----------------------------------------------------------------------------
  call initializeaccessvars()
  Table="ModelMunGroup"
  indexstring=trim(AllTrim(ModelFilter)) // " ORDER BY RowNum"
  NDPFIELD=6
  Field(1)="IndoorFac1"
  Field(2)="IndoorFac2"
  Field(3)="IndoorFac3"
  Field(4)="IndoorFac4"
  Field(5)="IndoorFac5"
  Field(6)="IndoorFac6"
  NLOGICFIELD=2
  LOGICFIELDS(1)="ID"
  LOGICFIELDS(2)="GWQx"
  NSTRINGFIELD=1
  STRINGFIELDS(1)="MunGroupName"
  call AccessRead()
  NMUN=KOUNTFSQL
  DO L=1,KOUNTFSQL
    MUNDEM(L)=0.0
    MUNID(L)=F90SQLINTEGER(L,1)
    MunGWQx(L)=F90SQLINTEGER(L,2)
    MunGroup(L)=F90SQLSTRINGS(L,1)
    DO K=1,6
      SELECT CASE (TypeYear)
        CASE (WaterYear)
          IF (K.LE.9) THEN
            KK=K+3
          ELSE
            KK=K-9
          END IF
        CASE (WaterYearNov)
          IF(K.LE.10)THEN
            KK=K+2
          ELSE
            KK=K-10
          ENDIF
        CASE DEFAULT
          KK=K
      END SELECT
      QMIP(L,KK)=XI(L,K)
    END DO
  END DO
  if (NMUN>0) then
    !----------------------------------------------------------------------------
    !Read in ModelMunGroup table - The rest of Indoor factors
    !----------------------------------------------------------------------------
    Table="ModelMunGroup"
    indexstring=trim(AllTrim(ModelFilter)) // " ORDER BY RowNum"
    NDPFIELD=6
    Field(1)="IndoorFac7"
    Field(2)="IndoorFac8"
    Field(3)="IndoorFac9"
    Field(4)="IndoorFac10"
    Field(5)="IndoorFac11"
    Field(6)="IndoorFac12"
    call AccessRead()
    DO L=1,KOUNTFSQL
      DO K=7,12
        SELECT CASE (TypeYear)
          CASE (WaterYear)
            IF (K.LE.9) THEN
              KK=K+3
            ELSE
              KK=K-9
            END IF
          CASE (WaterYearNov)
            IF(K.LE.10)THEN
              KK=K+2
            ELSE
              KK=K-10
            ENDIF
          CASE DEFAULT
            KK=K
        END SELECT
        QMIP(L,KK)=XI(L,K-6)
      END DO
    END DO
    !----------------------------------------------------------------------------
    !Read in ModelMunGroup table - Outdoor factors
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="ModelMunGroup"
    indexstring=trim(AllTrim(ModelFilter)) // " ORDER BY RowNum"
    NDPFIELD=12
    Field(1)="OutDoorFac1"
    Field(2)="OutDoorFac2"
    Field(3)="OutDoorFac3"
    Field(4)="OutDoorFac4"
    Field(5)="OutDoorFac5"
    Field(6)="OutDoorFac6"
    Field(7)="OutDoorFac7"
    Field(8)="OutDoorFac8"
    Field(9)="OutDoorFac9"
    Field(10)="OutDoorFac10"
    Field(11)="OutDoorFac11"
    Field(12)="OutDoorFac12"
    NLOGICFIELD=1
    LOGICFIELDS(1)="ModelID"
    call AccessRead()
    DO L=1,KOUNTFSQL
      DO K=1,12
        SELECT CASE (TypeYear)
          CASE (WaterYear)
            IF (K.LE.9) THEN
              KK=K+3
            ELSE
              KK=K-9
            END IF
          CASE (WaterYearNov)
            IF(K.LE.10)THEN
              KK=K+2
            ELSE
              KK=K-10
            ENDIF
          CASE DEFAULT
            KK=K
        END SELECT
        OutDoorFac(L,KK)=XI(L,K)
      END DO
    END DO
    !----------------------------------------------------------------------------
    !Read in ModelMunGroup table - misc. factors
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="ModelMunGroup"
    indexstring=trim(AllTrim(ModelFilter)) // " ORDER BY RowNum"
    NDPFIELD=1
    Field(1)="PrivDomestic"
    NLOGICFIELD=2
    LOGICFIELDS(1)="ID"
    LOGICFIELDS(2)="QXret"
    NSTRINGFIELD=1
    STRINGFIELDS(1)="MunGroupName"
    call AccessRead()
    DO L=1,KOUNTFSQL
      MUNID(L)=F90SQLINTEGER(L,1)
      MunQXRet(L)=F90SQLINTEGER(L,2)
      MunGroup(L)=F90SQLSTRINGS(L,1)
      PrivDomestic(L)=XI(L,13)
    END DO
    !----------------------------------------------------------------------------
    !Read in private domestic landareas (if any)
    !----------------------------------------------------------------------------
    IF (PrivDomestic(L)==0.0.OR.CalcPrivDomesticInt<>0) THEN
      Table="ModelMunGroup"
      indexstring=trim(AllTrim(ModelFilter)) // " ORDER BY RowNum"
      NSTRINGFIELD=3
      STRINGFIELDS(1)="Landarea1"
      STRINGFIELDS(2)="Landarea2"
      STRINGFIELDS(3)="Landarea3"
      call AccessRead()
      DO L=1,KOUNTFSQL
        Landarea(L,1)=F90SQLSTRINGS(L,1)
        Landarea(L,2)=F90SQLSTRINGS(L,2)
        Landarea(L,3)=F90SQLSTRINGS(L,3)
      END DO
    ELSE
      Landarea(L,1)="'"
      Landarea(L,2)="'"
      Landarea(L,3)="'"
    END IF
    !----------------------------------------------------------------------------
    !Read in ReturnFlows table
    ! This table list the return flows from various kinds of uses.
    !----------------------------------------------------------------------------
    call initializeaccessvars()
    Table="ReturnFlows"
    NDPFIELD=6
    Field(1)="ResReturn"
    Field(2)="ComReturn"
    Field(3)="InstReturn"
    Field(4)="IndReturn"
    Field(5)="SewageReturn"
    Field(6)="SepticReturn"
    !Field(7)="OutDoor"
    indexstring=""
    call AccessRead()
    DO J=1,KOUNTFSQL
      ResReturn=XI(J,1)
      ComReturn=XI(J,2)
      InstReturn=XI(J,3)
      IndReturn=XI(J,4)
      SewageReturn=XI(J,5)
      SepticReturn=XI(J,6)
      !OutDoorReturn=XI(J,7)
    END DO
    DO L=1,NMUN
      !----------------------------------------------------------------------------
      !Read in ModelMunProv table
      !----------------------------------------------------------------------------
      WRITE(TempStr,"(I6)")MunID(L)
      indexstring=trim(AllTrim(ModelFilter)) // &
        " AND MunGrpID=" // trim(ALLTRIM(TempStr))
      TempStr2=indexstring
      call initializeaccessvars()
      Table="ModelMunProv"
      NLOGICFIELD=1
      LOGICFIELDS(1)="WResID"
      NSTRINGFIELD=1
      STRINGFIELDS(1)="Name"
      call AccessRead()
      NCOMM(L)=KOUNTFSQL
      IF (KOUNTFSQL>0) THEN
        DO J=1,KOUNTFSQL
          MunProv(L,J)=F90SQLINTEGER(J,1)
          MunName(L,J)=F90SQLSTRINGS(J,1)
        END DO
      END IF
      !----------------------------------------------------------------------------
      !Read in ModelMunRet
      !----------------------------------------------------------------------------
      call initializeaccessvars()
      Table="ModelMunRet"
      NLOGICFIELD=1
      LOGICFIELDS(1)="YEAR"
      NDPFIELD=1
      FIELD(1)="OutRet"
      WRITE(TempStr,"(I6)")MunID(L)
      indexstring=trim(ModelFilter) // " AND MunGrpID=" // trim(AllTrim(TempStr)) &
        // " ORDER BY [Year]"
      call AccessRead()
      NumRead=KOUNTFSQL
      Ind1=1
      Ind2=MIN(NumRead,2)
      DO J=1,NYRS
        IND=J+INYR-1
        IF (Ind2.LE.0) THEN
          JJJ=1
        END IF
        IF (IND.GE.F90SQLINTEGER(Ind2,1)) THEN
          DO
            IF (Ind2.EQ.NumRead.AND.Ind1.EQ.Ind2) THEN
              EXIT
            END IF
            IF (IND.LT.F90SQLINTEGER(Ind2,1)) THEN
              EXIT
            END IF
            IF(Ind2.LT.NumRead)THEN
              Ind1=Ind2
              Ind2=Ind2+1
            ELSE
              Ind2=NumRead
              Ind1=Ind2
            END IF
          END DO
        END IF
        OutDoorReturn(L,J)=XI(Ind1,1)
      END DO
      !----------------------------------------------------------------------------
      !Read in ModelMunNonComm table
      !----------------------------------------------------------------------------
      Table="ModelMunNonComm"
      call initializeaccessvars()
      NLOGICFIELD=1
      LOGICFIELDS(1)="WResID"
      NSTRINGFIELD=1
      STRINGFIELDS(1)="Name"
      WRITE(TempStr,"(I6)")MunID(L)
      indexstring=trim(ModelFilter) // " AND MunGrpID=" // trim(AllTrim(TempStr))
      call AccessRead()
      NumNonComm(L)=KOUNTFSQL
      DO J=1,KOUNTFSQL
        NonComm(L,J)=F90SQLINTEGER(J,1)
        NonName(L,J)=F90SQLSTRINGS(J,1)
      END DO
      !----------------------------------------------------------------------------
      !Read in ModelMunQX table
      !----------------------------------------------------------------------------
      call initializeaccessvars()
      Table="ModelMunQX"
      NLOGICFIELD=6
      LOGICFIELDS(1)="ID"
      LOGICFIELDS(2)="QXin"
      LOGICFIELDS(3)="QXup"
      LOGICFIELDS(4)="QXdn"
      LOGICFIELDS(5)="ResID"
      LOGICFIELDS(6)="DamRow"
!      NSTRINGFIELD=1
!      STRINGFIELDS(1)="DamID"
      WRITE(TempStr,"(I6)")MUNID(L)
      indexstring=trim(ModelFilter) // " AND MunGrpID="  // &
        trim(AllTrim(TempStr))
      call AccessRead()
      NQXMun(L)=KOUNTFSQL
      HasReservoir=.false.
      if (KOUNTFSQL>0) then
        DO J=1,KOUNTFSQL
          MunQXid(L,J)=F90SQLINTEGER(J,1)
          MunQXin(L,J)=F90SQLINTEGER(J,2)
          MunQXup(L,J)=F90SQLINTEGER(J,3)
          MunQXdn(L,J)=F90SQLINTEGER(J,4)
          MResID(L,J)=F90SQLINTEGER(J,5)
          MResNum(L,J)=F90SQLINTEGER(J,6)
!          MDamID(L,J)=F90SQLSTRINGS(J,1)
          if (MResID(L,J)>0) then
            MLNRS(L)=MLNRS(L)+1
            DO M=1,NRES
              IF (ReserID(M).EQ.MResID(L,J)) THEN
                MRESUP(L,MLNRS(L))=M
              END IF
            END DO
            HasReservoir = .true.
          end if
        END DO
      end if
      !----------------------------------------------------------------------------
      !Read in ModelMunSSIndustry table
      !----------------------------------------------------------------------------
      call initializeaccessvars()
      Table="ModelMunSSIndustry"
      NLOGICFIELD=1
      LOGICFIELDS(1)="WResID"
      NSTRINGFIELD=1
      STRINGFIELDS(1)="Name"
      call AccessRead()
      NMunSS(L)=KOUNTFSQL
      DO J=1,KOUNTFSQL
        MSSWresID(L,J)=F90SQLINTEGER(J,1)
        MSSname(L,J)=F90SQLSTRINGS(J,1)
      END DO
      !----------------------------------------------------------------------------
      !Read in ModelMunQXup table
      !----------------------------------------------------------------------------
      IF (HasReservoir) THEN
        DO J=1,NQXMun(L)
          IF (MResID(L,J)>0) THEN
            call initializeaccessvars()
            WRITE(TempStr,"(I6)")MunQXid(L,J)
            indexstring = trim(AllTrim(TempStr2)) // " AND QXid=" // &
              trim(ALLTRIM(TempStr))
            Table="ModelMunResQXup"
            NLOGICFIELD=1
            LOGICFIELDS(1)="QXup"
            call AccessRead()
            NQXup(L,J)=KOUNTFSQL
            DO M=1,KOUNTFSQL
              MQXup(L,J,M)=F90SQLINTEGER(M,1)
            END DO
          END IF
        END DO
      END IF
    END DO
    DivIndoor=0.0
    DivOutdoor=0.0
    DivGW=0.0
    DivSurf=0.0
    MunDepPot=0.0
    MunDepSec=0.0
    MunDepPTot=0.0
    MunDepSTot=0.0
    RetIndoor=0.0
    RetOutdoor=0.0
    DO L=1,NMUN
      !------------------------------------------------------------------
      !Read in private domestic values
      !------------------------------------------------------------------
      IF (PrivDomestic(L)==0.0.OR.CalcPrivDomesticInt<>0) THEN
        !Test to see if there is any private domestic.
        FoundIt = .FALSE.
        DO I=1,3
          if (INDEX(AllTrim(LandArea(L,I)),"-")<=0) then
            CYCLE
          end if
          J=Len(Trim(AllTrim(LandArea(L,I))))
          IF(J<=11.AND.J.GT.4) Then
            FoundIt=.TRUE.
          END IF
        END DO
        IF (FoundIt) THEN
          call initializeaccessvars()
          Table="AreaPrivDom"
          IndexString="WHERE Landarea IN ("
          DO I = 1,3
            if (INDEX(AllTrim(LandArea(L,I)),"-")<=0) then
              CYCLE
            end if
            If (Len(Trim(AllTrim(IndexString))) > 21) Then
              IndexString = Trim(IndexString) // ", "
            End If
            IF (Len(Trim(AllTrim(LandArea(L,I))))>1) THEN
              IndexString = Trim(IndexString) // "'" // &
                Trim(AllTrim(Landarea(L,I))) // "'"
            END IF
          END DO
          If (Len(Trim(IndexString)) > 13) Then
            IndexString = Trim(AllTrim(IndexString)) // ")"
          Else
            IndexString = ""
          End If
          NSTRINGFIELD=2
          STRINGFIELDS(1)="Landarea"
          STRINGFIELDS(2)="LaName"
          NDPFIELD=1
          Field(1)="PrivDom"
          call AccessRead()
          PrivDomestic(L)=0.0
          DO J=1,KOUNTFSQL
            PrivDomestic(L)=PrivDomestic(L)+XI(J,1)
          END DO
        ELSE
          PrivDomestic(L)=0.0
        END IF
      END IF
      if (PrivDomestic(L)>0.0) then
        DO J=1,NYRS
          DO K=1,12
            TotIn=PrivDomestic(L)/3.0*QMIP(L,K)
            TotOut=PrivDomestic(L)*2.0/3.0*OutDoorFac(L,K)
            ResInUse(L,J,K)=ResInUse(L,J,K)+TotIn
            ResInUse(L,J,13)=ResInUse(L,J,13)+TotIn
            ResOutUse(L,J,K)=ResOutUse(L,J,K)+TotOut
            ResOutUse(L,J,13)=ResOutUse(L,J,13)+TotOut
            MunDemIn(L,J,K)=MunDemIn(L,J,K)+TotIn
            MunDemIn(L,J,13)=MunDemIn(L,J,13)+TotIn
            MunDemOut(L,J,K)=MunDemOut(L,J,K)+TotOut
            MunDemOut(L,J,13)=MunDemOut(L,J,13)+TotOut
            MunGW(L,J,K)=MunGW(L,J,K)+TotIn+TotOut
            MunGW(L,J,13)=MunGW(L,J,13)+TotIn+TotOut
            RetIn=TotIn*ResReturn*SepticReturn
            RetOut=TotOut*OutdoorReturn(L,J)
            MunDepPot(L,J,K)=MunDepPot(L,J,K)+(TotIn-RetIn)+(TotOut-RetOut)
            MunDepPot(L,J,13)=MunDepPot(L,J,13)+(TotIn-RetIn)+(TotOut-RetOut)
            MunDepPTot(J,K)=MunDepPTot(J,K) + (TotIn-RetIn)+(TotOut-RetOut)
            DepIndoor(L,J,K)=DepIndoor(L,J,K)+(TotIn-RetIn)
            DepIndoor(L,J,13)=DepIndoor(L,J,13)+(TotIn-RetIn)
            DepOutDoor(L,J,K)=DepOutDoor(L,J,K)+(TotOut-RetOut)
            DepOutdoor(L,J,13)=DepOutdoor(L,J,13)+(TotOut-RetOut)
            MunDep(L,J,K)=MunDep(L,J,K)+(TotIn-RetIn)+(TotOut-RetOut)
            MunDep(L,J,13)=MunDep(L,J,13)+(TotIn-RetIn)+(TotOut-RetOut)
            MunReturn(L,J,K)=MunReturn(L,J,K)+ RetIn+RetOut
            DivGW(L,J,K)=DivGW(L,J,K)+TotIn+TotOut
            DivGW(L,J,13)=DivGW(L,J,13)+TotIn+TotOut
          END DO
          IF (J.EQ.1) THEN
            JJJ=1
          END IF
        END DO
      end if
      IF (NMunSS(L)>0) THEN
        call initializeaccessvars()
        Table="SSI"
        NLOGICFIELD=2
        LOGICFIELDS(1)="Year"
        LOGICFIELDS(2)="WaterResID"
        NDPFIELD=9
        Field(1)="ResIn"
        Field(2)="ResOut"
        Field(3)="Comm"
        Field(4)="Inst"
        Field(5)="Ind"
        Field(6)="SecOut"
        Field(7)="SecComm"
        Field(8)="SecInst"
        Field(9)="SecInd"
        DO M=1,NMunSS(L)
          WRITE(TempStr2,"(I5)")MSSWresID(L,M)
          IndexString="WHERE WaterResID=" // Trim(AllTrim(TempStr2))
          CALL AccessRead()
          NssPTs(L,M)=KOUNTFSQL
          DO J=1,KOUNTFSQL
            SSyear(L,M,J)=F90SQLINTEGER(J,1)
            SSin(L,M,J)=XI(J,1)
            SSout(L,M,J)=XI(J,2)
            SSComm(L,M,J)=XI(J,3)
            SSInst(L,M,J)=XI(J,4)
            SSInd(L,M,J)=XI(J,5)
            SSCout(L,M,J)=XI(J,6)
            SSCcomm(L,M,J)=XI(J,7)
            SSCInst(L,M,J)=XI(J,8)
            SSCInd(L,M,J)=XI(J,9)
          END DO
          Ind1=1
          Ind2=MIN(2,NssPTs(L,M))
          DO J=1,NYRS
            IF (J.GE.SSyear(L,M,Ind2)) THEN
              DO
                IF(Ind2.EQ.NssPTs(L,M).AND.Ind1.EQ.Ind2)THEN
                  EXIT
                END IF
                IF (J.LT.SSyear(L,M,Ind2)) THEN
                  EXIT
                END IF
                IF (Ind2.LT.NssPTs(L,M)) THEN
                  Ind1=Ind2
                  Ind2=Ind2+1
                ELSE
                  Ind2=NssPTs(L,M)
                  Ind1=Ind2
                END IF
              END DO
            ENDIF
            DO K=1,12
              TotIn=(SSin(L,M,Ind1)+.8*SSComm(L,M,Ind1)+.2*SSInst(L,M,Ind1)+ &
                SSInd(L,M,Ind1))* QMIP(L,K)
              RetIn=(SSin(L,M,Ind1)*ResReturn+.8*SSComm(L,M,Ind1)*ComReturn &
                +.2*SSInst(L,M,Ind1)*InstReturn)* QMIP(L,K)*SepticReturn + &
                SSInd(L,M,Ind1)*QMIP(L,K)*IndReturn
              TotInd=SSCInd(L,M,Ind1)*QMIP(L,K)
              TotPout=(SSOut(L,M,Ind1)+SSCout(L,M,Ind1)+.2*SSComm(L,M,Ind1)+ &
                .8*SSInst(L,M,Ind1))*OutDoorFac(L,K)
              TotPret=TotPout*OutDoorReturn(L,J)
              TotOut= (SSOut(L,M,Ind1)+SSCout(L,M,Ind1)+.2*SSComm(L,M,Ind1)+ &
                .8*SSInst(L,M,Ind1)+SSCComm(L,M,Ind1)+SSCInst(L,M,Ind1))* &
                OutDoorFac(L,K) + TotInd
              RetOut=(TotOut-TotInd)*OutDoorReturn(L,J)+TotInd*IndReturn
              TotOutInd=(SSCout(L,M,Ind1)+SSCcomm(L,M,Ind1)+SSCInst(L,M,Ind1)) &
                * OutDoorFac(L,K) + TotInd
              !Eliminate the secondary use from groundwater outdoor.
              !All secondary water is considered outdoor except SSCInd(L,M,Ind1)
              !which is considered indoor used, for example, for power plant
              !Cooling
              IF (L.EQ.3.AND.K.EQ.1) THEN
                 JJJ=1
              END IF
              !---------------------------------------------
              !Reinserted Secondary Depletion 11/6/2012
              !---------------------------------------------
              Secondary(L,J,K)=Secondary(L,J,K)+TotOutInd
              Secondary(L,J,13)=Secondary(L,J,13)+TotOutInd
              MunDepSec(L,J,K)=MunDepSec(L,J,K)+(TotOutInd-TotInd)* &
                (1.0-OutDoorReturn(L,J))+TotInd*(1.0-IndReturn)
              MunDepSec(L,J,13)=MunDepSec(L,J,13)+(TotOutInd-TotInd)* &
                (1.0-OutDoorReturn(L,J))+TotInd*(1.0-IndReturn)
              !---------------------------------------------
              MunGW(L,J,K)=MunGW(L,J,K)+TotIn+TotOut-TotOutInd
              MunGW(L,J,13)=MunGW(L,J,13)+TotIn+TotOut-TotOutInd
              MunSurf(L,J,K)=MunSurf(L,J,K)+TotOutInd
              MunSurF(L,J,13)=MunSurf(L,J,13)+TotOutInd
              MunDepPTot(J,K)=MunDepPTot(J,K)+ (SSin(L,M,Ind1)+ &
                .8*SSComm(L,M,Ind1)+.2*SSInst(L,M,Ind1))*QMIP(L,K) &
                * (1.0-ResReturn*SepticReturn) + SSInd(L,M,Ind1)*QMIP(L,K) &
                * (1.0 - IndReturn)
              MunDepPTot(J,13)=MunDepPTot(J,13)+ (SSin(L,M,Ind1)+ &
                .8*SSComm(L,M,Ind1)+.2*SSInst(L,M,Ind1))*QMIP(L,K) &
                * (1.0-ResReturn*SepticReturn) + SSInd(L,M,Ind1)*QMIP(L,K) &
                * (1.0 - IndReturn)
              MunDepSTot(J,K)=MunDepSTot(J,K)+(TotOutInd-TotInd)* &
                (1.0-OutDoorReturn(L,J))+TotInd*(1.0-IndReturn)
              MunDepSTot(J,13)=MunDepSTot(J,13)+(TotOutInd-TotInd)* &
                (1.0-OutDoorReturn(L,J))+TotInd*(1.0-IndReturn)
              MunDemIn(L,J,K)=MunDemIn(L,J,K)+TotIn
              MunDemIn(L,J,13)=MunDemIn(L,J,13)+TotIn
              MunDemOut(L,J,K)=MunDemOut(L,J,K)+TotOut
              MunDemOut(L,J,13)=MunDemOut(L,J,13)+TotOut
              MunDepPot(L,J,K)=MunDepPot(L,J,K)+TotIn-RetIn+TotPout-TotPret
              MunDepPot(L,J,13)=MunDepPot(L,J,13)+TotIn-RetIn+TotPout-TotPret
              DepIndoor(L,J,K)=DepIndoor(L,J,K)+TotIn+TotInd-RetIn
              DepIndoor(L,J,13)=DepIndoor(L,J,13)+TotIn+TotInd-RetIn
              DepOutdoor(L,J,K)=DepOutdoor(L,J,K)+TotOut-RetOut
              DepOutdoor(L,J,13)=DepOutdoor(L,J,13)+TotOut-RetOut
              MunDep(L,J,K)=MunDep(L,J,K)+TotIn-RetIn+TotOut-RetOut
              MunDep(L,J,13)=MunDep(L,J,13)+TotIn-RetIn+TotOut-RetOut
              MunReturn(L,J,K)=MunReturn(L,J,K)+RetIn+RetOut
              SSIDep(L,J,K)=SSIDep(L,J,K)+TotIn-RetIn+TotOut-RetOut
              SSIDep(L,J,13)=SSIDep(L,J,13)+TotIn-RetIn+TotOut-RetOut
              SSIDepM(L,K)=SSIDepM(L,K)+(TotIn-RetIn+TotOut-RetOut)/FLOAT(NYRS)
              SSIDepM(L,13)=SSIDepM(L,13)+(TotIn-RetIn+TotOut-RetOut)/FLOAT(NYRS)
              SSIDeplete(J,K)=SSIDeplete(J,K)+TotIn-RetIn+TotOut-RetOut
              SSIDeplete(J,13)=SSIDeplete(J,13)+TotIn-RetIn+TotOut-RetOut
              SSIDepleteM(K)=SSIDepleteM(K)+(TotIn-RetIn+TotOut-RetOut) &
                /FLOAT(NYRS)
              SSIDepleteM(13)=SSIDepleteM(13)+(TotIn-RetIn+TotOut-RetOut) &
                /FLOAT(NYRS)
            END DO
          END DO
        END DO
      END IF
    END DO
    DO L=1,NMUN
      !--------------------------------------------------------------------
      ! Use TempNon for temporary storage of each provider.  Use the
      ! following columns:
      !--------------------------------------------------------------------
      !--------------------------------------------------------------------
      ! Calculate values for community systems
      !--------------------------------------------------------------------
      DO M=1,NCOMM(L)
        !--------------------------------------------------------------------
        !  Read in reliable capacity for each system
        !  From POTABLE table
        !--------------------------------------------------------------------
        call initializeaccessvars()
        Table="Potable"
        NLOGICFIELD=2
        LOGICFIELDS(1)="WaterResID"
        LOGICFIELDS(2)="Year"
        NDPFIELD=11
        Field(1)="RelSpring"
        Field(2)="RelWells"
        Field(3)="RelSurface"
        Field(4)="MaxSprings"
        Field(5)="MaxWells"
        Field(6)="MaxSurface"
        Field(7)="ResIn"
        Field(8)="ResOut"
        Field(9)="Com"
        Field(10)="Inst"
        Field(11)="Ind"
        WRITE(TempStr,"(I6)")MunProv(L,M)
        indexstring="WHERE [WaterResID]=" // trim(AllTrim(TempStr)) &
          // " ORDER BY [Year]"
        call AccessRead()
        IF (KOUNTFSQL.EQ.0) THEN
          print *,"No data for ",MunProv(L,M)
          WRITE(6,*)"No data for ",MunProv(L,M)
        END IF
        NumRead=KOUNTFSQL
        DO J=1,KOUNTFSQL
          IND=F90SQLINTEGER(J,2)-INYR+1
          TempArr(J,1)=REAL(IND,KIND=8)
          TempArr(J,2)=XI(J,1) ! Reliable Spring Supply
          TempArr(J,3)=XI(J,2) ! Reliable Well Supply
          TempArr(J,4)=XI(J,3) ! Reliable Surface Supply
          TempArr(J,5)=XI(J,1)+XI(J,2)+XI(J,3) ! Total reliable capacity
          TempArr(J,6)=XI(J,4) ! Max Spring Supply
          TempArr(J,7)=XI(J,5) ! Max Well Supply
          TempArr(J,8)=XI(J,6) ! Max Surface Supply
          TempArr(J,9)=XI(J,4)+XI(J,5)+XI(J,6)
          TempArr(J,10)=XI(J,7) ! Residential Indoor
          TempArr(J,11)=XI(J,8) ! Residential Outdoor
          TempArr(J,12)=XI(J,9) ! Commercial
          TempArr(J,13)=XI(J,10) ! Institutional
          TempArr(J,14)=XI(J,11) ! Industrial
        END DO
        !--------------------------------------------------------------------
        !  Calculate CII values for each system for each year
        !--------------------------------------------------------------------
        Ind1=1
        Ind2=MIN(NumRead,2)
        if (NumRead>0) then
          DO J=1,NYRS
            IND=J+INYR-1
            IF (Ind2.LE.0) THEN
              JJJ=1
            END IF
            IF (IND.GE.F90SQLINTEGER(Ind2,2)) THEN
              DO
                IF (Ind2.EQ.NumRead.AND.Ind1.EQ.Ind2) THEN
                  EXIT
                END IF
                IF (IND.LT.F90SQLINTEGER(Ind2,2)) THEN
                  EXIT
                END IF
                if(Ind2.LT.NumRead) THEN
                  Ind1=Ind2
                  Ind2=Ind2+1
                ELSE
                  Ind2=NumRead
                  Ind1=Ind2
                END IF
              END DO
            END IF
            IF (TempArr(Ind1,5)>0.0) THEN ! Use reliable supply pro if there
              GWprop = (TempArr(Ind1,2)+TempArr(Ind1,3))/TempArr(Ind1,5)
            ELSE IF(TempArr(Ind1,9)>0.0) THEN ! Else use max supply if there
              GWprop = (TempArr(Ind1,6)+TempArr(Ind1,7))/TempArr(Ind1,9)
            ELSE ! Otherwise use 2005 data if there ELSE assume all groundwater
              GWprop=1.0
              DO IA=1,NumRead
                IF (TempArr(IA,1)==(2005-INYR+1)) THEN
                  IF (TempArr(IA,5)>0.0) THEN
                    GWprop=(TempArr(IA,2)+TempArr(IA,3))/TempArr(IA,5)
                  ELSE IF (TempArr(IA,9)>0.0) THEN
                    GWprop = (TempArr(IA,6)+TempArr(IA,7))/TempArr(IA,9)
                  ELSE
                    GWprop = 1.0
                  END IF
                  EXIT
                END IF
              END DO
            ENDIF
            DivOutdoor(M,J,13)=0.0
            DivIndoor(M,J,13)=0.0
            RetOutDoor(M,J,13)=0.0
            RetIndoor(M,J,13)=0.0
            DivGW(M,J,13)=0.0
            DivGW(M,J,13)=0.0
            DO K=1,12
              ResInUse(L,J,K)=ResInUse(L,J,K)+TempArr(IND1,10)*QMIP(L,K)
              ResOutUse(L,J,K)=ResOutUse(L,J,K)+TempArr(IND1,11)* &
                OutDoorFac(L,K)
              CommUse(L,J,K)=CommUse(L,J,K)+0.8*TempArr(Ind1,12)*QMIP(L,K) + &
                0.2*TempArr(Ind1,12)*OutDoorFac(L,K)
              InstUse(L,J,K)=InstUse(L,J,K)+0.2*TempArr(Ind1,13)*QMIP(L,K) + &
                0.8*TempArr(Ind1,13)*OutDoorFac(L,K)
              IndUse(L,J,K)=IndUse(L,J,K)+TempArr(Ind1,14)*QMIP(L,K)
              TotIn=(TempArr(Ind1,10)+0.8*TempArr(Ind1,12)+0.2*TempArr(Ind1,13)+ &
                TempArr(Ind1,14))* QMIP(L,K)
              TotOut=(TempArr(ind1,11)+0.2*TempArr(Ind1,12)+0.8*TempArr(Ind1,13))* &
                OutDoorFac(L,K)
              DivIndoor(M,J,K)=DivIndoor(M,J,K)+TotIn
              DivOutdoor(M,J,K)=DivOutdoor(M,J,K)+TotOut
              MunDemIn(L,J,K)=MunDemIn(L,J,K)+TotIn
              MunDemIn(L,J,13)=MunDemIn(L,J,13)+TotIn
              MunDemOut(L,J,K)=MunDemOut(L,J,K)+TotOut
              MunDemOut(L,J,13)=MunDemOut(L,J,13)+TotOut
              RetIn=(TempArr(Ind1,10)*ResReturn + &
                0.8*TempArr(Ind1,12)*ComReturn + &
                0.2*TempArr(Ind1,13)*InstReturn+TempArr(Ind1,14)*IndReturn) * QMIP(L,K)
              RetOut=(TempArr(Ind1,11)+ &
                0.2*TempArr(Ind1,12)+0.8*TempArr(Ind1,13))* &
                OutDoorReturn(L,J) * OutDoorFac(L,K)
              RetIndoor(M,J,K)=RetIndoor(M,J,K)+RetIn
              RetIndoor(M,J,13)=RetIndoor(M,J,13)+RetIn
              RetOutDoor(M,J,K)=RetOutDoor(M,J,K)+RetOut
              RetOutDoor(M,J,13)=RetOutDoor(M,J,13)+RetOut
              MunGW(L,J,K)=MunGW(L,J,K)+(TotIn+TotOut)*GWprop
              MunGW(L,J,13)=MunGW(L,J,13)+(TotIn+TotOut)*GWprop
              DivGW(M,J,K)=(TotIn+TotOut)*GWprop
              DivGW(M,J,13)=DivGW(M,J,13)+(TotIn+TotOut)*GWprop
              DivSurf(M,J,K)=(TotIn+TotOut)*(1.0-GWProp)
              DivSurf(M,J,13)=DivSurf(M,J,13)+(TotIn+TotOut)*(1.0-GWProp)
              MunSurf(L,J,K)=MunSurf(L,J,K)+ (TotIn+TotOut)*(1.0-GWProp)
              DivOutdoor(M,J,13)=DivOutdoor(M,J,13)+DivOutdoor(M,J,K)
              DivIndoor(M,J,13)=DivIndoor(M,J,13)+DivIndoor(M,J,K)
              MunDepPot(L,J,K)=MunDepPot(L,J,K) + TotOut-RetOut
              MunDepPot(L,J,13)=MunDepPot(L,J,13)+ TotOut-RetOut
              DepOutdoor(L,J,K)=DepOutdoor(L,J,K)+TotOut-RetOut
              DepOutdoor(L,J,13)=DepOutdoor(L,J,13)+TotOut-RetOut
            END DO
          END DO
        end if
        !--------------------------------------------------------------------
        !  Read in secondary values
        !--------------------------------------------------------------------
        call initializeaccessvars()
        Table="Secondary2"
        NLOGICFIELD=2
        LOGICFIELDS(1)="WaterResID"
        LOGICFIELDS(2)="Year"
        NDPFIELD=4
        Field(1)="Com"
        Field(2)="Res"
        Field(3)="Inst"
        Field(4)="Ind"
        WRITE(TempStr,"(I6)")MunProv(L,M)
        indexstring="WHERE WaterResID=" // trim(AllTrim(TempStr)) &
          // " ORDER BY [Year]"
        call AccessRead()
        TempVar=0.0
        NumRead=KOUNTFSQL
        NumSecData=0
        SecYear=0
        SecRes=0.0
        SecCom=0.0
        SecInst=0.0
        SecInd=0.0
        PrevYear=0
        DO J=1,KOUNTFSQL
          IF (F90SQLINTEGER(J,2).NE.PrevYear) THEN
            NumSecData=NumSecData+1
            PrevYear=F90SQLINTEGER(J,2)
            SecYear(NumSecData)=PrevYear
          END IF
          SecRes(NumSecData)=SecRes(NumSecData)+XI(J,1)
          SecCom(NumSecData)=SecCom(NumSecData)+XI(J,2)
          SecInst(NumSecData)=SecInst(NumSecData)+XI(J,3)
          SecInd(NumSecData)=SecInd(NumSecData)+XI(J,4)
        END DO
        Ind1=1
        Ind2=MIN(NumSecData,2)
        if (NumSecData.GT.0) then
          DO J=1,NYRS
            IND=J+INYR-1
            IF (IND.GE.SecYear(Ind2)) THEN
              DO
                IF (Ind2.EQ.NumSecData.AND.Ind1.EQ.Ind2) THEN
                  EXIT
                END IF
                IF (IND.LT.SecYear(Ind2)) THEN
                  EXIT
                END IF
                if(Ind2.LT.NumSecData) THEN
                  Ind1=Ind2
                  Ind2=min(NumSecData,Ind2+1)
                ELSE
                  Ind2=NumSecData
                  Ind1=Ind2
                END IF
              END DO
            END IF
            RetOutdoor(M,J,13)=0.0
            DivOutdoor(M,J,13)=0.0
            DO K=1,12
              TotOut=SecRes(Ind1)+SecCom(Ind1)+SecInst(Ind1)
              TotInd=SecInd(Ind1)
              Secondary(L,J,K)=Secondary(L,J,K)+TotOut* &
                OutDoorFac(L,K)+TotInd*QMIP(L,K)
              DivOutdoor(M,J,K)=DivOutdoor(M,J,K)+TotOut*OutDoorFac(L,K) + &
                TotInd*QMIP(L,K)
              MunSurf(L,J,K)=MunSurf(L,J,K)+TotOut*OutDoorFac(L,K) + &
                TotInd*QMIP(L,K)
              MunDemOut(L,J,K)=MunDemOut(L,J,K)+TotOut*OutDoorFac(L,K) + &
                TotInd*QMIP(L,K)
              MunDemOut(L,J,13)=MunDemOut(L,J,13)+XI(Ind1,1)*OutDoorFac(L,K)
              RetOutdoor(M,J,K)=RetOutdoor(M,J,K)+TotOut* &
                OutDoorFac(L,K)*OutDoorReturn(L,J)
              RetOutdoor(M,J,13)=RetOutdoor(M,J,13)+TotOut* &
                OutDoorFac(L,K)*OutDoorReturn(L,J)
              !RetOutdoor(M,J,13)=RetOutdoor(M,J,13)+RetOutdoor(M,J,K)
              DivOutdoor(M,J,13)=DivOutdoor(M,J,13)+DivOutdoor(M,J,K)
              DivSurf(M,J,K)=DivSurf(M,J,K)+TotOut*OutDoorFac(L,K) + &
                TotInd*QMIP(L,K)
              MunDepSec(L,J,K)=MunDepSec(L,J,K)+TotOut* &
                OutDoorFac(L,K)*(1.0-OutDoorReturn(L,J))+TotInd*QMIP(L,K)
              MunDepSec(L,J,13)=MunDepSec(L,J,13)+TotOut* &
                OutDoorFac(L,K)*(1.0-OutDoorReturn(L,J))+TotInd*QMIP(L,K)* &
                (1.0-IndReturn)
              MunDepSTot(J,K)=MunDepSTot(J,K)+TotOut* &
                OutDoorFac(L,K)*(1.0-OutDoorReturn(L,J))+TotInd*QMIP(L,K)* &
                (1.0-IndReturn)
              DepIndoor(L,J,K)=DepIndoor(L,J,K)+TotInd*QMIP(L,K)* &
                (1.0-IndReturn)
              DepIndoor(L,J,13)=DepIndoor(L,J,13)+TotInd*QMIP(L,K)* &
                (1.0-IndReturn)
              DepOutdoor(L,J,K)=DepOutdoor(L,J,K)+TotOut* &
                OutDoorFac(L,K)*(1.0-OutDoorReturn(L,J))
              DepOutdoor(L,J,13)=DepOutdoor(L,J,13)+TotOut* &
                OutDoorFac(L,K)*(1.0-OutDoorReturn(L,J))
            END DO
            IF (J.EQ.1) THEN
              JJJ=1
            END IF
          END DO
        end if
        !--------------------------------------------------------------------
        !  Read in treatment methods
        !--------------------------------------------------------------------
        call initializeaccessvars()
        Table="Treatment"
        NLOGICFIELD=2
        LOGICFIELDS(1)="WaterResID"
        LOGICFIELDS(2)="TreatmentCode"
        WRITE(TempStr,"(I6)")MunProv(L,M)
        indexstring="WHERE WaterResID=" // trim(AllTrim(TempStr))
        call AccessRead()
        DO J=1,KOUNTFSQL
          TreatmentCode=F90SQLINTEGER(J,2)
        END DO
        DO J=1,NYRS
          RetIndoor(M,J,13)=0.0
          DO K=1,12
            IF (KOUNTFSQL>0) THEN
              SELECT CASE (TreatmentCode)
                CASE (1,2) ! Sewage Treatment Plant OR Facultative Ponds/Lagoons
                  !DepIndoor(L,J,K)=DepIndoor(L,J,K)+RetIndoor(M,J,K)* &
                  !  (1.0-SewageReturn)
                  !DepIndoor(L,J,13)=DepIndoor(L,J,13)+RetIndoor(M,J,K)* &
                  !  (1.0-SewageReturn)
                  RetIndoor(M,J,K)=RetIndoor(M,J,K)*SewageReturn
                CASE (3)
                  !DepIndoor(L,J,K)=DepIndoor(L,J,K)+RetIndoor(M,J,K)* &
                  !  (1.0-SepticReturn)
                  !DepIndoor(L,J,13)=DepIndoor(L,J,13)+RetIndoor(M,J,K)* &
                  !  (1.0-SepticReturn)
                  RetIndoor(M,J,K)=RetIndoor(M,J,K)*SepticReturn
              END SELECT
            ELSE
              !DepIndoor(L,J,K)=DepIndoor(L,J,K)+RetIndoor(M,J,K)* &
              !  (1.0-SepticReturn)
              !DepIndoor(L,J,13)=DepIndoor(L,J,13)+RetIndoor(M,J,K)* &
              !  (1.0-SepticReturn)
              RetIndoor(M,J,K)=RetIndoor(M,J,K)*SepticReturn
            END IF
            RetIndoor(M,J,13)=RetIndoor(M,J,13)+RetIndoor(M,J,K)
          END DO
        END DO
        !--------------------------------------------------------------------
        !  Read in sewage pond values
        !--------------------------------------------------------------------
        call initializeaccessvars()
        Table="ponds"
        NLOGICFIELD=1
        LOGICFIELDS(1)="WaterResID"
        NDPFIELD=2
        Field(1)="pond acreage"
        Field(2)="et"
        WRITE(TempStr,"(I6)")MunProv(L,M)
        indexstring="WHERE WaterResID=" // trim(AllTrim(TempStr))
        call AccessRead()
        pondacres=0.0
        et=0.0
        DO J=1,KOUNTFSQL
          pondacres=XI(J,1)
          et=XI(J,2)
        END DO
        if (DebugIt) then
          WRITE(6,"(10X,'Community Provider ',I5,', ',A/10X," // &
            "'Year,Indoor,Outdoor,RetIndoor,RetOutdoor,DivGW,DivSurf')") &
            MunProv(L,M),trim(AllTrim(MunName(L,M)))
        end if
        Bug=0.0
        DO J=1,NYRS
          RetIndoor(M,J,13)=0.0
          RetOutdoor(M,J,13)=0.0
          DivIndoor(M,J,13)=0.0
          DivOutdoor(M,J,13)=0.0
          DivGW(M,J,13)=0.0
          DivSurf(M,J,13)=0.0
          MunGW(L,J,13)=0.0
          MunReturn(L,J,13)=0.0
          DO K=1,12
            !DepIndoor(L,J,K)=DepIndoor(L,J,K)+MIN(RetIndoor(M,J,K), &
            !  pondacres*et/12.0*OutDoorFac(L,K))
            !DepIndoor(L,J,13)=DepIndoor(L,J,13)+MIN(RetIndoor(M,J,K), &
            !  pondacres*et/12.0*OutDoorFac(L,K))
            Bug(1,K)=Bug(1,K)+MAX(0.0,pondacres*et/12.0*OutDoorFac(L,K)- &
              RetIndoor(M,J,K))/FLOAT(NYRS)
            Bug(1,13)=Bug(1,13)+MAX(0.0,pondacres*et/12.0*OutDoorFac(L,K)- &
              RetIndoor(M,J,K))/FLOAT(NYRS)
            RetIndoor(M,J,K)=MAX(0.0,RetIndoor(M,J,K)-pondacres* &
              et/12.0*OutDoorFac(L,K))
            !Add return flow to municipal area values
            !DepOutdoor(L,J,K)=DepOutdoor(L,J,K)+DivOutdoor(M,J,K) - &
            !  RetOutdoor(M,J,K)
            MunDep(L,J,K)=MunDep(L,J,K)+DivIndoor(M,J,K)-RetIndoor(M,J,K)+ &
              DivOutdoor(M,J,K) - RetOutdoor(M,J,K)
            MunDep(L,J,13)=MunDep(L,J,13)+DivIndoor(M,J,K)-RetIndoor(M,J,K)+ &
              DivOutdoor(M,J,K) - RetOutdoor(M,J,K)
            MunReturn(L,J,K)=MunReturn(L,J,K)+RetIndoor(M,J,K)+RetOutdoor(M,J,K)
            MunReturn(L,J,13)=MunReturn(L,J,13)+RetIndoor(M,J,K)+ &
              RetOutdoor(M,J,K)
            RetIndoor(M,J,13)=RetIndoor(M,J,13)+RetIndoor(M,J,K)
            RetOutdoor(M,J,13)=RetOutdoor(M,J,13)+RetOutDoor(M,J,K)
            DivOutdoor(M,J,13)=DivOutdoor(M,J,13)+DivOutdoor(M,J,K)
            DivIndoor(M,J,13)=DivIndoor(M,J,13)+DivIndoor(M,J,K)
            DivGW(M,J,13)=DivGW(M,J,13)+DivGW(M,J,K)
            DivSurf(M,J,13)=DivSurf(M,J,13)+DivSurf(M,J,K)
            MunGW(L,J,13)=MunGW(L,J,13)+MunGW(L,J,K)
          END DO
          IF (J.EQ.1) THEN
            JJJ=1
          END IF
          IF (DebugIt) THEN
            WRITE(6,"(10X,I4,6F9.1)")(INYR+J-1),DivIndoor(M,J,13), &
              DivOutdoor(M,J,13),RetIndoor(M,J,13),RetOutdoor(M,J,13), &
              DivGW(M,J,13),DivSurf(M,J,13)
          END IF
        END DO
        DO J=1,NYRS
          DO K=1,12
            MunDepPot(L,J,K)=MunDepPot(L,J,K)+MAX(0.0, &
              DivIndoor(M,J,K)-RetIndoor(M,J,K))
            MunDepPot(L,J,13)=MunDepPot(L,J,13)+MAX(0.0, &
              DivIndoor(M,J,K)-RetIndoor(M,J,K))
            DepIndoor(L,J,K)=DepIndoor(L,J,K)+MAX(0.0, &
              DivIndoor(M,J,K)-RetIndoor(M,J,K))
            DepIndoor(L,J,13)=DepIndoor(L,J,13)+MAX(0.0, &
              DivIndoor(M,J,K)-RetIndoor(M,J,K))
          END DO
          if (J.EQ.1) then
            JJJ=1
          end if
        END DO
      END DO
      RetIndoor=0.0
      RetOutdoor=0.0
      DivIndoor=0.0
      DivOutdoor=0.0
      DivGW=0.0
      DivSurf=0.0
      DO M=1,NumNonComm(L)
        !--------------------------------------------------------------------
        !  Read in non-community potable values
        !--------------------------------------------------------------------
        call initializeaccessvars()
        Table="Potable"
        NLOGICFIELD=2
        LOGICFIELDS(1)="WaterResID"
        LOGICFIELDS(2)="Year"
        NDPFIELD=11
        Field(1)="Resin"
        Field(2)="ResOut"
        Field(3)="Com"
        Field(4)="Inst"
        Field(5)="Ind"
        Field(6)="RelSpring"
        Field(7)="RelWells"
        Field(8)="RelSurface"
        Field(9)="MaxSprings"
        Field(10)="MaxWells"
        Field(11)="MaxSurface"
        WRITE(TempStr,"(I6)")NonComm(L,M)
        indexstring="WHERE WaterResID=" // trim(AllTrim(TempStr)) &
          // " ORDER BY Year"
        call AccessRead()
        IF (KOUNTFSQL.EQ.0) THEN
          print *,"No data for ",NonComm(L,M)
          WRITE(6,*)"No data for ",NonComm(L,M)
        END IF
        NumRead=KOUNTFSQL
        IF (NumRead>0) THEN
          DO J=1,KOUNTFSQL
            IND=F90SQLINTEGER(J,2)-INYR+1
            TempArr(J,1)=REAL(IND,KIND=8)
            TempArr(J,2)=XI(J,1) ! ResIn
            TempArr(J,3)=XI(J,2) ! ResOut
            TempArr(J,4)=XI(J,3) ! Com
            TempArr(J,5)=XI(J,4) ! Inst
            TempArr(J,6)=XI(J,5) ! Ind
            TempArr(J,7)=XI(J,6) ! RelSpring
            TempArr(J,8)=XI(J,7) ! RelWells
            TempArr(J,9)=XI(J,8) ! RelSurface
            TempArr(J,10)=XI(J,9) ! MaxSprings
            TempArr(J,11)=XI(J,10) ! MaxWells
            TempArr(J,12)=XI(J,11) ! MaxSurface
          END DO
          !Allocate the data
          Ind1=1
          Ind2=MIN(NumRead,2)
          DO J=1,NYRS
            IF (J.GE.INT(TempArr(Ind2,1))) THEN
              DO
                IF (Ind2.EQ.NumRead.AND.Ind1.EQ.Ind2) THEN
                  EXIT
                END IF
                IF (J.LT.INT(TempArr(Ind2,1))) THEN
                  EXIT
                END IF
                if(Ind2.LT.NumRead) THEN
                  Ind1=Ind2
                  Ind2=Ind2+1
                ELSE
                  Ind2=NumRead
                  Ind1=Ind2
                END IF
              END DO
            END IF
            ! Use reliable supply proportion if there
            GWProp=1.0
            IF (TempArr(Ind1,7)+TempArr(Ind1,8)+TempArr(Ind1,9)>0.0) THEN
              GWprop = (TempArr(Ind1,7)+TempArr(Ind1,8))/(TempArr(Ind1,7)+ &
                TempArr(Ind1,8)+TempArr(Ind1,9))
            ! Else use max supply proportion if there
            ELSE IF(TempArr(Ind1,10)+TempArr(Ind1,11)+TempArr(Ind1,12)>0.0) THEN
              GWprop = (TempArr(Ind1,10)+TempArr(Ind1,11))/(TempArr(Ind1,10) + &
                TempArr(Ind1,11)+TempArr(Ind1,12))
            ! Otherwise use 2005 data if exists otherwise assume all groundwater
            ELSE
              GWprop=1.0
              DO IA=1,NumRead
                IF (TempArr(IA,1)==(2005-INYR+1)) THEN
                  IF (TempArr(IA,6)+TempArr(IA,7)+TempArr(IA,8)>0.0) THEN
                    GWprop = (TempArr(IA,6)+TempArr(IA,7))/ &
                      (TempArr(IA,6)+ TempArr(IA,7)+TempArr(IA,8))
                  ELSE IF (TempArr(IA,9)+TempArr(IA,10)+TempArr(IA,11)>0.0) THEN
                    GWprop = (TempArr(IA,9)+TempArr(IA,10))/ &
                      (TempArr(IA,9)+TempArr(IA,10)+TempArr(IA,11))
                  ELSE
                    GWprop = 1.0
                  END IF
                  EXIT
                END IF
              END DO
            END IF
            DO K=1,12
              NonResIn(L,J,K)=NonResIn(L,J,K)+TempArr(Ind1,2)*QMIP(L,K)
              NonResOut(L,J,K)=NonResOut(L,J,K)+TempArr(Ind1,3)*OutDoorFac(L,K)
              NonCommUse(L,J,K)=NonCommUse(L,J,K)+0.2*TempArr(Ind1,4)* &
                QMIP(L,K)+0.8*0.8*TempArr(Ind1,4)*OutDoorFac(L,K)
              NonInst(L,J,K)=NonInst(L,J,K)+0.2*TempArr(Ind1,5)*QMIP(L,K) + &
                0.8*TempArr(Ind1,5)*OutDoorFac(L,K)
              NonInd(L,J,K)=NonInd(L,J,K)+TempArr(Ind1,6)*QMIP(L,K)
              TotIn=(TempArr(Ind1,2)+0.8*TempArr(Ind1,4)+ &
                0.2*TempArr(Ind1,5)+TempArr(Ind1,6))*QMIP(L,K)
              TotOut=(TempArr(Ind1,3)+0.2*TempArr(Ind1,4) + &
                0.8 * TempArr(Ind1,5))*OutDoorFac(L,K)
              DivIndoor(M,J,K)=DivIndoor(M,J,K)+TotIn
              DivIndoor(M,J,13)=DivIndoor(M,J,13)+TotIn
              DivOutdoor(M,J,K)=DivOutdoor(M,J,K)+TotOut
              DivOutdoor(M,J,13)=DivOutdoor(M,J,13)+TotOut
              MunDemIn(L,J,K)=MunDemIn(L,J,K)+TotIn
              MunDemIn(L,J,13)=MunDemIn(L,J,13)+TotIn
              MunDemOut(L,J,K)=MunDemOut(L,J,K)+TotOut
              MunDemOut(L,J,13)=MunDemOut(L,J,13)+TotOut
              RetIn=(TempArr(Ind1,2)*ResReturn+ 0.8*TempArr(Ind1,4)* &
                ComReturn + 0.2*TempArr(Ind1,5)*InstReturn + TempArr(Ind1,6)* &
                IndReturn)*QMIP(L,K)
              RetOut=TotOut*OutDoorReturn(L,J)
               RetIndoor(M,J,K)=RetIndoor(M,J,K)+RetIn
              RetIndoor(M,J,13)=RetIndoor(M,J,13)+RetIn
              RetOutdoor(M,J,K)=RetOutdoor(M,J,K)+RetOut
              RetOutdoor(M,J,13)=RetOutdoor(M,J,13)+RetOut
              !DepIndoor(L,J,K)=DepIndoor(L,J,K)+TotIn-RetIn
              !DepIndoor(L,J,13)=DepIndoor(L,J,13)+TotIn-RetIn
              DepOutdoor(L,J,K)=DepOutdoor(L,J,K)+TotOut-RetOut
              DepOutdoor(L,J,13)=DepOutdoor(L,J,13)+TotOut-RetOut
              !MunDep(L,J,K)=MunDep(L,J,K)+(TotIn-RetIn)+(TotOut-RetOut)
              MunDepPot(L,J,K)=MunDepPot(L,J,K) + TotOut-RetOut
              MunDepPot(L,J,13)=MunDepPot(L,J,13)+ TotOut-RetOut
              MunGW(L,J,K)=MunGW(L,J,K)+GWProp * (TotIn+TotOut)
              MunGW(L,J,13)=MunGW(L,J,13)+ GWProp*(TotIn+TotOut)
              MunSurf(L,J,K)=MunSurf(L,J,K)+ (TotIn+TotOut)*(1. - GWProp)
              DivGW(M,J,K)=GWProp * (TotIn+TotOut)
              DivSurf(M,J,K)=(TotIn+TotOut)*(1. - GWProp)
            END DO
            IF (J.EQ.1) THEN
              JJJ=1
            END IF
          END DO
        END IF
        !--------------------------------------------------------------------
        !  Read in non-community secondary values
        !--------------------------------------------------------------------
        call initializeaccessvars()
        Table="Secondary2"
        NLOGICFIELD=2
        LOGICFIELDS(1)="WaterResID"
        LOGICFIELDS(2)="Year"
        NDPFIELD=4
        Field(1)="Res"
        Field(2)="Com"
        Field(3)="Inst"
        Field(4)="Ind"
        WRITE(TempStr,"(I6)")NonComm(L,M)
        indexstring="WHERE WaterResID=" // trim(AllTrim(TempStr)) &
          // " ORDER BY [Year]"
        call AccessRead()
        NumRead=KOUNTFSQL
        IF (NumRead>0) THEN
          DO J=1,KOUNTFSQL
            IND=F90SQLINTEGER(J,2)-INYR+1
            TempArr(J,1)=REAL(IND,KIND=8)
            TempArr(J,2)=XI(J,1) ! Res
            TempArr(J,3)=XI(J,2) ! Com
            TempArr(J,4)=XI(J,3) ! Inst
            TempArr(J,5)=XI(J,4) ! Ind
          END DO
          !Allocate the data
          Ind1=1
          Ind2=MIN(NumRead,2)
          DO J=1,NYRS
            IF (J.GE.1.AND.J.LE.NYRS) THEN
              IF (J.GE.INT(TempArr(Ind2,1))) THEN
                DO
                  IF (Ind2.EQ.NumRead.AND.Ind1.EQ.Ind2) THEN
                    EXIT
                  END IF
                  IF (J.LT.INT(TempArr(Ind2,1))) THEN
                    EXIT
                  END IF
                  if(Ind2.LT.NumRead) THEN
                    Ind1=Ind2
                    Ind2=Ind2+1
                  ELSE
                    Ind2=NumRead
                    Ind1=Ind2
                  END IF
                END DO
              END IF
            END IF
            TotFlow=TempArr(Ind1,2)+TempArr(Ind1,3)+TempArr(Ind1,4)+ &
              TempArr(Ind1,5)
            DO K=1,12
              TotOut=(TempArr(Ind1,2)+ TempArr(Ind1,3)+ TempArr(Ind1,4)) &
                * OutDoorFac(L,K)
              TotIn=TempArr(Ind1,5)*QMIP(L,K)
              MunDemOut(L,J,K)=MunDemOut(L,J,K)+TotOut
              MunDemOut(L,J,13)=MunDemOut(L,J,13)+TotOut
              RetOut=TotOut * OutDoorFac(L,K) * OutDoorReturn(L,J)
              ResOutUse(L,J,K)=ResOutUse(L,J,K)+TempArr(Ind1,2)* OutDoorFac(L,K)
              DivIndoor(M,J,K)=DivIndoor(M,J,K)+TotIn
              MunDemIn(L,IND,K)=MunDemIn(L,IND,K)+TotIn
              MunDemIn(L,IND,13)=MunDemIn(L,IND,13)+TotIn
              RetIn=TotIn*(1.0-IndReturn)
              RetIndoor(M,IND,K)=RetIndoor(M,IND,K)+RetIn
              RetIndoor(M,IND,13)=RetIndoor(M,IND,13)+RetIn
              CommUse(L,J,K)=CommUse(L,J,K)+TempArr(Ind1,3)*OutDoorFac(L,K)
              InstUse(L,J,K)=InstUse(L,J,K)+TempArr(Ind1,4)*OutDoorFac(L,K)
              IndUse(L,J,K)=IndUse(L,J,K)+TempArr(Ind1,5)*OutDoorFac(L,K)
              DivOutdoor(M,J,K)=DivOutdoor(M,J,K)+TotOut
              DivOutdoor(M,J,13)=DivOutdoor(M,J,13)+TotOut
              DivIndoor(M,J,K)=DivIndoor(M,J,K)+TotIn
              DivIndoor(M,J,13)=DivIndoor(M,J,13)+TotIn
              RetOutdoor(M,J,K)=RetOutdoor(M,J,K)+TotOut*OutDoorReturn(L,J)
              RetOutdoor(M,J,13)=RetOutdoor(M,J,13)+TotOut*OutDoorReturn(L,J)
              MunSurf(L,J,K)=MunSurf(L,J,K)+TotOut+TotIn
              MunSurf(L,J,13)=MunSurf(L,J,13)+TotOut+TotIn
              MunDepSec(L,J,K)=MunDepSec(L,J,K)+TotOut * (1.0-OutDoorReturn(L,J))
              MunDepSec(L,J,13)=MunDepSec(L,J,13)+TotOut * (1.0-OutDoorReturn(L,J))
              Secondary(L,J,K)=Secondary(L,J,K)+TotIn+TotOut
              Secondary(L,J,13)=Secondary(L,J,13)+TotIn+TotOut
              DivSurf(M,J,K)=DivSurf(M,J,K)+TotIn+TotOut
              DepOutdoor(L,J,K)=DepOutdoor(L,J,K)+TotOut-RetOut
              DepOutdoor(L,J,13)=DepOutdoor(L,J,13)+TotOut-RetOut
              DepIndoor(L,J,K)=DepIndoor(L,J,K)+TotIn-RetIn
              DepIndoor(L,J,13)=DepIndoor(L,J,13)+TotIn-RetIn
            END DO
            IF (J.EQ.1) THEN
              JJJ=1
            END IF
          ENDDO
        ENDIF
        !--------------------------------------------------------------------
        !  Read in treatment methods
        !--------------------------------------------------------------------
        call initializeaccessvars()
        Table="Treatment"
        NLOGICFIELD=2
        LOGICFIELDS(1)="WaterResID"
        LOGICFIELDS(2)="TreatmentCode"
        WRITE(TempStr,"(I6)")NonComm(L,M)
        indexstring="WHERE WaterResID=" // trim(AllTrim(TempStr))
        call AccessRead()
        DO J=1,KOUNTFSQL
          TreatmentCode=F90SQLINTEGER(J,2)
        END DO
        DO J=1,NYRS
          DO K=1,12
            IF (KOUNTFSQL>0) THEN
              SELECT CASE (TreatmentCode)
                CASE (1,2) ! Sewage Treatment Plant OR Facultative Ponds/Lagoons
                  !DepIndoor(L,J,K)=DepIndoor(L,J,K)+RetIndoor(M,J,K)*(1.0 - &
                  !  SewageReturn)
                  !DepIndoor(L,J,13)=DepIndoor(L,J,13)+RetIndoor(M,J,K)*(1.0 - &
                  !  SewageReturn)
                  RetIndoor(M,J,K)=RetIndoor(M,J,K)*SewageReturn
                CASE (3)
                  !DepIndoor(L,J,K)=DepIndoor(L,J,K)+RetIndoor(M,J,K)*(1.0 - &
                  !  SepticReturn)
                  !DepIndoor(L,J,13)=DepIndoor(L,J,13)+RetIndoor(M,J,K)*(1.0 - &
                  !  SepticReturn)
                  RetIndoor(M,J,K)=RetIndoor(M,J,K)*SepticReturn
              END SELECT
            ELSE
              !DepIndoor(L,J,K)=DepIndoor(L,J,K)+RetIndoor(M,J,K)*(1.0 - &
              !  SepticReturn)
              !DepIndoor(L,J,13)=DepIndoor(L,J,13)+RetIndoor(M,J,K)*(1.0 - &
              !  SepticReturn)
              RetIndoor(M,J,K)=RetIndoor(M,J,K)*SepticReturn
            END IF
          END DO
        END DO
        !--------------------------------------------------------------------
        !  Read in sewage pond values
        !--------------------------------------------------------------------
        call initializeaccessvars()
        Table="ponds"
        NLOGICFIELD=1
        LOGICFIELDS(1)="WaterResID"
        NDPFIELD=2
        Field(1)="pond acreage"
        Field(2)="et"
        WRITE(TempStr,"(I6)")NonComm(L,M)
        indexstring="WHERE WaterResID=" // trim(AllTrim(TempStr))
        call AccessRead()
        pondacres=0.0
        et=0.0
        DO J=1,KOUNTFSQL
          pondacres=XI(J,1)
          et=XI(J,2)
        END DO
        if (DebugIt) then
          WRITE(6,"(10X,'Non-Community Provider ',I6,', ',A/10X," // &
            "'Year,Indoor,Outdoor,RetIndoor,RetOutdoor,DivGW,DivSurf')") &
            NonComm(L,M),trim(AllTrim(NonName(L,M)))
        end if
        DO J=1,NYRS
          DivOutdoor(M,J,13)=0.0
          DivIndoor(M,J,13)=0.0
          RetIndoor(M,J,13)=0.0
          RetOutdoor(M,J,13)=0.0
          DivGW(M,J,13)=0.0
          DivSurf(M,J,13)=0.0
          DO K=1,12
            !DepIndoor(L,J,K)=DepIndoor(L,J,K)+ MIN(pondacres*et/12.0* &
            !  OutDoorFac(L,K),RetIndoor(M,J,K))
            !DepIndoor(L,J,13)=DepIndoor(L,J,13)+MIN(pondacres*et/12.0* &
            !  OutDoorFac(L,K),RetIndoor(M,J,K))
            RetIndoor(M,J,K)=MAX(0.0,RetIndoor(M,J,K)-pondacres* &
              et/12.0*QMIP(L,K))
            !Add return flow to municipal area values
            !DepOutDoor(L,J,K)=DepOutdoor(L,J,K)+DivOutdoor(M,J,K)- &
            !  RetOutdoor(M,J,K)
            MunDep(L,J,K)=MunDep(L,J,K) + DivIndoor(M,J,K)- RetIndoor(M,J,K) + &
              DivOutdoor(M,J,K)- RetOutdoor(M,J,K)
            MunDep(L,J,13)=MunDep(L,J,13)+DivIndoor(M,J,K)- RetIndoor(M,J,K) + &
              DivOutdoor(M,J,K)- RetOutdoor(M,J,K)
            MunReturn(L,J,K)=MunReturn(L,J,K)+RetIndoor(M,J,K)+RetOutdoor(M,J,K)
            DivOutdoor(M,J,13)=DivOutdoor(M,J,13)+DivOutdoor(M,J,K)
            DivIndoor(M,J,13)=DivIndoor(M,J,13)+DivIndoor(M,J,K)
            RetIndoor(M,J,13)=RetIndoor(M,J,13)+RetIndoor(M,J,K)
            RetOutdoor(M,J,13)=RetOutdoor(M,J,13)+RetOutdoor(M,J,K)
            DivGW(M,J,13)=DivGW(M,J,13)+DivGW(M,J,K)
            DivSurf(M,J,13)=DivSurf(M,J,13)+DivSurf(M,J,K)
          END DO
          IF (J.EQ.1) THEN
            JJJ=1
          END IF
          IF (DebugIt) THEN
            WRITE(6,"(10X,I4,6F9.1)")(INYR+J-1),DivIndoor(M,J,13), &
              DivOutdoor(M,J,13),RetIndoor(M,J,13),RetOutdoor(M,J,13), &
              DivGW(M,J,13),DivSurf(M,J,13)
          END IF
        END DO
        DO J=1,NYRS
          DO K=1,12
            DepIndoor(L,J,K)=DepIndoor(L,J,K)+ MAX(0.0,DivIndoor(M,J,K) - &
              RetIndoor(M,J,K))
            DepIndoor(L,J,13)=DepIndoor(L,J,13)+ MAX(0.0,DivIndoor(M,J,K) - &
              RetIndoor(M,J,K))
            MunDepPot(L,J,K)=MunDepPot(L,J,K)+ MAX(0.0,DivIndoor(M,J,K) - &
              RetIndoor(M,J,K))
            MunDepPot(L,J,13)=MunDepPot(L,J,13)+ MAX(0.0,DivIndoor(M,J,K) - &
              RetIndoor(M,J,K))
          END DO
          IF (J.EQ.1) THEN
            JJJ=1
          END IF
        END DO
      END DO
      DO J=1,NYRS
        MunDep(L,J,13)=0.0
        DO K=1,12
          MunDep(L,J,K)=MunGW(L,J,K)+MunSurf(L,J,K)-MunReturn(L,J,K)
          MunDep(L,J,13)=MunDep(L,J,13)+MunDep(L,J,K)
        END DO
      END DO
      IF (MunGWQx(L)>0)THEN
        DO J=1,NYRS
          DO K=1,12
            QX(MunGWQx(L),J,K)=MunGW(L,J,K)
          END DO
        END DO
      END IF
    END DO
  end if
else
  NMUN=0
end if
!----------------------------------------------------------------------------
!MINIMUM FLOW DATA
!----------------------------------------------------------------------------
!Read in ModelMunGroup table - Indoor factors
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelMinFlow"
indexstring=ModelFilter
NDPFIELD=12
Field(1)="Jan"
Field(2)="Feb"
Field(3)="Mar"
Field(4)="Apr"
Field(5)="May"
Field(6)="Jun"
Field(7)="Jul"
Field(8)="Aug"
Field(9)="Sep"
Field(10)="Oct"
Field(11)="Nov"
Field(12)="Dec"
NLOGICFIELD=3
LOGICFIELDS(1)="QX"
LOGICFIELDS(2)="UpstreamRes"
LOGICFIELDS(3)="ID"
NSTRINGFIELD=1
STRINGFIELDS(1)="DamID"
call AccessRead()
WillOpen = .FALSE.
NQXMN=KOUNTFSQL
!      DO I=1,NQXMN
!         READ(5,'(I3,9F8.1/3F8.1,F10.1)')IQNUM(I), &
!                 (QXMN(K,I),K=1,13)
!         READ(5,'(I3,20I3)')IQRES(I),(IQXMN(J,I),J=1,20)
!      ENDDO
DO L=1,KOUNTFSQL
  QXMN(13,L)=0.0
  do K=1,12
    QXMN(K,L)=XI(L,K)
    QXMN(13,L)=QXMN(13,L)+XI(L,K)
  end do
  QXMN(13,L)=QXMN(13,L)/12.0
  IQNUM(L)=F90SQLINTEGER(L,1)
  IQXMIN(IQNUM(L))=L
  IF (IQNUM(L).GT.0) THEN
    DO J=1,NYRS
      DO K=1,13
        FBYPS(IQNUM(L),J,K)=QXMN(K,L)
      END DO
    END DO
  END IF
  ITEMP=F90SQLINTEGER(L,2)
  IF (NRES>0.AND.ITEMP>0) THEN
    DO M=1,NRES
      IF (ReserID(M)==ITEMP) THEN
        IQRES(L)=M
        EXIT
      END IF
    END DO
  END IF
  QXmnID(L)=F90SQLINTEGER(J,1)
END DO

!----------------------------------------------------------------------------
!MINIMUM FLOW UPSTREAM QX DATA
!----------------------------------------------------------------------------
!Read in ModelMinQXup table
!----------------------------------------------------------------------------
if (NQXMN>0) then
  DO L=1,NQXMN
    IF(IQRES(L)>0) THEN
      call initializeaccessvars()
      WRITE(TempStr,"(I6)")QXmnID(L)
      Table="ModelMinQXup"
      indexstring=ModelFilter // " AND QX=" // trim(ALLTRIM(TempStr))
      call AccessRead()
      NumMinQXup(L)=KOUNTFSQL
      DO M=1,NumMinQXup(L)
        IQXMN(M,L)=F90SQLINTEGER(M,1)
      END DO
    END IF
  END DO
end if
!----------------------------------------------------------------------------
!Inflow and Outflow data to define yield
!----------------------------------------------------------------------------
!MainStem inflows
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelMainstem"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="MainStemQX"
call AccessRead()
NumMainstem = KOUNTFSQL
NINFL=KOUNTFSQL
DO J=1,KOUNTFSQL
  IINFL(J)=F90SQLINTEGER(J,1)
END DO
!----------------------------------------------------------------------------
!Tributary inflows
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelTributary"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="TributaryQX"
call AccessRead()
NumTributary = KOUNTFSQL
NTRIB=KOUNTFSQL
DO J=1,KOUNTFSQL
  ITRIB(J)=F90SQLINTEGER(J,1)
END DO
!----------------------------------------------------------------------------
!Imports
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelImport"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="ImportQX"
call AccessRead()
NumImport = KOUNTFSQL
NIMP=KOUNTFSQL
DO J=1,KOUNTFSQL
  IIMP(J)=F90SQLINTEGER(J,1)
END DO
!----------------------------------------------------------------------------
!Exports
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelExport"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="ExportQX"
call AccessRead()
NumExport = KOUNTFSQL
NEXPO=KOUNTFSQL
DO J=1,KOUNTFSQL
  IEXPO(J)=ABS(F90SQLINTEGER(J,1))
END DO
!----------------------------------------------------------------------------
!Basin Inflows
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelBasinIn"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="QXin"
call AccessRead()
NUMBASIN=KOUNTFSQL
DO J=1,KOUNTFSQL
  IBASIN(J)=ABS(F90SQLINTEGER(J,1))
END DO
!----------------------------------------------------------------------------
!Basin Outflows
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelBasinOut"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="QXout"
call AccessRead()
NUMBASOUT=KOUNTFSQL
DO J=1,KOUNTFSQL
  IBASOUT(J)=ABS(F90SQLINTEGER(J,1))
END DO
!----------------------------------------------------------------------------
!Ungauged
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelUngauged"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="UngaugedQX"
call AccessRead()
NumUngauged = KOUNTFSQL
NUNG=KOUNTFSQL
DO J=1,KOUNTFSQL
  IUNG(J)=F90SQLINTEGER(J,1)
END DO
!----------------------------------------------------------------------------
!Outflow
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelOutflow"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="OutflowQX"
call AccessRead()
NumOutflow = KOUNTFSQL
DO J=1,KOUNTFSQL
  NOUFL(J)=F90SQLINTEGER(J,1)
END DO
!----------------------------------------------------------------------------
!Outflow to Evap
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelOutEvap"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="OutEvapQX"
call AccessRead()
NumOutEvap = KOUNTFSQL
DO J=1,KOUNTFSQL
  NOUEVFL(J)=F90SQLINTEGER(J,1)
END DO
!----------------------------------------------------------------------------
!Transbasin Exports and Imports
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelTransbasin"
indexstring=ModelFilter
NLOGICFIELD=1
LOGICFIELDS(1)="QX"
NSTRINGFIELD=3
STRINGFIELDS(1)="InOut"
STRINGFIELDS(2)="OtherBasinNum"
STRINGFIELDS(3)="OtherBasin"
call AccessRead()
TranNum = KOUNTFSQL
DO J=1,KOUNTFSQL
  TranQX(J)=F90SQLINTEGER(J,1)
  TranDir(J)=AllTrim(F90SQLSTRINGS(J,1))
  TranBasin(J)=AllTrim(F90SQLSTRINGS(J,2))
  TranName(J)=AllTrim(F90SQLSTRINGS(J,3))
END DO
!----------------------------------------------------------------------------
!Read in Output Files
!----------------------------------------------------------------------------
call initializeaccessvars()
Table="ModelOutputs"
indexstring=ModelFilter
NLOGICFIELD=6
LOGICFIELDS(1)="OutNumber"
LOGICFIELDS(2)="Type"
LOGICFIELDS(3)="ResSto"
LOGICFIELDS(4)="ResArea"
LOGICFIELDS(5)="ResElev"
LOGICFIELDS(6)="ResEvap"
NSTRINGFIELD=1
STRINGFIELDS(1)="FileName"
call AccessRead()
NOUTFL=KOUNTFSQL
if (NOUTFL.GT.0) then
  IDISK=.TRUE.
ELSE
  IDISK=.FALSE.
end if
DO J=1,KOUNTFSQL
  OutNumber(J)=F90SQLINTEGER(J,1)
  OutType(J)=F90SQLINTEGER(J,2)
  OutResSto(J)=F90SQLINTEGER(J,3)
  OutResArea(J)=F90SQLINTEGER(J,4)
  OutResElev(J)=F90SQLINTEGER(J,5)
  OutResEvap(J)=F90SQLINTEGER(J,6)
  OutPutFile(J)=F90SQLSTRINGS(J,1)
END DO
!----------------------------------------------------------------------------
!Read in Calibration options
!----------------------------------------------------------------------------
Calibrate=.FALSE.
WillClose=.TRUE.
call initializeaccessvars()
Table="ModelCalibrate"
Indexstring=ModelFilter
NLOGICFIELD=3
LOGICFIELDS(1)="TypeCalib"
LOGICFIELDS(2)="Area"
LOGICFIELDS(3)="Calibrate"
NDPFIELD=3
FIELD(1)="MaxCal"
FIELD(2)="MinCal"
FIELD(3)="InitialCal"
NSTRINGFIELD=1
STRINGFIELDS(1)="Description"
call AccessRead()
DO J=1,KOUNTFSQL
  TypeCalib=F90SQLINTEGER(J,1)
  CalibArea=F90SQLINTEGER(J,2)
  IF (F90SQLINTEGER(J,3).NE.0)THEN
    Calibrate=.TRUE.
  ELSE
    Calibrate=.FALSE.
  END IF
  MaxCal=XI(J,1)
  MinCal=XI(J,2)
  InitialCal=XI(J,3)
END DO
!----------------------------------------------------------------------------
!Output files of Budget results
!----------------------------------------------------------------------------

!      READ(5,'(I3)')NOUFL
!      READ(5,'(19I3)')(IINFL(I),I=1,19)
!      READ(5,'(19I3)')(ITRIB(I),I=1,19)
!      READ(5,'(19I3)')( IIMP(I),I=1,19)
!      READ(5,'(19I3)')(IEXPO(I),I=1,19)
!      READ(5,'(19I3)')( IUNG(I),I=1,19)

!IF (IMNPLT) THEN
!  call initializeaccessvars()
!  Table="ModelMonQXPlt"
!  NLOGICFIELD=1
!  LOGICFIELDS(1)="QX"
!      indexstring=ModelFilter // " AND QX=" // trim(ALLTRIM(TempStr))
!  call AccessRead()
!  NmonQXPlt=KOUNTFSQL
!  DO M=1,NmonQXPlt
!    ISQX(M)=F90SQLINTEGER(M,1)
!  END DO
!END IF
!      READ(5,"(7I5,11L1,I3,I5,I3,I2,L1)")NYRS, &
!            INYR,NRES,NLND,NQIN,NHPW,NFDT, &
!            IPSH,IPST,IPEV,IPEL,IPSA,IPAS, &
!            IANPLT,IMNPLT, IDISK,IPQIN,IBUD,NQXMN,ITERMX,NQX, &
!            INEW,SPLOT
      DO WHILE(IANPLT.AND.PLOTFL.LE.' ')
      WRITE(*,*)'Please Input Plot File Name'
      WRITE(*,*)' '
      READ(*,*)PLOTFL
      ENDDO
      DO WHILE(IMNPLT.AND.PLOTFL2.LE.' ')
      WRITE(*,*)'Please Input Plot File Name'
      WRITE(*,*)' '
      READ(*,*)PLOTFL2
      ENDDO
      NYRS=MAX(1,NYRS)
      INYR=MAX(1,INYR)
!
      IF(NRES .GT. MRES .OR. NLND .GT. MLAND .OR. NQX .GT. MQX .OR. &
        NYRS .GT. MYEAR .OR. NHPW .GT. MPP .OR. NQIN .GT. MQSTA)THEN
      IF(NRES .GT. MRES)THEN
          WRITE(0,701)
  701       FORMAT(' INCREASE FORTRAN DIMENSIONS', &
                  ' FOR NUMBER OF RESERVOIRS')
      ENDIF
      IF(NLND .GT. MLAND)THEN
          WRITE(0,702)
  702       FORMAT(' INCREASE FORTRAN DIMENSIONS FOR', &
                  ' NUMBER OF LAND AREAS')
      ENDIF
      IF(NQX .GT. MQX)THEN
          WRITE(0,703)
  703       FORMAT(' INCREASE FORTRAN DIMENSIONS FOR', &
                  ' NUMBER OF QX''S')
      ENDIF
      IF(NYRS .GT. MYEAR)THEN
          WRITE(0,704)
  704       FORMAT(' INCREASE FORTRAN DIMENSIONS FOR', &
                  ' NUMBER OF YEARS')
      ENDIF
      IF(NHPW .GT. MPP)THEN
          WRITE(0,705)
  705       FORMAT(' INCREASE FORTRAN DIMENSIONS FOR', &
                  ' NUMBER OF HYDRO STA.')
      ENDIF
      IF(NQIN .GT. MQSTA)THEN
          WRITE(0,706)
  706       FORMAT(' INCREASE FORTRAN DIMENSIONS FOR', &
                  ' NUMBER OF STREAMFLOW STATIONS')
      ENDIF
      WRITE(0,707)
  707   FORMAT(/' PRESS INTER TO STOP')
      READ(0,*)
      STOP
      ENDIF
!
!   VARIABLES:
!              NYRS - NUMBER OF YEARS
!              INYR - INITIAL YEAR
!              NRES - NUMBER OF RESERVOIRS
!              NLND - NUMBER OF LAND AREAS
!              NHPW - NUMBER OF HYD POWR S
!              NFDT - NUMBER OF FLOW DUR QS
!              NQX  - NUMBER OF QX'S
!   LOGIC VARIABLES SET EQUAL TO 1 TO:
!              IPQN - USE RESRVR TARGET VAL
!              IPSH - PRINT LAND AREA MONTH SHORTGS
!              IPST - PRINT EOM STORAGE
!              IPEV - PRINT MONTH EVAP
!              IPEL - PRINT EOM ELEVATIONS
!              IPSA - PRINT EOM SURF AREA
!              IPAS - PRINT ANNUAL LAND AREA SHORTAGES
!              IQMN - MINIMUM FLOW IN RIVER
!              IANPLT - CREATE ANNUAL PLOT
!              IMNPLT - CREATE MONTHLY PLOT
!              IQMNL - LIMIT MIN REL TO INF
!              IDISK - SAVE QX'S ON DISK
!              IQIFLV - READ QIN NAMES FROM DISK
!              IPQIN - WRITE OUT QIN'S
!
!-----------------------------------------------------------------------
!
!     VARIABLES USED IN THE INITIALIZATION SECTION:
!
!     IYEAR   - ARRAY CONTAINING THE YEARS OF THE SIMULATION
!     FNYEARS - NUMBER OF YEARS IN THE SIMULATION (NYRS FLOATED)
!     STO     - ARRAY WITH THE INITIAL STORAGE IN THE RESERVOIRS
!     REL     - ARRAY WITH THE INITIAL RESERVOIR ELEVATIONS
!     RAR     - ARRAY WITH THE INITIAL RESERVOIR SURFACE AREAS
!     H1I     - ARRAY WITH INITIAL HEAD (ELEVATION-TAIL WATER ELEVATION)
!     EF      - ARRAY WITH PERCENT OF DIVERTED IRRIGATION WATER
!               RETURNING TO THE STREAM
!     EFM     - ARRAY WITH RETURN PERCENTAGE OF M AMD I WATER
!     QDVRA   - ARRAY WITH VOLUME OF WATER NEEDED AT DIVERSION TO LAND
!               AREAS BY MONTH
!     QDMI    - ARRAY WITH M AND I DEMAND BY MONTH IN THE LAND AREAS
!     QDVR    - ARRAY WITH TOTAL DIVERSION REQUIREMENT FOR THE LAND
!               AREAS (QDVRA+QDMI)
!
!-----------------------------------------------------------------------
!
!
!        READ DATE AND VARIABLES
!
!      CALL DATE(DATE2)
      DATE2=' '
      CALL DATE_AND_TIME(DATE9,TIME9)
      READ(DATE9,"(I4,I2,I2)")RptYear,RptMonth,RptDay
      DATE9=DATE9(5:6)//'-'//DATE9(7:8)//'-'//DATE9(1:4)
      TIME9=TIME9(1:2)//':'//TIME9(3:4)//':'//TIME9(5:6)
      WRITE(DATE2,'(A,2X,A)') TRIM(DATE9),TRIM(TIME9)
ICHK=0
DO M=1,NRES
     IRSCK=0
  if (NRES>0) then
         DO I=1,NQTG(M)
            IF(IQTG(M,I).GT.0)CALL IOU(IQTG(M,I),2,M)
         ENDDO
         DO I=1,NQTG(M)
            IF(IQTG(M,I).GT.0)THEN
               IQTGNM=IQTG(M,I)
               IF(ICHK(IQTGNM).GT.0)THEN
                 WRITE(6,*)'CANNOT ASSIGN QIN(',ICHK(IQTGNM),') TO ', &
                 'QX(',IQTGNM,') BECAUSE IT IS A RELEASE ', &
                 'QX FOR RESERVOIR ',M,' - IQXN ZEROED OUT'
                 WRITE(0,*)'CANNOT ASSIGN QIN(',ICHK(IQTGNM),') TO ', &
                 'QX(',IQTGNM,') BECAUSE IT IS A RELEASE ', &
                 'QX FOR RESERVOIR ',M,' - IQXN ZEROED OUT'
                 IQXN(ICHK(IQTGNM))=0
                 IRSCK(IQTGNM)=M
               ENDIF
            ENDIF
         ENDDO
         DO I=1,19
            IF(IQXAD(M,I).GT.0)CALL INQ(IQXAD(M,I),2,M)
         ENDDO
  end if
END DO
    DO L=1,NLND
       IF(IPH(L).GT.0.0)THEN
          IF(IPHU(L).LE.0)THEN
             WRITE(6,*)'Warning, upstream QX to serve wetlands in area ',L, &
                  ' not specified'
             WRITE(0,*)'Warning, upstream QX to serve wetlands in area ',L, &
                  ' not specified.'
             !ISTOP=.TRUE.
          ENDIF
          IF(IPHD(L).LE.0)THEN
             WRITE(6,*)'Warning, downstream QX to serve wetlands in area ',L, &
                  ' not specified.'
             WRITE(0,*)'Warning, downstream QX to serve wetlands in area ',L, &
                  ' not specified.'
             !ISTOP=.TRUE.
          ENDIF
       ENDIF
       IF(IRT(L).LE.0)THEN
          WRITE(6,*)'Return Flow QX for Land Area ',L,' is 0 or less'
          WRITE(0,*)'Return Flow QX for Land Area ',L,' is 0 or less'
          ISTOP=.TRUE.
       ENDIF
       IF(IRV(L).GT.0.AND.IFT(L).LE.0)THEN
          WRITE(6,*)'Bypass for irrigated wet pasture, Area ',L, &
               ' is 0 or less'
          WRITE(0,*)'Bypass for irrigated wet pasture, Area ',L, &
               ' is 0 or less'
          ISTOP=.TRUE.
       ENDIF
       IF(IRV(L).GT.0.AND.IUR(L).LE.0)THEN
          WRITE(6,*)'Upstream QX for irrigated wet pasture, Area ',L, &
               ' is 0 or less'
          WRITE(0,*)'Upstream QX for irrigated wet pasture, Area ',L, &
               ' is 0 or less'
          ISTOP=.TRUE.
       ENDIF
       CALL IOU(IRT(L),1,L)
       IF(IUR(L).GT.0)CALL INQ(IUR(L),1,L)
       IF(IRV(L).GT.0)CALL CGW(IRV(L),1,L)
       IF(IFT(L).GT.0)CALL CBY(IFT(L),1,L)
       DO I=1,NQLIN(L)
          IF(IDV(L,I).LE.0)THEN
             WRITE(6,*)'Diversion ',I,' for Land Area',L, &
                     ' is 0 or less'
             WRITE(0,*)'Diversion ',I,' for Land Area',L, &
                     ' is 0 or less'
             ISTOP=.TRUE.
          ENDIF
          CALL INQ(IDV(L,I),1,L)
          IF(IBY(L,I).GT.0)CALL CBY(IBY(L,I),1,L)
!~~Figure out why this is not working correctly
          IF(ILUP(L,I).GT.0)CALL UPC(ILUP(L,I),1,L)
          IF(ILQIN(L,I).GT.0)THEN
             IXQX=IQXN(ILQIN(L,I))
             IF(IXQX.GT.0)THEN
                 IF(IDV(L,I).NE.IXQX)THEN
                  WRITE(6,*)'QIN(',ILQIN(L,I), &
                              ') is assigned to ', &
                              'diversion ',I,' of land area ',L, &
                              ' and cannot ', &
                        'be assinged QX ',IXQX
                  WRITE(0,*)'QIN(',ILQIN(L,I), &
                              ') is assigned to ', &
                              'diversion ',I,' of land area ',L, &
                              ' and cannot ', &
                        'be assinged QX ',IXQX
                  ISTOP=.TRUE.
                 ENDIF
              ENDIF
             ENDIF
          IF(IDV(L,I).GT.0)THEN
            IF (NRES.GT.0) THEN
             IF(IRSCK(IDV(L,I)).GT.0)THEN
              WRITE(6,*)'Diversion ',I,' for Land Area',L, &
                        ' is an outlet' &
                     ,' QX for reservoir ',IRSCK(IDV(L,I))
              WRITE(0,*)'Diversion ',I,' for Land Area', &
                        L,' is an outlet' &
                     ,' QX for reservoir ',IRSCK(IDV(L,I))
              ISTOP=.TRUE.
             ENDIF
            END IF
             IF(ILUP(L,I).LE.0)THEN
              IF(ILQIN(L,I).GT.0)THEN
                 WRITE(6,*)'Warning, No upstream QX', &
                           ' number for diversion ' &
                           ,I,' of land area ',L
                 WRITE(0,*)'Warning, No upstream QX ', &
                           'number for diversion ' &
                           ,I,' of land area ',L
              ELSE
                 WRITE(6,*)'Warning, No upstream QX ', &
                           'number for diversion ' &
                           ,I,' of land area ',L, &
                           ', Not defined by QIN'
                 WRITE(0,*)'Warning, No upstream QX ', &
                           'number for diversion ' &
                           ,I,' of land area ',L, &
                           ', Not defined by QIN'
              ENDIF
             ENDIF
          ENDIF
       ENDDO
       DO K=2,12
          IGRND(L,K)=IGRND(L,1)
       ENDDO
       IF(LNRS(L).GT.0)THEN
              DO MM=1,LNRS(L)
                  DO M=1,20
                        IF(ILNQ(L,MM,M).GT.0)CALL RCK(ILNQ(L,MM,M),2, &
                            LRESROW(L,MM))
                  ENDDO
                  ISDIV=.FALSE.
                  DO M=1,20
                        IF(ILNQ(L,MM,M).GT.0)THEN
                            INQX=ILNQ(L,MM,M)
                            INLND=ITYOU(INQX)
                            INDIV=ICKOU(INQX)
                            IF(INLND.EQ.2.AND.INDIV.EQ.LRESROW(L,MM))THEN
                                  ISDIV=.TRUE.
!-----------------------------------------------------------------------
!     NDEML - ARRAY OF VALUES HOLDING THE QX NUMBER OF THE DIVERSION
!     INTO LAND AREA L FROM RESERVOIR ICKRS(INQX)
!-----------------------------------------------------------------------
                                  NDEML(L,LRESROW(L,MM))=INQX
!-----------------------------------------------------------------------
!     LMRES - ARRAY OF VALUES HOLDING THE SUBCRIPT NUMBER OF THE
!     RESERVOIR ICKRS(INQX)
!-----------------------------------------------------------------------
                                  LMRES(L,LRESROW(L,MM))=MM
                            ENDIF
                          ENDIF
                  ENDDO
                ENDDO
          ENDIF
      enddo
   22 CONTINUE
!-----------------------------------------------------------------------
!        PRINT INPUT DATA
!-----------------------------------------------------------------------
      WRITE(6,"(//10X,'DATA FILE : ',A)")Trim(AllTrim(TITLE))
      WRITE(6,"(10X,  'MODEL NO. : ',I3)")ModelID
      LINE=LINE+4
!-----------------------------------------------------------------------
!     PRINT OUT RESERVOIR AND LAND USE DATA
!-----------------------------------------------------------------------
      IF(NRES.GT.0)THEN
      CALL HEADIN
      WRITE (6,"(/30X,'RESERVOIR SIMULATION INPUT DATA')")
      CLOSE(UNIT=5)
  202 FORMAT (6X,A4,12(4X,A4,1X),5X,A4,/)
   60 WRITE (6,211)
  211 FORMAT (////,30X,'RESERVOIR PARAMETERS')
      LINE = LINE + 10
      DO 62 M=1,NRES
         IF(LINE + 3*NP(M)/10 + 17 .GT.  LinesPerPage) CALL HEADIN
         WRITE (6,"(//,10X,A32)") trim(AllTrim(PRESV(M)))
         WRITE (6,"(10X,'NP,SMX ...',I10,3F10.0)") &
               NP(M),SMX(M),SMN(M),STOIC(M)
         AVRESET(M,13)=0.0
         DO K=1,12
          AVRESET(M,K)=0.0
          DO J=1,NYRS
            AVRESET(M,K)=AVRESET(M,K)+EVRT(M,J,K)
            AVRESET(M,13)=AVRESET(M,13)+EVRT(M,J,K)
          END DO
          AVRESET(M,K)=AVRESET(M,K)/FLOAT(NYRS)
         END DO
         AVRESET(M,13)=AVRESET(M,13)/FLOAT(NYRS)
         WRITE (6,"(10X,'EVRT WY   ',12F8.2,F10.2)") &
               (AVRESET(M,K),K=1,13)
         WRITE (6,"(10X,'SEEPAGE   ',12F8.0,F10.0)") &
               (QSM(M,K),K=1,13)
         WRITE (6,215) (QRMN(M,K),K=1,13)
         LINE = LINE + 7
  215    FORMAT (10X,'QRMN WY   ',12F8.0,F10.0)
         WRITE (6,333)
  333    FORMAT (/,10X,'ELEVATION-AREA-CAPACITY TABLE')
         WRITE (6,334) (E(M,N),N=1,NP(M))
         IF(LINE .GT.  2*LinesPerPage/3) THEN
            CALL HEADIN
         END IF
         WRITE (6,334) (A(M,N),N=1,NP(M))
         IF(LINE .GT.  2*LinesPerPage/3) CALL HEADIN
         WRITE (6,334) (V(M,N),N=1,NP(M))
         IF(LINE .GT.  2*LinesPerPage/3) CALL HEADIN
  334    FORMAT (10X,10F11.1)
         LINE = LINE + 3*NP(M)/10 + 5
         IF (IPQN(M)) THEN
            WRITE (6,229) (QTG(M,K),K=1,13)
 229           FORMAT (10X,'QTG  WY   ',12F8.0,F10.0)
            LINE=LINE+1
         ENDIF
         WRITE(6,"(/12X,'RELEASE WATER FROM RESERVOIR THROUGH"// &
         " QX NUMBERS',19I3)")(IQTG(M,I),I=1,NQTG(M))
         LINE=LINE+2
         WRITE(6,"(12X,'Inflow QXs',2X,19I3)")(IQXAD(M,I), &
            I=1,NQTG(M))
 127     FORMAT(' IQXAD(',I2,')',2X,19I3)
         IF(SEEPTB(M))THEN
            WRITE(6,"(12X,'Seepage Routed Through QX(',I3,')')") &
                  ISEPQ(M)
            WRITE(6,"(12X,'Seepage Table Values')")
            WRITE(6,"(12X,'Resr Elev.     ',8F9.2)")(ESEEP(I,M),I=1,8)
            WRITE(6,"(12X,'Resr Seep (cfs)',8F9.2)")(SSEEP(I,M),I=1,8)
            LINE=LINE+4
         ENDIF
         IF (ISTOR(M)) THEN
           WRITE(6,'(12X,"TARGET STORAGE ",L1," QIN NUMBER ",I3)')ISTOR(M), &
              TargetStorQIN(M)
         END IF
         IF (ISTOR(M)) THEN
           WRITE(6,"(/12X,'Target Storage in QIN(',I3,')')")TargetStorQIN(M)
           LINE=LINE+2
         END IF
         IF (IREL(M)) THEN
           WRITE(6,"(/12X,'Target Release in QIN(',I3,')')")TargetRelQIN(M)
           LINE=LINE+2
         END IF
         IF (EvapOpt(M).EQ.2) THEN
           WRITE(6,"(/12X,'Evaporation ft./month in QIn(',I3,')')") EvapQIN(M)
           LINE=LINE+2
         ELSE IF(EvapOpt(M).EQ.3)THEN
           WRITE(6,"(/12X,'Evaporation af in QIn(',I3,')')") EvapQIN(M)
           LINE=LINE+2
         ELSE IF(EvapOpt(M).EQ.1)THEN
           WRITE(6,"(/12X,'Evaporation Factors ft./month for each month')")
           WRITE(6,"(12X,12A7,A8)")(VAR(K),K=2,14)
           WRITE(6,"(12X,12F7.4,F8.4)")(EvapFac(M,K),K=1,13)
           LINE=LINE+4
         END IF
         if (PRNRS(M)) then
           WRITE(6,"(/12X,'Print Reservoir Data')")
           LINE=LINE+2
         end if
   62 CONTINUE
      ENDIF
!-----------------------------------------------------------------------
!     MINIMUM FLOW DATA
!-----------------------------------------------------------------------
      IF(NQXMN.GT.0)THEN
       CALL HEADIN
       WRITE(6,'(/'' MINIMUM FLOW REQUIREMENTS FOR VARIOUS QX S'')')
       WRITE(6,"( '  MIN QX  UPSTREAM QX-S'"// &
                    "'  UP RES  FLOWS')")
       LINE=LINE+3
       DO 122 I=1,NQXMN
          WRITE(6,'(1X,I3,3X,20I3/1X,I3,13F9.1)')IQNUM(I), &
               (IQXMN(J,I),J=1,20),IQRES(I),(QXMN(K,I),K=1,13)
 122     CONTINUE
      ENDIF
      IF(NLND.GT.0)THEN
       CALL HEADIN
       WRITE (6,"(/,30X,'LAND USE PARAMETERS',/)")
      ENDIF
!-------------------------------------------------------------------------
!Land area data
!-------------------------------------------------------------------------
      RETI=1.00
      DO L=1,NLND
       IF(L.NE.1)CALL HEADIN
       WRITE(6,400)Trim(AllTrim(PLAND(L)))
400    FORMAT(25X,'SUMMARY OF WATER USE DATA FOR LAND AREA ',A)
           WRITE(6,"(/33X,'MONTH    OCT   NOV   DEC   JAN   FEB   MAR   A"// &
            "PR   MAY   JUN   JUL   AUG   SEP    ANN')")
          TOTPRE = 0.0
          DO IA=1,12
             TOTPRE = TOTPRE + PR(L,IA)
          ENDDO
          WRITE(6,4000)(PR(L,IA),IA=1,12),TOTPRE
 4000     FORMAT(/12X,'AVERAGE MON. PRECIPITATION ',12F6.2,F7.2)
          WRITE(6,'(A)')' '
          WRITE(6,"(33X,'MONTH    OCT   NOV   DEC   JAN   FEB   MAR   AP"// &
            "R   MAY   JUN   JUL   AUG   SEP    ANN')")
          TOTEM = 0.0
          DO IA= 1,12
             TOTEM = TOTEM + TM (L,IA) /12.
          ENDDO
          WRITE(6,"(12X,'AVERAGE MON. TEMPERATURE   ',12F6.1,F7.1)" &
                  )(TM(L,IA),IA=1,12),TOTEM
 9300     FORMAT(12X,'AVERAGE MON. TEMPERATURE   ',12F6.1,F7.1)
        WRITE(6,"(/23X,'Proportion Shallow GW Use by Alfalfa',F10.3)")CPRO(L)
        WRITE(6,108)IRV(L),IFT(L),IRT(L)
 108    FORMAT(10X,' GW QX NUMBER FOR PASTURE ',I3, &
          ' DOWNSTREAM ',I3,' TOTAL RETURN FLOW ',I3/)
        WRITE(6,"(12X,'IRRIGATED CROPLAND ACREAGES')")
        WRITE(6,"(12X,'Land Use Description',5X,'Acreage',"// &
          "3X,'Land Type')")
        CALL WRITECROP(Alfalfa(L)+Hay(L)-SubHay(L),"Hay and Alfalfa")
        CALL WRITECROP(Pasture(L)-SubPast(L),"Pasture")
        CALL WRITECROP(Grain(L),"Grain")
        CALL WRITECROP(Corn(L),"Corn")
        CALL WRITECROP(Orchard(L),"Orchard")
        CALL WRITECROP(Sorghum(L),"Sorghum")
        CALL WRITECROP(Turf(L),"Turf")
        CALL WRITECROP(Onions(L),"Onions")
        CALL WRITECROP(OtherHort(L),"Other Horticulture")
        CALL WRITECROP(Potatoes(L),"Potatoes")
        CALL WRITECROP(Berries(L),"Berries")
        CALL WRITECROP(OtherVeg(L),"Other Vegetables")
        CALL WRITECROP(Tomatoes(L),"Tomatoes")
        CALL WRITECROP(Beans(L),"Beans")
        CALL WRITECROP(Vineyard(L),"Vineyard")
        CALL WRITECROP(SubHay(L),"Subirrigated Hay")
        CALL WRITECROP(SubPast(L),"Subirrigated Pasture")
       WRITE(6,'(20X," TOTAL CROPLAND ACRES",F12.1)')ACRAG(L)
       WRITE(6,'(22X," GROUNDWATER MINING",F12.1," Acre Feet")')GWMining(L)
!       EFPRE(L)=80.
       WRITE(6,"(16X,'  EFFECTIVE PRECIPITATION"// &
               "      ',F5.1,'%')")EFPRE(L)*100.0
       WRITE(6,"(/12X,' YEAR  CANAL EFF.  ON FARM EFF.')")
       LINE=LINE+2
       DO J=1,NYRS
         IF (J.EQ.1) THEN
           PrintEffLine=.TRUE.
         ELSE
           IF (ABS(CEFF(L,J)-CEFF(L,J-1)).GT..0001.OR. &
             ABS(IEFF(L,J)-IEFF(L,J-1)).GT..0001) THEN
             PrintEffLine=.TRUE.
           ELSE
             PrintEffLine=.FALSE.
           END IF
         END IF
         IF (PrintEffLine) THEN
           WRITE(6,"(13X,I4,3X,F6.1,'%',5X,F6.1,'%')")J+INYR-1,CEFF(L,J)*100.0, &
             IEFF(L,J)*100.0
           LINE=LINE+1
         END IF
       END DO
       !WRITE(6,"(12X,'  CONVEYANCE EFFICIENCY (PERCENT)"// &
       !        "  ',F5.1,'%')")CEFF(L)*100.0
       !WRITE(6,"(12X,'  IRRIGATION EFFICIENCY"// &
       !        " (PERCENT)  ',F5.1,'%')")IEFF(L)*100.
       !WRITE(6,"(12X,'  PROP. OF "// &
       !        "DEEP PERC. RETURNING    ',F5.3)")RETI(L)
       WRITE(6,"(/12X,'  USE SOIL MOISTURE "// &
               "STORAGE            ',L1)")ISOL(L)
       IF(ISOL(L))WRITE(6,"(12X,'  SOIL MOISTURE "// &
               "HOLDING CAPACITY   ',F5.2,' INCHES')") &
               SOIL(L)*12.0/(TotalAcres(L)-SubHay(L)-SubPast(L))
       LINE=LINE+9
       TOT=0.0
       DO I=1,10
          TOT = TOT + PCRF(L,I)
       ENDDO
!       IF(TOT.GT.0.0)THEN
!          DO I=1,10
!             PCRF(L,I)=PCRF(L,I)/TOT
!          ENDDO
!       END IF
!       TOT=1.0
       WRITE (6,28021)(PCRF(L,KK),KK=1,10),TOT
28021    FORMAT(/12X,'AGRICULTURAL RETURN FLOW FACTORS'/ &
               12X,'LAG  0    1    2    3    4    5', &
               '    6    7    8    9   TOT'/ &
               14X,11F5.2)
       IF(LNRS(L).GT.0)THEN
          WRITE(6,109)LNRS(L)
109         FORMAT('0',12X,'NUMBER OF RESERVOIRS FOR THIS ', &
                  'LAND AREA TO CALL ',I3/ &
                  12X,' RES. NO.   QX''S FOR ROUTE')
          LINE=LINE+2
          DO I=1,LNRS(L)
             WRITE(6,"(12X,I5,2X,20I3,' Begin Month ',I3," &
                  //"' End Month ',I3)")LRES(L,I), &
                     (ILNQ(L,I,M),M=1,20),BEGDEL(L,I),ENDDEL(L,I)
             LINE=LINE+1
             IF(LINE.GT.60)CALL HEADIN
          ENDDO
       ENDIF
       IF(NQLIN(L).GT.0)THEN
          WRITE(6,"('0',12X,'DIVERSION  BYPASS    "// &
                  "QIN NO.   UPST QX  Min Flow Reqd')")
          LINE=LINE+2
          DO I=1,NQLIN(L)
             IF (IBY(L,I).GT.0) THEN
               IF (IQXMIN(IBY(L,I)).GT.0) THEN
                 ISBY(L,I)=.TRUE.
               END IF
             END IF
             WRITE(6,"(12X,I4,I10,I10,I12,L8)") &
                     IDV(L,I),IBY(L,I),ILQIN(L,I),ILUP(L,I), &
                     ISBY(L,I)
             LINE=LINE+1
             IF(ISBY(L,I))THEN
                WRITE(6,"(12X,' BYPASS FLOWS '/12X,12F8.0,F9.0)") &
                       (FBYPS(L,I,K),K=1,13)
                LINE=LINE+1
             ENDIF
          ENDDO
       ENDIF
      ENDDO
      WETLNDPR=0.0
      WETLNDTM=0.0
      WETLNDET=0.0
      FNYRS=FLOAT(NYRS)
      DO L=1,NRIP
        IF (L.EQ.1.OR.LINE>LINESPERPAGE-30) THEN
          CALL HEADIN()
          WRITE(6,"(/14X,'RIPARIAN USE DATA')")
        END IF
        LINE=LINE+5
        WRITE(6,"(25X,'SUMMARY OF WATER DEMAND DATA FOR RIPARIAN AREA ',A)") &
          Trim(AllTrim(RipHeading(L)))
        WRITE(6,"(/33X,'MONTH     OCT    NOV    DEC    JAN    FEB    MAR    A"// &
          "PR    MAY    JUN    JUL    AUG    SEP     ANN')")
        DO K=1,12
          DO J=1,NYRS
            WETLNDPR(L,K)=WETLNDPR(L,K)+WTPRE(L,J,K)/FNYRS
            WETLNDPR(L,13)=WETLNDPR(L,13)+WTPRE(L,J,K)/FNYRS
          END DO
        ENDDO
        WRITE(6,"(/12X,'AVERAGE MON. PRECIPITATION ',12F7.2,F8.2)") &
          (WETLNDPR(L,K),K=1,13)
        DO K= 1,12
          DO J=1,NYRS
            WETLNDTM(L,K)=WETLNDTM(L,K)+WTEM(L,J,K)/FNYRS
            WETLNDTM(L,13)=WETLNDTM(L,13)+WTEM(L,J,K)/(FNYRS*12.0)
          END DO
        ENDDO
        WRITE(6,"(12X,'AVERAGE MON. TEMPERATURE   ',12F7.1,F8.1)" &
                )(WETLNDTM(L,K),K=1,13)
        DO K= 1,12
          DO J=1,NYRS
            WETLNDET(L,K)=WETLNDET(L,K)+WETUSE(K,J,L)/FNYRS
            WETLNDET(L,13)=WETLNDET(L,13)+WETUSE(K,J,L)/(FNYRS)
          END DO
        ENDDO
        WRITE(6,"(12X,'AVERAGE MON. DEMAND        ',12F7.0,F8.0)" &
                )(WETLNDET(L,K),K=1,13)
        WRITE(6,"(/10X,' QX NUMBERS FOR RIPARIAN DEMAND ',I3," // &
          "' UPSTREAM ',I3,' DOWNSTREAM ',I3/)")IPH(L),IPHU(L),IPHD(L)
        WRITE(6,"(/10X,' Proportion of Landarea riparian demand '," // &
          "F6.2)")
        WRITE(6,'(19X," TOTAL WETLAND ACRES =",F12.1)')WACRE(L)
        LINE=LINE+11
      END DO
      DO L=1,NMUN
       CALL HEADIN()
       WRITE(6,"(/14X,'MUNICIPAL DEMAND DATA')")
       WRITE(6,"(25X,'SUMMARY OF WATER USE DATA FOR MUNICIPAL AREA ',A)") &
        trim(AllTrim(MunGroup(L)))
       DAT=0.0
       DO J=1,NYRS
        DO K=1,12
          DAT(K)=DAT(K)+(MunGW(L,J,K)+MunSurf(L,J,K))/FLOAT(NYRS)
          DAT(13)=DAT(13)+(MunGW(L,J,K)+MunSurf(L,J,K))/FLOAT(NYRS)
        END DO
       END DO
       WRITE(6,"(12X,'AVERAGE MONTHLY MUNICPAL DEMAND')")
       WRITE(6,"(10X,12(4X,A4,1X),5X,A4)")(VAR(I),I=2,14)
       WRITE(6,"(10X,12F9.0,F10.0)") (DAT(I),I=1,13)
       WRITE(6,"(/12X,'INDOOR DEMAND FACTORS BY MONTH')")
       TOT=0.0
       DO K=1,12
          TOT=TOT+QMIP(L,K)
       ENDDO
       WRITE(6,"(12X,12(2X,A4),2X,A4)")(VAR(I),I=2,14)
       WRITE(6,"(12X,'Monthly Indoor Factors')")
       WRITE(6,"(12X,13F6.3)")(QMIP(L,IA),IA=1,12),TOT
       TOT=0.0
       DO K=1,12
          TOT=TOT+OutDoorFac(L,K)
       ENDDO
       WRITE(6,"(12X,'Monthly Outdoor Factors')")
       WRITE(6,"(12X,13F6.3)")(OutdoorFac(L,IA),IA=1,12),TOT
       IF (NCOMM(L)>0) THEN
         WRITE(6,"(/10X,'Community System Providers')")
         DO M=1,NCOMM(L)
           WRITE(6,"(10X,A)")Trim(AllTrim(MunName(L,M)))
         END DO
       END IF
       IF (NumNonComm(L)>0) THEN
         WRITE(6,"(/10X,'Non-Community System Providers')")
         do M=1,NumNonComm(L)
           WRITE(6,"(10X,A)")Trim(AllTrim(NonName(L,M)))
         end do
       END IF
       IF (NMunSS(L)>0) THEN
         WRITE(6,"(/10X,'Self-supplied industry')")
         DO M=1,NMunSS(L)
           WRITE(6,"(10X,A)")trim(AllTrim(MSSName(L,M)))
         END DO
       END IF
       WRITE(6,"(//10X,'Return flow QX=',I4)")MunQXRet(L)
       IF (MunGWQx(L)>0) THEN
         WRITE(6,"(10X,'Groundwater QX=',I4)")MunGWQx(L)
       END IF
       IF (NQXMun(L)>0) THEN
         WRITE(6,"(10X,'QXs supplying area demands')")
         WRITE(6,"(14X,'QXin QXup QXdn ResUp')")
         DO M=1,NQXMun(L)
           WRITE(6,"(13X,3I5,1X,A20)")MunQXin(L,M),MunQXup(L,M), &
             MunQXdn(L,M),Trim(AllTrim(MDamID(L,M)))
         END DO
       END IF
      END DO
      CALL HEADIN
      WRITE(6,"(10X,A20,'=',I7,2X,A20,'=',I7,2X,A20,'=',I7)") &
        VarTitles(1),NYRS,VarTitles(2),INYR,VarTitles(3),NRES
      WRITE(6,"(10X,A20,'=',I7,2X,A20,'=',I7,2X,A20,'=',I7)") &
        VarTitles(4),NLND,VarTitles(5),NQIN,VarTitles(6),NQX
      IF (PrintCapacityInt<>0) THEN
        IPST=.TRUE.
      ELSE
        IPST=.FALSE.
      END IF
      IF (PrintResDataInt<>0) THEN
        IPEV=.TRUE.
      ELSE
        IPEV=.FALSE.
      END IF
      IF (PrintStageInt<>0) THEN
        IPEL=.TRUE.
      ELSE
        IPEL=.FALSE.
      END IF
      IF (PrintAreaInt<>0) THEN
        IPSA=.TRUE.
      ELSE
        IPSA=.FALSE.
      END IF
      IF (PrintLandAreaInt<>0) THEN
        IPAS=.TRUE.
      ELSE
        IPAS=.FALSE.
      END IF
      IF (PrintInflowsInt<>0) THEN
        IPQIN=.TRUE.
      ELSE
        IPQIN=.FALSE.
      END IF
      SELECT CASE (TypeYear)
        CASE (WaterYear)
          TempStr=" Water Year"
        CASE (WaterYearNov)
          TempStr=" Water Year Nov"
        CASE (CalendarYear)
          TempStr=" Calendar"
        CASE DEFAULT
      END SELECT
      WRITE(6,"(10X,A20,'=',L7,2X,A20,'=',L7,2X,A20,'=',L7)") &
        VarTitles(7),IPSH,VarTitles(8),IPST,VarTitles(9),IPSA
      WRITE(6,"(10X,A20,'=',L7,2X,A20,'=',L7,2X,A20,'=',L7)") &
        VarTitles(10),IPEV,VarTitles(11),IPEL,VarTitles(12),IPSA
      WRITE(6,"(10X,A20,'=',I7,2X,A20,'=',I7,2X,A20,'=',A)") &
        VarTitles(13),NQXMN,VarTitles(14),ITERMX,VarTitles(15),TempStr
      WRITE(6,"(/10X,'Model Number ',I3)")ModelID
      WRITE(6, "(10X,'OUTPUT FILE, ',A)")Trim(AllTrim(printfile))
      LINE=LINE+11
!      IF (NHPW.GT.0) THEN
!       CALL HEADIN
!       DO IP=1,NHPW
!          WRITE(6,"(///30X,'Hydropower Data For ',A32/)") &
!                  PHYPW(IP)
!          WRITE(6,"(10X,'QPMX,QPMN,HDMN,ELTW,E1,CP1,KS,KE')")
!          WRITE(6,"(10X,2F10.0,2F10.1,F10.2,F10.5,2I10)") &
!                  QPMXI(IP),QPMNI(IP),HDMNI(IP),ELTWI(IP), &
!                  E1I(IP),CP1I,KSI(IP),KEI(IP)
!          WRITE(6,"(10X,'IHS,IHP,IHB,IHR,IHNP')")
!          WRITE(6,"(10X,5I4)") &
!                  IHS(IP),IHP(IP),IHB(IP),IHR(IP),IHNP(IP)
!          IF(IHNP(IP).GT.0) THEN
!             WRITE(6,"(10X,' PIPE      QX"// &
!                     "    LENGTH  DIAMETER ROUGHNESS')")
!             DO I=1,IHNP(IP)
!              WRITE(6,'(10X,I5,I8,F10.1,F10.4,F10.7)') I, &
!                     IHPQX(IP,I),HPLEN(IP,I),HPDIA(IP,I),HPRGH(IP,I)
!             ENDDO
!          ENDIF
!       END DO
!      ENDIF
      CALL HEADIN
      WRITE(6,"(10X,' IINFL ',19I3,A)")IINFL," Inflow QXs from other areas"
      WRITE(6,"(10X,' ITRIB ',19I3,A)")ITRIB," Flow QXs originating in subarea"
      WRITE(6,"(10X,'  IIMP ',19I3,A)")IIMP," Import QXs from other areas"
      WRITE(6,"(10X,' IEXPO ',19I3,A)")IEXPO," Export QXs to other areas"
      WRITE(6,"(10X,'  IUNG ',19I3,A)")IUNG," Ungauged QXs originating in subarea"
      WRITE(6,"(10X,' NOUFL ',19I3,A)")NOUFL," Outflow QXs to downstream area"
      WRITE(6,"(10X,'NOUEVFL',19I3,A)")NOUEVFL," Outflow QXs to terminal evaporation"
      IF (TranNum.GT.0) THEN
        WRITE(6,"(10X,'TranQX ',19I3,A)")TRANQX," Transbasin Diversions"
        WRITE(6,"(10X,'TranDir ',19(A2,1X),T75,A)")TranDir, &
          " Transbasin Direction Ex or Im"
      END IF
      IF (NUMBASIN.GT.0) THEN
        WRITE(6,"(10X,'BainIn ',19I3,A)")IBASIN," Basin Inflows"
        LINE=LINE+1
      END IF
      IF (NUMBASOUT.GT.0) THEN
        WRITE(6,"(10X,'BainIn ',19I3,A)")IBASOUT," Basin Inflows"
        LINE=LINE+1
      END IF
!      IF(IANPLT.OR.IMNPLT)WRITE(6,245)IANPLT,IMNPLT
!  245 FORMAT(/10X,'PLOTS:  IANPLT,IMNPLT',2L5/)
      LINE=LINE+7
!     PRINT NAMES OF QIN FILES
!
      IF(NQIN.GT.0)THEN
       WRITE(6,"(////10X,'QIN FILE',32X,'  QX NO.  DESCRIPTION',T116,'METHOD')")
       DO I=1,NQIN
          QFILE(I)=Trim(AllTrim(QFILE(I)))
          charLen=LEN_TRIM(QFILE(I))
          locComma(1)=INDEX(QFILE(I),",")
          IF (locComma(1).GT.0) THEN
            locComma(2)=INDEX(QFILE(I)((locComma(1)+1):),",")+locComma(1)
            locComma(3)=INDEX(QFILE(I),"(")-1
            locComma(4)=INDEX(QFILE(I)((locComma(3)+2):),"(")+locComma(3)
            locComma(5)=INDEX(QFILE(I)((locComma(4)+1):),",")+locComma(4)
            if (locComma(5).GT.locComma(4)) then
              PourKind=QFILE(I)((locComma(5)+1):charLen)
              if (trim(PourKind)=="AA") then
                tempStr="Area Altitude"
              else if(trim(PourKind)=="SS") then
                tempStr="StreamStats"
              end if
            else
              PourKind="UNK"
              tempStr="Unknown"
            end if
          ELSE
            PourKind=""
            tempStr=""
          END IF
          WRITE(6,"(10X,A40,1X,I7,2X,A52,1X,A13)") &
                  LeftAdj(QFILE(I),40),IQXN(I),AllTrim(CDESCR(I)),tempStr
       ENDDO
       LINE=LINE+NQIN+5
       
      ENDIF
!!--Extracted from main program
!
!     INITIALIZATION
!
      DO K=1,13
         EFCR   (K)=0.0
         WPRE   (K)=0.0
         WOPN   (K)=0.0
         DO I=1,MLAND
            LGEFPRE(I,K)=0.0
            GWUSE(I,K)=0.0
            RTUSE(I,K)=0.0
            SBUSE(I,K)=0.0
         ENDDO
      ENDDO
      DO J=1,NYRS
         IYEAR(J)=INYR+J-1
      ENDDO
      FNYRS=NYRS
      DO M=1,NRES
         RV = STOIC(M)
         STO(M) = RV
         CALL RACE (M,RV,RA,RE)
         REL(M)= RE
         RAR(M)= RA
         H1I(M)=REL(M)-ELTWI(M)
      ENDDO
      DO L=1,NLND
         DO K=1,13
            DO J=1,NYRS
              CPACFT(L,J,K)=0.0
            ENDDO
         ENDDO
      ENDDO
      DO L=1,NLND
       DO J=1,NYRS
         TEF(L,J)=CEFF(L,J)*IEFF(L,J)
       END DO
       CN=0.0
       PRC=0.0
       CON=0.0
!-End of extraction from main program
!Took initialization from here
!-----------------------------------------------------------------------
!     BEGIN CROP LOOP
!-----------------------------------------------------------------------
       DO J = 1, NYRS
!
!           BEGIN MONTH LOOP
          DO K = 1,12
!~~We need some measure of the effective precipitation over crop land.
             CALL FAVER(EFCR,EffPRE(L,J,K)/ &
                     (REAL(NYRS,KIND=8)),REAL(1.,KIND=8),REAL(1.,KIND=8),K)
!~~We need some measure of the effective precipitation over subirr crop land.
!**********************************************************************
!              WETLANDS
!**********************************************************************
             CALL FAVER(WOPN,WCUSE(L,J,K)/FNYRS,REAL(1.,KIND=8), &
               REAL(1.,KIND=8),K)
          ENDDO
       ENDDO
      DO K=1,13
         CN(K)=0.0
         PRC(K)=0.0
      ENDDO
      DO J=1,NYRS
         DO K=1,12
              IF(NYRS.GT.0)THEN
                 CN(K)=CN(K)+(CONUSE(K,J,L)+WCUSE(L,J,K))/ &
                             FLOAT(NYRS)
                 PRC(K)=PRC(K)+PERCO(K,J,L)/FLOAT(NYRS)
                 CN(13)=CN(13)+(CONUSE(K,J,L)+WCUSE(L,J,K))/ &
                             FLOAT(NYRS)
                 PRC(13)=PRC(13)+PERCO(K,J,L)/FLOAT(NYRS)
              ENDIF
         ENDDO
      ENDDO
      IF(LINE.GT.53)CALL HEADIN
      WRITE(6,"(/10X,'DEMAND SUMMARY FOR ',A)")trim(AllTrim(PLAND(L)))
      WRITE (6,23)(VAR(I),I=2,14)
   23 FORMAT(/25X,12(3X,A4,1X),5X,A4)
      WRITE(6,"(/10X,' ROOT ZONE REQ.',12F8.0,F10.0)") &
              (CN(K),K=1,13)
      WRITE(6,'(10X," EXCESS RAIN   ",12F8.1,F10.1)')(PRC(K),K=1,13)
       DO J=1,NYRS
          DO K=1,12
             IF(NYRS.GT.0)THEN
              CON(K)=CON(K)+(CONUSE(K,J,L)+ &
                        WCUSE(L,J,K))/(CEFF(L,J)*IEFF(L,J))/FLOAT(NYRS)
              CON(13)=CON(13)+(CONUSE(K,J,L)+ &
                        WCUSE(L,J,K))/(CEFF(L,J)*IEFF(L,J))/FLOAT(NYRS)
             ENDIF
          ENDDO
       ENDDO
       WRITE(6,"(10X,' DIVERSION REQ.',12F8.0,F10.0)") &
               (CON(K),K=1,13)
!---------------------------------------------------------------------
!     DO NOT INCLUDE LAWN AND GARDEN ACREAGES (ACRAGR) INTO ACRAG
!---------------------------------------------------------------------
       QDVR(L,13)=0.0
       QDVRA(L,13)=0.0
       QDMI(L,13)=0.0
       DO K=1,12
          QDVRA(L,K)=CON(K)
          QDVR(L,K)=QDVRA(L,K)+QDMI(L,K)
          QDVRA(L,13)=QDVRA(L,13)+QDVRA(L,K)
          QDVR(L,13)=QDVR(L,13)+QDVR(L,K)
          QDMI(L,13)=QDMI(L,13)+QDMI(L,K)
       ENDDO
       IF(.NOT.ISOL(L))THEN
          SOIL(L)=0.0
          MOIST(L)=0.0
          WSOIL(L)=0.0
          WMOIS(L)=0.0
       END IF
      ENDDO
      WPRE=0.0
      DO L=1,NRIP
        DO J=1,NYRS
          DO K=1,12
            CALL FAVER(WPRE,WETPRE(K,J,L)*.8/ &
               (REAL(NYRS,KIND=8)),REAL(1.,KIND=8),REAL(1.,KIND=8),K)
          END DO
        END DO
      END DO
      IF(ISTOP)STOP
      DO M=1,NRES
       DO K=1,12
          QRMN(M,K)=QRMN(M,K)+QSM(M,K)*FLOAT(IDAYS(K))*1.983471074
       ENDDO
       QRMN(M,K)=0.0
       QSM(M,K)=0.0
       DO K=1,12
          QSM(M,13)=QSM(M,13)+QSM(M,K)
          QRMN(M,13)=QRMN(M,13)+QRMN(M,K)+QSM(M,K)*FLOAT( &
            IDAYS(K))*1.983471074
       ENDDO
      ENDDO
      FNYRS=FLOAT(NYRS)
!
!     PRINT QIN'S
!
      IF(IPQIN)CALL PRNTQI
!
!     READ RESERVOIR INPUT FILES, IF ANY
!     PRINT INPUT FILES IF NEEDED
!
      DO L=1,NLND
        DO K=1,13
          TM(L,K)=0.0
          PR(L,K)=0.0
          SUM=0.
          DO J=1,NYRS
            TM(L,K)=TM(L,K)+TEM(L,J,K)/FLOAT(NYRS)
            PR(L,K)=PR(L,K)+TEM(L,J,K)/FLOAT(NYRS)
            SUM=SUM+TEM(L,J,K)
          ENDDO
          QINM(K)=ROUND(SUM/FNYRS,2)
        ENDDO
      ENDDO
!********************************************************************
!Start Simulation
!********************************************************************
     IF (Calibrate) THEN
       TOL=.0001
       IF (TypeCalib.EQ.2) THEN ! Calibrate Return Flow
          DO M=1,10
            RetProp(M)=PCRF(CalibArea,M)/PCRF(CalibArea,1)
          END DO
       END IF
       CALL BRENT(MaxCal,InitialCal,MinCal,Simul,TOL,CalibVal)
       WRITE(6,"(10X,'Final Value = ',F6.3)")CalibVal
       PRINT *,"Final Value ",CalibVal
       Retn=Simul(CalibVal)
     ELSE
       theZero=0.0
       Retn=Simul(theZero)
     END IF
!********************************************************************
!Print Results
!********************************************************************

!     PRINT SIMULATION OUTPUT
!
     CALL PRINTR
!
     IF (WriteDatabase) THEN
       !Write a text file to be read by VB.NET and added to
       !output.accdb databases.
       FileData="O:\database\watbudg\data\results\" // &
         trim(alltrim(PrintName)) // ".txt"
       OPEN(UNIT=7,FILE=FileData,STATUS='UNKNOWN',ACTION='WRITE', &
         CARRIAGECONTROL='FORTRAN',IOSTAT=ioerror,BLOCKSIZE=5000)
       WRITE(7,"(I5,',',A,6(',',I5))")ModelID,trim(NoComma(ModelName)), &
         NLND,NMUN,NRIP,NRES,INYR,EndYear
       !Write out Land Area data
       DO L=1,NLND
         DO J=1,NYRS
           GWDiv=0.0
           SurDiv=0.0
           PotET=0.0
           AvPre=0.0
           AvRainAF=0.0
           AvNatuse=0.0
           EffRainFall=0.0
           RetFlow=0.0
           IF (GWQX(L).GT.0) THEN
             DO K=1,13
               GWDiv(K)=QX(GWQX(L),J,K)
             END DO
           END IF
           DO M=1,NumAGQX(L)
             DO K=1,13
               SurDiv(K)=SurDiv(K)+QX(UniqAgDivQX(L,M),J,K)
             END DO
           END DO
           IF (IRV(L).GT.0) THEN
             DO K=1,13
               SurDiv(K)=SurDiv(K)+QX(IRV(L),J,K)
             END DO
           END IF
           IF (IRT(L).GT.0) THEN
             DO K=1,13
               RetFlow(K)=RetFlow(K)+QX(IRT(L),J,K)
             END DO
           END IF
           DO K=1,13
             PotET(K)=(CONUSE(K,J,L)+WCUSE(L,J,K))
             EffRainFall(K)=(EffPRE(L,J,K)-PERCO(K,J,L)-WPERC(K,J,L))
           END DO
           LandAreaCode(L)=trim(AllTrim(LandAreaCode(L)))
           WRITE(7,"(' Ag,',I5,',',I2,',',I2,',',2(I4,','),2(D15.9,','),A," // &
             "13(',',D15.9),13(',',D15.9),13(',',D15.9),13(',',D15.9),39(',',D15.9))") &
             MODELID,RptDay,RptMonth,RptYear,J+INYR-1,CEFF(L,J),IEFF(L,J), &
             trim(LandAreaCode(L)),(AgDeplete(L,J,K),K=1,13),(Short(L,J,K),K=1,13), &
             (GWDiv(K),K=1,13),(SurDiv(K),K=1,13),(PotET(K),K=1,13), &
             (EffRainFall(K),K=1,13),(RetFlow(K),K=1,13)
         END DO
       END DO
       !Write out municipal data
       DO J=1,NYRS
         GWDiv=0.0
         SurDiv=0.0
         DO L=1,NMUN
           IF (MunGWQx(L).GT.0) THEN
             DO K=1,13
               GWDiv(K)=GWDiv(K)+QX(MunGWQx(L),J,K)
             END DO
           END IF
           DO M=1,NumMIQX(L)
             DO K=1,13
               SurDiv(K)=SurDiv(K)+QX(UniqMIDiv(L,M),J,K)
             END DO
           END DO
         END DO
         WRITE(7,"(' Municipal,',I5,',',I2,',',I2,2(',',I4)," // &
           "4(13(',',D15.9)))")ModelID,RptDay,RptMonth,RptYear,J+INYR-1, &
           (SurDiv(K),K=1,13),(GWDiv(K),K=1,13), &
           (MandIDep(J,K),K=1,13),(SSIDeplete(J,K),K=1,13)
       END DO
       !Write out riparian data
       DO J=1,NYRS
         WRITE(7,"(' Riparian,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
           ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(WetDep(J,K),K=1,13)
       END DO
       !Write out reservoirs
       DO L=1,NRES
         DO J=1,NYRS
           StoDiff(L,J,13)=0.0
           DO K=1,12
             StoDiff(L,J,13)=StoDiff(L,J,13)+StoDiff(L,J,K)
           END DO
           WRITE(7,"(' Reservoir,',I5,',',I2,',',I2,2(',',I4),',',A,',',A,26(',',D15.9))") &
             ModelID,RptDay,RptMonth,RptYear,J+INYR-1, trim(DAM_NUMBER(L)),&
             trim(DAM_NAME(L)),(EVAP(L,J,K),K=1,13),(StoDiff(L,J,K),K=1,13)
         END DO
       END DO
       !Write out Inflow
       DO J=1,NYRS
         WRITE(7,"(' Inflow,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
           ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(SubInflow(J,K),K=1,13)
       END DO
       !Write Outflow
       DO J=1,NYRS
         WRITE(7,"(' Outflow,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
           ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(SubOutflow(J,K),K=1,13)
       END DO
       !Write exports
       DO J=1,NYRS
         WRITE(7,"(' Exports,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
           ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(SubExport(J,K),K=1,13)
       END DO
       !Write Imports
       DO J=1,NYRS
         WRITE(7,"(' Imports,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
           ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(SubImport(J,K),K=1,13)
       END DO
       !Write Yield with Mining
       if (gwminingqm(13).gt.0.0) then
         DO J=1,NYRS
           WRITE(7,"(' YieldwMining,',I5,',',I2,',',I2,2(',',I4),26(',',D15.9))") &
             ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(YieldWMining(J,K),K=1,13), &
             (GWMiningQ(J,K),K=1,13)
         END DO
       end if
       !Monthly Yield
       DO J=1,NYRS
         WRITE(7,"(' Yield,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
           ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(MonYield(J,K),K=1,13)
       END DO
       !Subarea Data
       DO J=1,NYRS
         WRITE(7,"(' SubareaData,',I5,',',I2,',',I2,2(',',I4),39(',',D15.9))") &
           ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(AreaPrecip(J,K),K=1,13), &
           (AreaPrAF(J,K),K=1,13),(SubNatUse(J,K),K=1,13)
       END DO
       !Terminal Reservoir Data
       IF(NumOutEvap.GT.0.0) THEN
         DO J=1,NYRS
           WRITE(7,"(' Terminal,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
             ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(TermOutEvap(J,K),K=1,13)
         END DO
       ENDIF
       !Basin Inflows
       IF(NUMBASIN.GT.0)THEN
         DO J=1,NYRS
           WRITE(7,"(' BasinIn,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
             ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(BasinIn(J,K),K=1,13)
         END DO
       EndIf
       !Basin Outflows
       IF (NUMBASOUT.GT.0) THEN
         DO J=1,NYRS
           WRITE(7,"(' BasinOut,',I5,',',I2,',',I2,2(',',I4),13(',',D15.9))") &
             ModelID,RptDay,RptMonth,RptYear,J+INYR-1,(BasinOut(J,K),K=1,13)
         END DO
       END IF
       !Transbasin Imports or Exports
       TransBasinIn=0.0
       TransBasinOut=0.0
       BasinInNum=0
       BasinOutNum=0
       IF (TranNum.GT.0) THEN
          Tran: DO M=1,TranNum
            IF (trim(TranDir(M)).EQ."Import") THEN
              IF (BasinInNum.LE.0) THEN
                BasinInNum=1
                MM=1
                OtherBasinIn(1)=TranBasin(M)
                OtherNameIn(1)=TranName(M)
              ELSE
                MM=0
                Imp: DO I=1,BasinInNum
                  IF (trim(OtherBasinIn(I)).EQ.trim(TranBasin(M))) THEN
                    MM=I
                    EXIT Imp
                  END IF
                END DO Imp
              END IF
              IF (MM.EQ.0) THEN
                BasinInNum=BasinInNum+1
                MM=BasinInNum
                OtherBasinIn(MM)=TranBasin(M)
                OtherNameIn(MM)=TranName(M)
              END IF
              DO J=1,NYRS
                DO K=1,12
                  TransBasinIn(MM,J,K)=TransBasinIn(MM,J,K)+ &
                    QX(TranQX(M),J,K)
                  TransBasinIn(MM,J,13)=TransBasinIn(MM,J,13)+ &
                    QX(TranQX(M),J,K)
                END DO
              END DO
            ELSE IF (trim(TranDir(M)).EQ."Export") THEN
              IF (BasinOutNum.EQ.0) THEN
                BasinOutNum=1
                MM=1
                OtherBasinOut(1)=TranBasin(M)
                OtherNameOut(1)=TranName(M)
              ELSE
                MM=0
                Exp: DO I=1,BasinOutNum
                  IF (trim(OtherBasinOut(I)).EQ.trim(TranBasin(M))) THEN
                    MM=I
                    EXIT Exp
                  ENDIF
                END DO Exp
              END IF
              IF (MM.EQ.0) THEN
                BasinOutNum=BasinOutNum+1
                MM=BasinOutNum
                OtherBasinOut(MM)=TranBasin(M)
                OtherNameOut(MM)=TranName(M)
              END IF
              DO J=1,NYRS
                DO K=1,12
                  TransBasinOut(MM,J,K)=TransBasinOut(MM,J,K)+ &
                    QX(TranQX(M),J,K)
                  TransBasinOut(MM,J,13)=TransBasinOut(MM,J,13)+ &
                    QX(TranQX(M),J,K)
                END DO
              END DO
            END IF
          END DO Tran
          IF (BasinInNum.GT.0) THEN
            DO M=1,BasinInNum
              DO J=1,NYRS
                WRITE(7,"(' TransbasinImport,',I5,',',I2,',',I2,2(',',I4)," // &
                  "4(',',A),13(',',D15.9))")ModelID,RptDay,RptMonth,RptYear, &
                  J+INYR-1,trim(Basin(1)),trim(BasinName(1)), &
                  trim(OtherBasinIn(M)),trim(OtherNameIn(M)), &
                  (TransBasinIn(M,J,K),K=1,13)
              END DO
            END DO
          END IF
          IF (BasinOutNum.GT.0) THEN
            DO M=1,BasinOutNum
              DO J=1,NYRS
                WRITE(7,"(' TransbasinExport,',I5,',',I2,',',I2,2(',',I4)," // &
                  "4(',',A),13(',',D15.9))")ModelID,RptDay,RptMonth,RptYear, &
                  J+INYR-1,trim(Basin(1)),trim(BasinName(1)), &
                  trim(OtherBasinOut(M)),trim(OtherNameOut(M)), &
                  (TransBasinOut(M,J,K),K=1,13)
              END DO
            END DO
          END IF
       END IF
      ENDIF
     IF(NFDT.LE.0) GOTO 94
!
!      FLOW DURATION TABLE COMPUTATIONS
!
      MONTH(1)='  AN'
      MONTH(2)='NUAL'
      MONTH(3)='    '
      IENYR=INYR+NYRS -1
    94 CONTINUE
!
!        WRITE PLOT FILES
!
!        ANNUAL PLOTS
!      IF(IANPLT)THEN
!       DO J=1,NYRS
!          NPTS=0
!          CALL PLTPTS (23,MQX,NPTS,J,IQXPLT,QX,QQ,.FALSE.)
!          IF(NPTS.LE.23)THEN
!             CALL PLTPTS (7,MRES,NPTS,J,IRSPLT,ST,QQ,.FALSE.)
!             IF(NPTS.LE.23)THEN
!              CALL PLTPTS (7,MRES,NPTS,J,IEVPLT,EVAP,QQ,.FALSE.)
!              IF(NPTS.LE.23)THEN
!                 CALL PLTLPT (7,MLAND,NPTS,ISAPLT,QDVR,QQ)
!              ENDIF
!             ENDIF
!          ENDIF
!          IF(J.EQ.1)THEN
!             OPEN(7,FILE=PLOTFL,STATUS='UNKNOWN')
!             WRITE(7,"(1X,A76,'01',I2)")TITLE(1:76),NPTS
!          ENDIF
!          WRITE(7,"(I10,23F10.1)") IYEAR(J),(QQ(I),I=1,NPTS)
!       ENDDO
!      ENDIF
!        MONTHLY PLOTS
!      IF(IMNPLT)THEN
!       NMONTH=0
!       DO J=1,NYRS
!          DO K=1,12
!             NPTS=0
!             CALL PLTMPT (23,MQX,NPTS,IQXPLT,QX,QQ)
!                 CONVERT TO AC-FT/MONTH TO CFS
!             IF(NPTS.GE.23) GOTO 96
!             CALL PLTMPT (7,MRES,NPTS,IRSPLT,ST,QQ)
!             IF(NPTS.GE.23) GOTO 96
!             CALL PLTMPT (7,MRES,NPTS,IEVPLT,EVAP,QQ)
!             IF(NPTS.GE.23) GOTO 96
!             CALL PLMLPT (7,MLAND,NPTS,ISAPLT,QDVR,QQ)
!                 WRITE PLOT DATA FOR MONTH
!   96        CONTINUE
!             IF(J.EQ.1.AND.K.EQ.1)THEN
!                  OPEN(8,FILE=PLOTFL2,STATUS='UNKNOWN')
!                  WRITE(8,"(1X,A76,'01',I2)")TITLE(1:76),NPTS
!             ENDIF
!             NMONTH=NMONTH+1
!             FMONTH=NMONTH
!             FINYR=INYR
!             FYEAR = FINYR + FMONTH / 12.
!             WRITE(8,210) FYEAR,(QQ(I),I=1,NPTS)
!  210          FORMAT(F10.3,23F11.1)
!          ENDDO
!       ENDDO
!      ENDIF
      IF(IDISK)THEN
!        CREATE DISK FILES FOR QX'S
!        IF ISQX(I) IS NEGATIVE DO LAND AREA SHORTAGES INSTEAD
       IFIL=10
       DO I=1,NOUTFL
!~~Need to uncomment the following line        
          L=OutNumber(I)
          IF (OutType(I)==1) THEN
            QXLS='QX'
          ELSE IF (OutType(I)==6)THEN
            QXLS='LS'
          ELSE IF (OutType(I)==2) THEN
            QXLS='RS'
          END IF
          WRITE(QXLS(3:),'(I2.2)')ABS(L)
          OPEN(IFIL,FILE=AllTrim(OutPutFile(I)),STATUS='UNKNOWN')
          IL=IABS(L)
          IF (OutType(I)==1) THEN
            WRITE(IFIL,"(12X,'QX(',I2,')',2X,A48)") L,QXNAM(L)
          ELSE IF(OutType(I)==6) THEN
            WRITE(IFIL,"(12X,'SHORTAGE LAND AREA ',I2,2X,A32,5X)") &
                     L,PLAND(L)
          ELSE IF(OutType(I)==2) THEN
            IF (OutResSto(I).NE.0) THEN
              WRITE(IFIL,"(12X,'RESERVOIR ',I2,' STORAGE ',2X,A32,5X)") &
                     L,PRESV(L)
            ELSE IF (OutResArea(I).NE.0) THEN
              WRITE(IFIL,"(12X,'RESERVOIR ',I2,2X,' AREA ',A32,5X)") &
                     L,PRESV(L)
            ELSE IF (OutResElev(I).NE.0) THEN
              WRITE(IFIL,"(12X,'RESERVOIR ',I2,' ELEVATION ',2X,A32,5X)") &
                     L,PRESV(L)
            ELSE IF (OutResEvap(I).NE.0) THEN
              WRITE(IFIL,"(12X,'RESERVOIR ',I2,' EVAPORATION ',2X,A32,5X)") &
                     L,PRESV(L)
            END IF
          END IF
          DO J=1,NYRS
             XMAX = 0.0
             XMIN = 0.0
             DO K=1,12
              IF(OutType(I).EQ.1) THEN
                 XMAX=MAX(XMAX,QX(L,J,K))
                 XMIN=MIN(XMIN,QX(L,J,K))
              ELSE IF (OutType(I).EQ.6) THEN
                 XMAX=MAX(XMAX,SHORT(L,J,K))
                 XMIN=MIN(XMIN,SHORT(L,J,K))
              ELSE IF (OutType(I).EQ.2) THEN
                IF (OutResSto(I).NE.0) THEN
                  XMAX=MAX(XMAX,ST(L,J,K))
                  XMIN=MIN(XMIN,ST(L,J,K))
                ELSE IF (OutResArea(I).NE.0) THEN
                  XMAX=MAX(XMAX,SAR(L,J,K))
                  XMIN=MIN(XMIN,SAR(L,J,K))
                ELSE IF (OutResElev(I).NE.0) THEN
                  XMAX=MAX(XMAX,ELV(L,J,K))
                  XMIN=MIN(XMIN,ELV(L,J,K))
                ELSE IF (OutResEvap(I).NE.0) THEN
                  XMAX=MAX(XMAX,EVAP(L,J,K))
                  XMIN=MIN(XMIN,EVAP(L,J,K))
                END IF
              ENDIF
             ENDDO
             IF(XMAX.GT.0)THEN
              XMAX=LOG10(XMAX)
             ELSE IF(XMAX.LT.0)THEN
              XMAX=LOG10(ABS(XMAX))+1
             ELSE
              XMAX=0
             END IF
             IF(XMIN.GT.0)THEN
              XMIN=LOG10(XMIN)
             ELSE IF(XMIN.LT.0)THEN
              XMIN=LOG10(ABS(XMIN))+1
             ELSE
              XMIN=0
             END IF
             XMAX=MAX(XMAX,XMIN)
             IEXP = INT(XMAX)-4
             ISUM=0
             DO K=1,12
              IF(OutType(I)==1) THEN
                 IQX(K)=ROUND( QX(L,J,K)/10.**IEXP ,0)
              ELSE IF (OutType(I)==6) THEN
                 IQX(K)=ROUND(SHORT(L,J,K)/10.**IEXP ,0)
              ELSE IF (OutType(I)==2) THEN
                IF (OutResSto(I).NE.0) THEN
                  IQX(K)=ROUND(ST(L,J,K)/10.**IEXP ,0)
                ELSE IF (OutResArea(I).NE.0) THEN
                  IQX(K)=ROUND(SAR(L,J,K)/10.**IEXP ,0)
                ELSE IF (OutResElev(I).NE.0) THEN
                  IQX(K)=ROUND(ELV(L,J,K)/10.**IEXP ,0)
                ELSE IF (OutResEvap(I).NE.0) THEN
                  IQX(K)=ROUND(EVAP(L,J,K)/10.**IEXP ,0)
                END IF
              END IF
              ISUM=ISUM+IQX(K)
             ENDDO
             IQX(13)=ISUM
             WRITE(IFIL,"(8X,I4,I2,12I5,I6)")IYEAR(J),IEXP,IQX
          ENDDO
          CLOSE (IFIL)
       ENDDO
      ENDIF
      Print *,"All Done!!"
!     IF(SPLOT)CALL GRAPH90(JUSTEM(DFILE))
      STOP
      END
!-----------------------------------------------------------------------
!     SUBROUTINE INQ
!     PURPOSE : CHECKS INPUT DATA FOR ERRORS
!
!     *************************
!     Input Variable Definition
!     *************************
!
!     INP - QX NUMBER OR OTHER DATA ITEM TO BE CHECKED
!
!     MEANING OF ITY
!     1  -  QX for Land Area
!     2  -  QX for Reservoir
!     3  -  QX for Hydropower Plant
!
!     INUM - LAND AREA OR RESERVOIR ASSOCIATED WITH DATA
!
!     ******************
!     ARRAY DEFINITIONS:
!
!     ****************
!     QX Number Arrays
!     ****************
!
!     ICKIN - INFLOW ARRAY, FOR EVERY QX NUMBER ASSIGNED AS AN ARRAY
!           ICKIN RECORDS LAND AREA NUMBER OR RESERVOIR NUMBER
!     ICKOU - OUTFLOW ARRAY
!     ICKBY - BYPASS ARRAY
!     ICKUP - UPSTREAM (BEFORE DIVERSION) ARRAY
!     ICKRS - ARRAY OF QX'S USED TO CONNECT LAND AREAS TO RESERVOIRS
!           HOLDS RESERVOIR NUMBER WHICH IS CONNECTED TO LAND AREA
!
!     **************
!     QX Type Arrays
!     **************
!
!     ITYUP - ARRAY OF TYPES OF USES FOR UPSTREAM QX'S
!           1-RESERVOIR,2-LANDAREA, ETC.
!     ITYGW - ARRAY OF TYPES OF USES FOR GROUNDWATER QX'S
!     ITYBY - ARRAY OF TYPES OF USES FOR BYPASS QX'S
!     ITYIN - ARRAY OF TYPES OF USES FOR INFLOW QX'S
!     ITYOU - ARRAY OF TYPES OF USES FOR OUTFLOW QX'S
!
!     ***********
!     Subroutines
!     ***********
!
!
!     INQ - Fills Array for Inflow QX (ICKIN, ITYIN)
!           (IOPT=1)
!
!     IOU - Fills Array for Outflow QX (ICKOU, ITYOU)
!           (IOPT=2)
!
!     CBY - Fills Array for Bypass QX (ICKBY, ITYOU)
!           (IOPT=3)
!
!     CGW - Fills Array for Groundwater QX (ICKGW, ITYGW)
!           (IOPT=4)
!
!     UPC - Fills Array for Upstream QXs (ICKUP, ITYUP)
!           (IOPT=5)
!
!     RCK - Fills Array to Connect Land Areas to Reservoirs
!           (ICKRS, ITYUP)
!           (IOPT=6)
!----------------------------------------------------------------------
      SUB ROUTINE INQ(INP,ITY,INUM)
      use PARAMDIM
      use PrintStuff
      INTEGER*4 IOPT,INP,ITY,INUM,JJ
      IOPT=1
      GOTO 10
      ENTRY IOU(INP,ITY,INUM)
      IOPT=2
      GOTO 10
      ENTRY CBY(INP,ITY,INUM)
      IOPT=3
      GOTO 10
      ENTRY CGW(INP,ITY,INUM)
      IOPT=4
      GO TO 10
      ENTRY UPC(INP,ITY,INUM)
      IOPT=5
      GOTO 10
      ENTRY RCK(INP,ITY,INUM)
      IOPT=6
 10   CONTINUE
      IF(INP.LE.0)RETURN
      !IF(ICKIN(INP).GT.0.AND.IOPT.NE.1.AND.IOPT.NE.2.AND.IOPT.NE.3 &
      !  .AND.IOPT.NE.6) &
      !   CALL ERRPRT(INP,ITYIN(INP),ICKIN(INP),ITY,INUM,IOPT,1)
      IF(ICKOU(INP).GT.0.AND.(IOPT.EQ.4.OR.IOPT.EQ.2).AND.ITY.NE.2) &
        CALL ERRPRT(INP,ITYOU(INP),ICKOU(INP),ITY,INUM,IOPT,2)
      IF(ICKGW(INP).GT.0) &
        CALL ERRPRT(INP,ITYGW(INP),ICKGW(INP),ITY,INUM,IOPT,3)
      IF(ICKBY(INP).GT.0.AND.IOPT.NE.1.AND.IOPT.NE.2.AND.IOPT.NE.4 &
        .AND.IOPT.NE.6) &
        CALL ERRPRT(INP,ITYBY(INP),ICKBY(INP),ITY,INUM,IOPT,4)
      !IF(ICKUP(INP).GT.0.AND.IOPT.NE.2.AND.IOPT.NE.3.AND.IOPT.NE.5 &
      !  .AND.IOPT.NE.6) &
      !  CALL ERRPRT(INP,ITYUP(INP),ICKUP(INP),ITY,INUM,IOPT,5)
      !~~Redo this with new ICKRS dimensions
      !IF(ICKRS(INP).GT.0.AND.IOPT.NE.2.AND.IOPT.NE.3.AND.IOPT.NE.5 &
      !.AND.IOPT.NE.6) &
      !      CALL ERRPRT(INP,ITYRS(INP),ICKRS(INP),ITY,INUM,IOPT,6)
      IF(IOPT.EQ.1)THEN
       ICKIN(INP)=INUM
       ITYIN(INP)=ITY
      ELSE IF(IOPT.EQ.2)THEN
       ICKOU(INP)=INUM
       ITYOU(INP)=ITY
      ELSE IF(IOPT.EQ.3)THEN
       ICKBY(INP)=INUM
       ITYBY(INP)=ITY
      ELSE IF(IOPT.EQ.4)THEN
       ICKGW(INP)=INUM
       ITYGW(INP)=ITY
      ELSE IF(IOPT.EQ.5)THEN
       ICKUP(INP)=INUM
       ITYUP(INP)=ITY
      ELSE IF(IOPT.EQ.6)THEN
       DO JJ=1,MRES
        IF(ICKRS(INP,JJ).EQ.0)THEN
          ICKRS(INP,JJ)=INUM
          EXIT
        END IF
       END DO
       ITYUP(INP)=ITY
      ENDIF
      RETURN
      END
      SUBROUTINE ERRPRT(INP,ITYI,IOUT,ITY,INUM,IOPT,I1)
      use PARAMDIM
      use PrintStuff
       INTEGER*4 LMES(3),LFLO(6),L1,I1,ITYI,L3,L4,IOPT,ITY
      INTEGER*4 L2,INP,IOUT,INUM
      CHARACTER UTYPE(3)*20,UOPT(6)*20,MES1*20,MES2*20,MES3*20,MES4*20
      DATA UTYPE/'land area ','reservoir ','hydropower plant '/
      DATA UOPT/ ' inflow to ',' outflow from ',' groundwater to ', &
       ' bypass by ',' upstream flow of ',' outflow from '/
      DATA LMES/11,11,17/LFLO/11,14,16,11,17,14/
      IF(ITYI.EQ.1.AND.((IOPT.EQ.4.AND.I1.EQ.3).OR.(IOPT.EQ.3.AND.I1.EQ.4)) &
        .OR.(ITYI.EQ.1.AND.IOPT.EQ.5.AND.I1.EQ.4))THEN
        RETURN
      ENDIF
      L1=LFLO(I1)
      L2=LMES(ITYI)
      L3=LFLO(IOPT)
      L4=LMES(ITY)
      MES1=UOPT(I1)
      MES2=UTYPE(ITYI)
      MES3=UOPT(IOPT)
      MES4=UTYPE(ITY)
      WRITE(6,*)'QX(',INP,') is assigned to', &
       MES1(1:L1),MES2(1:L2),IOUT, &
       'and cannot be also be assigned to', &
       MES3(1:L3),MES4(1:L4),INUM
      WRITE(0,*)'QX(',INP,') is assigned to', &
       MES1(1:L1),MES2(1:L2),IOUT, &
       'and cannot be also be assigned to', &
       MES3(1:L3),MES4(1:L4),INUM
      ISTOP=.TRUE.
      END
      CHARACTER*80 FUNCTION GETPAT(FILE)
!***********************************************************************
!
!     UTAH DIVISION OF WATER RESOURCES
!
!     WRITTEN:  10-21-1992  BY: CWM
!     MODIFIED: 10-21-1992  BY: CWM
!
!     DESCRIPTION:
!        Returns the path of a file name FILE.
!***********************************************************************
      CHARACTER*(*) FILE
      INTEGER*4 IPOS,IBAR
      IPOS=1
 10   IF(IPOS.GT.NBLANK(FILE))THEN
       GETPAT=FILE
       RETURN
      ENDIF
      IBAR=MAX(INDEX(FILE(IPOS:),':'),INDEX(FILE(IPOS:),'\'))
      IF(IBAR.GT.0)THEN
       IPOS=IPOS+IBAR
       GOTO 10
      ENDIF
      IF(IPOS.EQ.1)THEN
       GETPAT=' '
      ELSE
       GETPAT=FILE(1:IPOS-1)
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
!     JUSTEM
!----------------------------------------------------------------------
      CHARACTER*8 FUNCTION JUSTEM(FILENM)
      INTEGER*4 NLEN,NDOT,NBAR,I
      CHARACTER FILENM*(*),RET*8
      NLEN=LEN(CHARNB(FILENM))
      NDOT=0
      DO I=NLEN,MAX(1,NLEN-3),-1
          IF(FILENM(I:I).EQ.'.')THEN
              NDOT=I
              EXIT
          ENDIF
      ENDDO
      IF(NDOT.GT.0)THEN
       RET=FILENM(NDOT-8:NDOT-1)
      ELSE
       RET=FILENM(NLEN-7:NLEN)
      ENDIF
      NBAR=MAX(INDEX(RET,':'),INDEX(RET,'\'))
      IF(NBAR.GT.0)THEN
       RET=RET(NBAR+1:)
      ENDIF
      JUSTEM=RET
      RETURN
      END
!***********************************************************************
!     NONULL
!***********************************************************************
      CHARACTER*80 FUNCTION NONULL(STRING)
      CHARACTER*(*) STRING,RET*80
      INTEGER*2 ILEN,I
      RET=' '
      ILEN=MIN(80,LEN(STRING))
      DO I=1,ILEN
       IF(ICHAR(STRING(I:I)).EQ.0)THEN
          RET(I:I)=' '
       ELSE
          RET(I:I)=STRING(I:I)
       ENDIF
      ENDDO
      NONULL=RET
      RETURN
      END
!-----------------------------------------------------------------------
      CHARACTER*10 FUNCTION PRNTIT(VAL,LVAL,ILEN,IDEC)
!***********************************************************************
!
!     UTAH DIVISION OF WATER RESOURCES
!
!     WRITTEN:  10-20-1994  BY: CWM
!     MODIFIED: 10-20-1994  BY: CWM
!
!     DESCRIPTION:
!        Returns a five character string to be printed
!     VARIABLES:
!
!***********************************************************************
      CHARACTER*40 FMT
      REAL*8 VAL
      INTEGER*4 IDEC,IVAL,ILEN
      LOGICAL*1 LVAL
      IF(LVAL)THEN
       IF(IDEC.GT.0)THEN
          WRITE(FMT,"('(F',I2.2,'.',I2.2,')')")ILEN,IDEC
          WRITE(PRNTIT(1:ILEN),FMT)VAL
       ELSE
          WRITE(FMT,"('(I',I2.2,')')")ILEN
          IVAL=NINT(VAL)
          WRITE(PRNTIT(1:ILEN),FMT)IVAL
       ENDIF
      ELSE
       PRNTIT='     '
      ENDIF
      RETURN
      END

REAL (KIND=8) FUNCTION ROUND(X,IX)
    REAL (KIND=8),INTENT(IN) :: X
    INTEGER (KIND=4),INTENT(IN) :: IX
    ROUND = JIDINT( ABS(X)*10.**IX + 0.5 ) / 10.**IX * SIGN(1.,X)
RETURN
END

!----------------------------------------------------------------------
!     ALLTRIM is a character function used to trim leading and trailing
!     blanks.
!----------------------------------------------------------------------
  CHARACTER (LEN=255) FUNCTION ALLTRIM(CHARIN)
  use F90SQLVARIABLES
  CHARACTER*(*) CHARIN
  INTEGER*4 SM,BG,THELEN
  DATA SM,BG/33,127/
  INTEGER*4 I,ILEN,IBEG,ILEN2,J,IVAL,ILAST
  CHARACTER*255 RES,TheLetter*1
  ILEN=MIN(255,LEN(trim(CHARIN)))
  ILAST=ILEN
  RES=CHARIN
  IBEG=1
  DO I=1,ILAST
     IF (IACHAR(RES(I:I)).GE.SM .AND.IACHAR(RES(I:I)).LT.BG) THEN
        EXIT
     ELSE
        IBEG=MIN(I+1,ILEN)
     END IF
  ENDDO
  ILAST=IBEG+1
  DO I=IBEG+1,ILEN
     TheLetter=RES(I:I)
     IVAL=IACHAR(RES(I:I))
     IF (IACHAR(RES(I:I)).LT.SM-1 .OR.IACHAR(RES(I:I)).GE.BG) THEN
        ILAST=MAX(1,I-1)
        EXIT
     ELSE
        ILAST=I
     END IF
  END DO
  ALLTRIM=trim(RES(IBEG:ILAST))
  RETURN
  END

      CHARACTER*5 FUNCTION JUSTEXT(FILE)
!***********************************************************************
!
!     UTAH DIVISION OF WATER RESOURCES
!
!     WRITTEN:  12-30-1992  BY: CWM
!     MODIFIED: 12-30-1992  BY: CWM
!
!     DESCRIPTION:
!        Given a filename, returns the extension
!***********************************************************************
  CHARACTER*(*) FILE
  INTEGER*4 NDOT
  CHARACTER*200 ALLTRIM
  NDOT=INDEX(FILE,'.')
  IF(NDOT.GT.0)THEN
     JUSTEXT=TRIM(ALLTRIM(FILE(NDOT+1:)))
  ELSE
     JUSTEXT=' '
  ENDIF
  RETURN
  END

  LOGICAL (KIND=4) FUNCTION StrComp(Str1,Str2)
  IMPLICIT NONE
  INTEGER (KIND=4) :: i
  CHARACTER*(*) Str1
  CHARACTER*(*) Str2
  character (len=len(Str1)) :: Str1a
  character (len=len(Str2)) :: Str2a
  Str1a=Str1
  Str2a=Str2
  do i=1, LEN(str1a)
    IF (Str1a(i:i) >='a' .AND. Str1a(i:i)<='z') THEN
        Str1a(i:i) = achar(iachar(Str1a(i:i))-32)
    END IF
  end do
  do i=1, LEN(str2a)
    IF (Str2a(i:i) >='a' .AND. Str2a(i:i)<='z') THEN
        Str2a(i:i) = achar(iachar(Str2a(i:i))-32)
    END IF
  end do
  IF (Str1a==Str2a) THEN
     StrComp=.TRUE.
  ELSE
     StrComp=.FALSE.
  END IF
  RETURN
  END

!-------------------------------------------------------------------------
!Subroutine to initialize variables for ACCESSREAD
!Initializes the number of each variable and the field names
!-------------------------------------------------------------------------
subroutine InitializeAccessVars()
use FSQL_DATA
NDPFIELD=0
Field=" "
NDATEFIELD = 0
DATEFIELDS= " "
NSTRINGFIELD = 0
STRINGFIELDS = " "
NLOGICFIELD = 0
LOGICFIELDS = " "
return
end subroutine

subroutine swap(Val1,Val2)
real (Kind=4)::tempval
real (Kind=4),INTENT(INOUT) :: Val1,Val2
tempval=Val1
Val1=Val2
Val2=tempval
end subroutine

SUBROUTINE WriteCrop(CropAC,CropDesc)
use PrintStuff
REAL (KIND=8),INTENT(IN) :: CropAC
CHARACTER (len=*),INTENT(IN) :: CropDesc
CHARACTER :: CropType*30
if (CropAC>0.001) then
  IF (index(CropDesc,"Sub")>0) THEN
    CropType="Sub-Irrigated Crop"
  ELSEIF (index(CropDesc,"Rip")>0) THEN
    CropType="Wetland Vegetation"
  ELSE
    CropType="Irrigated Crop"
  END IF
  WRITE(6,"(2X,A40,1X,F10.1,3X,A20)") &
          CropDesc, CropAC,CropType
  LINE=LINE+1
end if
END SUBROUTINE

SUBROUTINE StoreMonVar(MonVar,ll,nn,mm,iNum,Opt,InArray,oo,pp,iStart,theYear)
! This subroutine stores access calendar data as water year or calendar data
!ll  = First dimension of MonVar(ll,nn,mm)
!nn  = Second dimension of MonVar(ll,nn,mm)
!mm  = Third 
!Opt = 1, MonVar has the following subscripts, iNum,J,K
!Opt = 2, MonVar has the following subscripts, K,J,iNum
!oo - first dimension of InArray(oo,pp)
!pp - second dimension of InArray(oo,pp)
!iStart
!iYear is the subscript number of the F90SQLINTEGER to use
USE PrintStuff
use F90SQLConstants
use F90SQL
use F90SQLVARIABLES
use FSQL_DATA
INTEGER (KIND=4),INTENT(IN) :: ll,nn,mm,oo,pp
REAL (kind=8),INTENT(INOUT) :: MonVar(ll,nn,mm)
REAL (kind=8),INTENT(IN) :: InArray(oo,pp)
INTEGER (kind=4),INTENT(IN) :: Opt,iNum,theYear,iStart
INTEGER (kind=4) :: ind,KK,KJ,ii
ReadIt=.FALSE.
do ii=1,KOUNTFSQL
  do KJ=1,12
    SELECT CASE (TypeYear)
      CASE (WaterYear)
        IF (KJ.gt.9) THEN
          ind=F90SQLINTEGER(ii,theYear)-INYR+2
          KK=KJ-9
        else
          ind=F90SQLINTEGER(ii,theYear)-INYR+1
          KK=KJ+3
        END IF
      CASE (WaterYearNov)
        IF (KJ.gt.10) THEN
          ind=F90SQLINTEGER(ii,theYear)-INYR+2
          KK=KJ-10
        else
          ind=F90SQLINTEGER(ii,theYear)-INYR+1
          KK=KJ+2
        END IF
      CASE (CalendarYear)
        KK=KJ
        ind=F90SQLINTEGER(ii,1)-INYR+1
    END SELECT
    IF (IND.GT.0.AND.IND.LE.NYRS) THEN
      if (Opt.eq.1) then
        MonVar(iNum,ind,KK)=InArray(ii,KJ+iStart-1)
      else IF(Opt.eq.2) then
        MonVar(kk,ind,iNum)=InArray(ii,KJ+iStart-1)
      end if
      ReadIt(ind,kk)=.TRUE.
    END IF
  end do
end do
AverVal=0.0
AverNum=0.0
do ii=1,NYRS
    do KK=1,12
        IF (ReadIt(ii,kk)) THEN
          if (Opt.eq.1) then
            AverVal(kk)=AverVal(kk)+MonVar(iNum,ii,kk)
          else if (Opt.eq.2) then
            AverVal(kk)=AverVal(kk)+MonVar(kk,ii,iNum)
          end if
          AverNum(kk)=AverNum(kk)+1.0
        END IF
    end do
end do
do kk=1,12
  AverVal(kk)=AverVal(kk)/AverNum(kk)
  AverVal(13)=AverVal(13)+AverVal(kk)
end do
do ii=1,NYRS
  if (Opt.EQ.1) then
    MonVar(iNum,ii,13)=0.0
  else if (Opt.EQ.2) then
    MonVar(13,ii,iNum)=0.0
  end if
  DO kk=1,12
    if (.NOT.ReadIt(ii,kk)) then
      if (Opt.eq.1) then
        MonVar(iNum,ii,kk)=AverVal(kk)
      else if(Opt.eq.2) then
        MonVar(kk,ii,iNum)=AverVal(kk)
      end if
    end if
    if (Opt.eq.1) then
      MonVar(iNum,ii,13)=MonVar(iNum,ii,13)+MonVar(iNum,ii,kk)
    else if(Opt.eq.2) then
      MonVar(13,ii,iNum)=MonVar(13,ii,iNum)+MonVar(kk,ii,iNum)
    end if
  end do
end do
END SUBROUTINE

SUBROUTINE StoreMonVarXI(MonVar,ll,nn,mm,iNum,Opt,iStart,theYear)
! This subroutine stores access calendar data as water year or calendar data
!ll  = First dimension of MonVar(ll,nn,mm)
!nn  = Second dimension of MonVar(ll,nn,mm)
!mm  = Third 
!Opt = 1, MonVar has the following subscripts, iNum,J,K
!Opt = 2, MonVar has the following subscripts, K,J,iNum
!iStart
!iYear is the subscript number of the F90SQLINTEGER to use
USE PrintStuff
use F90SQLConstants
use F90SQL
use F90SQLVARIABLES
use FSQL_DATA
INTEGER (KIND=4),INTENT(IN) :: ll,nn,mm
REAL (kind=8),INTENT(INOUT) :: MonVar(ll,nn,mm)
INTEGER (kind=4),INTENT(IN) :: Opt,iNum,theYear,iStart
INTEGER (kind=4) :: ind,KK,KJ,ii
ReadIt=.FALSE.
do ii=1,KOUNTFSQL
  do KJ=1,12
    SELECT CASE (TypeYear)
      CASE (WaterYear)
        IF (KJ.gt.9) THEN
          ind=F90SQLINTEGER(ii,theYear)-INYR+2
          KK=KJ-9
        else
          ind=F90SQLINTEGER(ii,theYear)-INYR+1
          KK=KJ+3
        END IF
      CASE (WaterYearNov)
        IF (KJ.gt.10) THEN
          ind=F90SQLINTEGER(ii,theYear)-INYR+2
          KK=KJ-10
        else
          ind=F90SQLINTEGER(ii,theYear)-INYR+1
          KK=KJ+2
        END IF
      CASE (CalendarYear)
        KK=KJ
        ind=F90SQLINTEGER(ii,1)-INYR+1
    END SELECT
    IF (IND.GT.0.AND.IND.LE.NYRS) THEN
      if (Opt.eq.1) then
        MonVar(iNum,ind,KK)=XI(ii,KJ+iStart-1)
      else IF(Opt.eq.2) then
        MonVar(kk,ind,iNum)=XI(ii,KJ+iStart-1)
      end if
      ReadIt(ind,kk)=.TRUE.
    END IF
  end do
end do
AverVal=0.0
AverNum=0.0
do ii=1,NYRS
    do KK=1,12
        IF (ReadIt(ii,kk)) THEN
          if (Opt.eq.1) then
            AverVal(kk)=AverVal(kk)+MonVar(iNum,ii,kk)
          else if (Opt.eq.2) then
            AverVal(kk)=AverVal(kk)+MonVar(kk,ii,iNum)
          end if
          AverNum(kk)=AverNum(kk)+1.0
        END IF
    end do
end do
do kk=1,12
  AverVal(kk)=AverVal(kk)/AverNum(kk)
  AverVal(13)=AverVal(13)+AverVal(kk)
end do
do ii=1,NYRS
  if (Opt.EQ.1) then
    MonVar(iNum,ii,13)=0.0
  else if (Opt.EQ.2) then
    MonVar(13,ii,iNum)=0.0
  end if
  DO kk=1,12
    if (.NOT.ReadIt(ii,kk)) then
      if (Opt.eq.1) then
        MonVar(iNum,ii,kk)=AverVal(kk)
      else if(Opt.eq.2) then
        MonVar(kk,ii,iNum)=AverVal(kk)
      end if
    end if
    if (Opt.eq.1) then
      MonVar(iNum,ii,13)=MonVar(iNum,ii,13)+MonVar(iNum,ii,kk)
    else if(Opt.eq.2) then
      MonVar(13,ii,iNum)=MonVar(13,ii,iNum)+MonVar(kk,ii,iNum)
    end if
  end do
end do
END SUBROUTINE

SUBROUTINE StoreMon(iNum,Opt,iStart,theYear)
! This subroutine stores access calendar data as water year or calendar data
!ll  = First dimension of MonVar(ll,nn,mm)
!nn  = Second dimension of MonVar(ll,nn,mm)
!mm  = Third 
!Opt = 1, MonVar has the following subscripts, iNum,J,K
!Opt = 2, MonVar has the following subscripts, K,J,iNum
!oo - first dimension of InArray(oo,pp)
!pp - second dimension of InArray(oo,pp)
!iStart
!iYear is the subscript number of the F90SQLINTEGER to use
USE SubVars
USE PrintStuff
use F90SQLConstants
use F90SQL
use F90SQLVARIABLES
use FSQL_DATA
INTEGER (kind=4),INTENT(IN) :: Opt,iNum,theYear,iStart
INTEGER (kind=4) :: ind,KK,iSub,KJ,ii
REAL (KIND=8) :: theValue
ReadIt=.FALSE.
iSub=iNum
do ii=1,KOUNTFSQL
  do KJ=1,12
    SELECT CASE (TypeYear)
      CASE (WaterYear)
        IF (KJ.gt.9) THEN
          ind=F90SQLINTEGER(ii,theYear)-INYR+2
          KK=KJ-9
        else
          ind=F90SQLINTEGER(ii,theYear)-INYR+1
          KK=KJ+3
        END IF
      CASE (WaterYearNov)
        IF (KJ.gt.10) THEN
          ind=F90SQLINTEGER(ii,theYear)-INYR+2
          KK=KJ-10
        else
          ind=F90SQLINTEGER(i,theYear)-INYR+1
          KK=KJ+2
        END IF
      CASE (CalendarYear)
        KK=KJ
        ind=F90SQLINTEGER(ii,1)-INYR+1
    END SELECT
    IF (IND.GT.0.AND.IND.LE.NYRS) THEN
      theValue=MAX(0.0,XI(ii,KJ+iStart-1))
      SubPrecip(iSub,ind,KK)=theValue
      ReadIt(ind,kk)=.TRUE.
    END IF
  end do
end do
AverVal=0.0
AverNum=0.0
do ii=1,NYRS
    do KK=1,12
        IF (ReadIt(ii,kk)) THEN
          if (Opt.eq.1) then
            AverVal(kk)=AverVal(kk)+SubPrecip(iSub,ii,kk)
          else if (Opt.eq.2) then
            AverVal(kk)=AverVal(kk)+SubPrecip(kk,ii,iSub)
          end if
          AverNum(kk)=AverNum(kk)+1.0
        END IF
    end do
end do
do kk=1,12
  AverVal(kk)=AverVal(kk)/AverNum(kk)
  AverVal(13)=AverVal(13)+AverVal(kk)
end do
do ii=1,NYRS
  SubPrecip(iSub,ii,13)=0.0
  DO kk=1,12
    if (.NOT.ReadIt(ii,kk)) then
      SubPrecip(iSub,ii,kk)=AverVal(kk)
    end if
    SubPrecip(iSub,ii,13)=SubPrecip(iSub,ii,13)+SubPrecip(iSub,ii,kk)
  end do
end do
END SUBROUTINE

CHARACTER (LEN=255) FUNCTION JustRight(CharIn,CharLen)
CHARACTER (LEN=*),INTENT(IN) :: CharIn
INTEGER (KIND=4),INTENT(IN) :: CharLen
CHARACTER (LEN=255) :: CharCopy,Spaces
CHARACTER (LEN=255) :: AllTrim
DATA SPACES/"                                                        "/
integer (kind=4) :: theLen
CharCopy=AllTrim(CharIn)
theLen = len_trim(AllTrim(CharIn))
if (theLen>CharLen) then
  CharCopy=CharCopy(1:CharLen)
else
  CharCopy=SPACES(1:(CharLen-theLen)) // CharCopy(1:theLen)
end if
JustRight=CharCopy
end function

CHARACTER (LEN=255) FUNCTION NoComma(CharIn)
CHARACTER (LEN=*),INTENT(IN) :: CharIn
CHARACTER (LEN=255) :: CharCopy,Spaces
CHARACTER (LEN=255) :: AllTrim
DATA SPACES/"                                                        "/
integer (kind=4) :: theLen,II
CharCopy=AllTrim(CharIn)
theLen= len_trim(AllTrim(CharIn))
do II = 1,theLen
  if (CharCopy(II:II).EQ.",") Then
    CharCopy(II:II)=" "
  end if
end do
NoComma=CharCopy
END
