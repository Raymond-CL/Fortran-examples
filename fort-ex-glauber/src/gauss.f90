module gauss

! Gaussian-Legendre quadrature 38 and 50 points
! for simple 1D integration
! gaussian weights and abscissa are pre-calculated for efficiency

use nrtype
implicit none
private
public :: gauss38, gauss50

contains

subroutine gauss38(XINI,XFIN,XN,WN)
real(sp), intent(in) :: XINI,XFIN
real(sp), intent(out) :: XN(38),WN(38)
real(sp) :: X(38), W(38)
integer(i4b) :: I
DATA   &
  X(38)/9.980499305357E-01 /, &
  X(37)/9.897394542664E-01 /, &
  X(36)/9.748463285902E-01 /, &
  X(35)/9.534663309335E-01 /, &
  X(34)/9.257413320486E-01 /, &
  X(33)/8.918557390046E-01 /, &
  X(32)/8.520350219324E-01 /, &
  X(31)/8.065441676053E-01 /, &
  X(30)/7.556859037540E-01 /, &
  X(29)/6.997986803792E-01 /, &
  X(28)/6.392544158297E-01 /, &
  X(27)/5.744560210478E-01 /, &
  X(26)/5.058347179279E-01 /, &
  X(25)/4.338471694324E-01 /, &
  X(24)/3.589724404794E-01 /, &
  X(23)/2.817088097902E-01 /, &
  X(22)/2.025704538921E-01 /, &
  X(21)/1.220840253379E-01 /, &
  X(20)/4.078514790458E-2 /
DATA   &
  W(38)/5.002880749632E-3/, &
  W(37)/1.161344471647E-2/, &
  W(36)/1.815657770961E-2/, &
  W(35)/2.457973973823E-2/, &
  W(34)/3.083950054518E-2/, &
  W(33)/3.689408159400E-2/, &
  W(32)/4.270315850467E-2/, &
  W(31)/4.822806186076E-2/, &
  W(30)/5.343201991033E-2/, &
  W(29)/5.828039914700E-2/, &
  W(28)/6.274093339213E-2/, &
  W(27)/6.678393797914E-2/, &
  W(26)/7.038250706690E-2/, &
  W(25)/7.351269258474E-2/, &
  W(24)/7.615366354845E-2/, &
  W(23)/7.828784465821E-2/, &
  W(22)/7.990103324353E-2/, &
  W(21)/8.098249377060E-2/, &
  W(20)/8.152502928039E-2/
DO I = 1 ,19
  X(I)= -X(39 - I)
  W(I)= W( 39 - I )
ENDDO
DO I = 1 ,38
  XN(I) =(XFIN-XINI)*X(I)/2+(XINI+XFIN)/2
  WN(I) =(XFIN-XINI)*W(I)/2
ENDDO
RETURN
end subroutine gauss38

subroutine gauss50(XINI,XFIN,XN,WN)
real(sp), intent(in) :: XINI,XFIN
real(sp), intent(out) :: XN(50),WN(50)
real(sp) :: X(50), W(50)
integer :: I
DATA   &
  X(50)/0.9988664044200710/, &
  X(49)/0.9940319694320907/, &
  X(48)/0.9853540840480058/, &
  X(47)/0.9728643851066920/, &
  X(46)/0.9566109552428079/, &
  X(45)/0.9366566189448780/, &
  X(44)/0.9130785566557919/, &
  X(43)/0.8859679795236131/, &
  X(42)/0.8554297694299461/, &
  X(41)/0.8215820708593360/, &
  X(40)/0.7845558329003993/, &
  X(39)/0.7444943022260685/, &
  X(38)/0.7015524687068222/, &
  X(37)/0.6558964656854394/, &
  X(36)/0.6077029271849502/, &
  X(35)/0.5571583045146501/, &
  X(34)/0.5044581449074642/, &
  X(33)/0.4498063349740388/, &
  X(32)/0.3934143118975651/, &
  X(31)/0.3355002454194373/, &
  X(30)/0.2762881937795320/, &
  X(29)/0.2160072368760418/, &
  X(28)/0.1548905899981459/, &
  X(27)/0.0931747015600861/, &
  X(26)/0.0310983383271889/
DATA  &
  W(50)/0.0029086225531551/, &
  W(49)/0.0067597991957454/, &
  W(48)/0.0105905483836510/, &
  W(47)/0.0143808227614856/, &
  W(46)/0.0181155607134894/, &
  W(45)/0.0217802431701248/, &
  W(44)/0.0253606735700124/, &
  W(43)/0.0288429935805352/, &
  W(42)/0.0322137282235780/, &
  W(41)/0.0354598356151462/, &
  W(40)/0.0385687566125877/, &
  W(39)/0.0415284630901477/, &
  W(38)/0.0443275043388033/, &
  W(37)/0.0469550513039484/, &
  W(36)/0.0494009384494663/, &
  W(35)/0.0516557030695811/, &
  W(34)/0.0537106218889962/, &
  W(33)/0.0555577448062125/, &
  W(32)/0.0571899256477284/, &
  W(31)/0.0586008498132224/, &
  W(30)/0.0597850587042655/, &
  W(29)/0.0607379708417702/, &
  W(28)/0.0614558995903167/, &
  W(27)/0.0619360674206832/, &
  W(26)/0.0621766166553473/
DO I = 1 ,25
  X(I)= -X(51 - I)
  W(I)= +W(51 - I)
ENDDO
DO I = 1 ,50
  XN(I) =(XFIN-XINI)*X(I)/2+(XINI+XFIN)/2
  WN(I) =(XFIN-XINI)*W(I)/2
ENDDO
RETURN
end subroutine gauss50

end module gauss
