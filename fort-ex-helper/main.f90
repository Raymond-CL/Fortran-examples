program main
  use, intrinsic :: iso_fortran_env, only: stdout => output_unit
  use nrtype
  use fourmom
  use fourmomop
  implicit none
  real(dp) :: m,pT,y,phi
  real(dp) :: pe,px,py,pz
  type(pvec) :: p,p1,p2
  character, parameter :: tab = char(9)

  write(*,*) 'helper modules'

  write(*,*) 'testing 4-momentum vector:'

  m = 0d0 !mass
  pT = 30d0 !transverse momentum
  y = +0.5d0  !rapidity
  phi = atan(1d0) !azimuth

  ! convert to four-vector
  pe = sqrt(m*m + pT*pT) * cosh(y)
  pz = sqrt(m*m + pT*pT) * sinh(y)
  px = pT * cos(phi)
  py = pT * sin(phi)
  
  ! set four-vector to pvec object
  call p%setp(pe,px,py,pz)

  write(*,*) 'mass, pt, rap, phi'
  write(*,*) m,pT,y,phi
  write(*,*) p%mag(),p%perp(),p%rap(),p%phi()

  write(*,*) 'pe, px, py, pz'
  write(*,*) pe,px,py,pz
  write(*,*) p%E(),p%X(),p%Y(),p%Z()
  call p%printp(stdout)

  write(*,*) '+/-:',tab,tab,p%plus(),p%minus()
  write(*,*) 'rap/prap:',tab,p%rap(),p%prap()
  write(*,*) 'pol ang:',tab,p%theta(),p%costh()

  ! test binary operations
  call p1%setp(4d0,3d0,2d0,1d0)
  call p2%setp(8d0,7d0,6d0,5d0)
  call p1%printp(stdout)
  call p2%printp(stdout)

  write(*,*) 'assignment:'
  p2 = p1
  call p1%printp(stdout)
  call p2%printp(stdout)

  write(*,*) 'dot product:',dot(p1,p2)

end program main

