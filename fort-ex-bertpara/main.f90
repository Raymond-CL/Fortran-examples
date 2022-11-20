program main
  use nrtype
  use nr, only : ran1
  implicit none
  integer(i4b) :: n,i
  integer(i4b) :: naccept,nreject
  !real(dp), parameter :: PI = 4d0*atan(1d0)
  real(sp) :: side
  real(sp) :: th1,th2,x1,y1,x2,y2,rad
  real(sp) :: x,y,th,d
  real(sp) :: chord

  n = 1e+7
  write(*,*) "number of samples:",n
  side = sqrt(3d0)

  write(*,*) new_line('a'),"method 1: random endpoint"
  naccept = 0; nreject = 0
  do i=1,n
    call ran1(th1); th1 = th1 * 2d0*PI
    call ran1(th2); th2 = th2 * 2d0*PI
    x1 = cos(th1);  y1 = sin(th1)
    x2 = cos(th2);  y2 = sin(th2)
    chord = sqrt( (x1-x2)**2 + (y1-y2)**2 )
    if(chord .gt. side) naccept = naccept + 1
  enddo
  write(*,*) "longer:",naccept
  write(*,*) "probability:",dble(naccept)/dble(n)

  write(*,*) new_line('a'),"method 2: random radius"
  naccept = 0; nreject = 0
  do i=1,n
    call ran1(rad);
    chord = 2d0 * sqrt( 1d0 - rad**2 )
    if(chord .gt. side) naccept = naccept + 1
  enddo
  write(*,*) "longer:",naccept
  write(*,*) "probability:",dble(naccept)/dble(n)

  write(*,*) new_line('a'),"method 3: random mid-point"
  naccept = 0; nreject = 0
  do i=1,n
    call ran1(x); x = 2d0*x - 1d0
    call ran1(y); y = 2d0*y - 1d0
    do while( x*x+y*y.ge.1d0 )
      call ran1(x); x = 2d0*x - 1d0
      call ran1(y); y = 2d0*y - 1d0
    enddo
    call ran1(th); th = th * 2d0*PI
    d = abs( y*cos(th) - x*sin(th) )
    chord = 2d0 * sqrt( 1d0 - d**2 )
    if(d.ge.1d0) then
      write(*,*) 'something wrong', chord
    endif
    if(chord .gt. side) naccept = naccept + 1
  enddo
  write(*,*) "longer:",naccept
  write(*,*) "probability:",dble(naccept)/dble(n)

end program main

