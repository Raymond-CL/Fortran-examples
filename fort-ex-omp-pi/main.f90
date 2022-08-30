program main
  use omp_lib
  implicit none
  integer, parameter :: logn_max = 9
  !call omp_set_num_threads(2)
  write(*,*) 'Number of processors availabe: ',omp_get_num_procs()
  write(*,*) 'Number of threads available:   ',omp_get_max_threads()
  call r8_test(logn_max)
end program



subroutine r8_test(logn_max)
  use omp_lib
  implicit none
  double precision error,estimate
  integer logn,logn_max
  character(len=3) mode
  integer n
  double precision, parameter :: r8_pi = 4d0*atan(1d0)
  double precision wtime
  n=1
  do logn=1, logn_max

    mode = 'seq'
    wtime = omp_get_wtime()
    call r8_pi_est_seq(n,estimate)
    wtime = omp_get_wtime() - wtime
    error = abs(estimate-r8_pi)
    write(*,*) n,mode,estimate,error,wtime

    mode = 'omp'
    wtime = omp_get_wtime()
    call r8_pi_est_omp(n,estimate)
    wtime = omp_get_wtime() - wtime
    error = abs(estimate-r8_pi)
    write(*,*) n,mode,estimate,error,wtime
 
    n=n*10
  enddo
  return
end



subroutine r8_pi_est_seq(n,estimate)
  implicit none
  double precision h,estimate
  integer i,n
  double precision sum2,x
  h = 1d0/dble(2*n)
  sum2=0d0
  do i=1,n
    x=h*dble(2*i-1)
    sum2=sum2+1d0/(1d0+x**2)
  enddo
  estimate = 4d0*sum2/dble(n)
  return
end



subroutine r8_pi_est_omp(n,estimate)
  use omp_lib
  implicit none
  double precision h,estimate
  integer i,n
  double precision sum2,x
  h = 1d0/dble(2*n) 
  sum2=0d0
  !$omp parallel &
  !$omp shared (h,n) &
  !$omp private (i,x)
  !$omp do reduction (+:sum2)
  do i=1,n
    x=h*dble(2*i-1)
    sum2=sum2+1d0/(1d0+x**2)
  enddo
  !$omp end do
  !$omp end parallel
  estimate = 4d0*sum2/dble(n)
  return
end
