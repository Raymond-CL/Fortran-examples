      program main
      use vint
      implicit none
      double precision, dimension(4) :: region
      double precision :: tgral,sd,chi2a
      double precision :: func
      integer :: init,ncall,itmx,nprn
      external :: func
      real :: t1,t2
      double precision :: xmin,xmax,ymin,ymax,ans

      xmin = 0d0
      xmax = 50d0
      ymin = 1d0
      ymax = 50d0

      ans = (bessel_j0(xmin)-bessel_j0(xmax)) *
     &      (bessel_y0(ymin)-bessel_y0(ymax))

      nprn = -1
      region(1) = xmin
      region(2) = ymin
      region(3) = xmax
      region(4) = ymax

      write(*,*) "VEGAS adaptive Monte-Carlo test"
      write(*,*) "ans:",ans,new_line('a')

      ! plain integration
      write(*,*) "plain:"
      call cpu_time(t1)
      ncall = 2e8;  itmx = 1;   init = -1
      write(*,*) "ncall:",ncall,"it:",itmx
      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      call cpu_time(t2)
      write(*,*) tgral, sd, abs(ans-tgral), t2-t1, new_line('a')

      ! iterative integration
      write(*,*) "iterative:"
      call cpu_time(t1)
      ncall = 2e7;  itmx = 10;   init = -1
      write(*,*) "ncall:",ncall,"it:",itmx
      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      call cpu_time(t2)
      write(*,*) tgral, sd, abs(ans-tgral), t2-t1, new_line('a')

      ! adaptive integration
      write(*,*) "adaptive:"
      call cpu_time(t1)
      ncall = 1e4;  itmx = 10;   init = -1
      write(*,*) "ncall:",ncall,"it:",itmx
      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      ncall = 2e8;  itmx = 1;   init = +1
      write(*,*) "ncall:",ncall,"it:",itmx
      call vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      call cpu_time(t2)
      write(*,*) tgral, sd, abs(ans-tgral), t2-t1, new_line('a')

      end program main


      function func(dx,wgt)
      implicit none
      double precision, dimension(:), intent(in) :: dx
      double precision, intent(in) :: wgt
      double precision :: func
      double precision :: x,y
      x = dx(1)
      y = dx(2)
      func = bessel_j1(x)*bessel_y1(y)
      return
      end function func
