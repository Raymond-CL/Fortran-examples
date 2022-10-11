      MODULE vint

      private
      INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
      INTEGER, PARAMETER :: SP = KIND(1.0)
      INTEGER, PARAMETER :: DP = KIND(1.0D0)
      INTEGER(I4B), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
      INTEGER(I4B), PARAMETER :: hg=huge(1_I4B), hgm=-hg, hgng=hgm-1
      INTEGER(I4B), SAVE :: lenran=0, seq=0
      INTEGER(I4B), SAVE :: iran0,jran0,kran0,nran0,mran0,rans
      INTEGER(I4B), DIMENSION(:,:), POINTER, SAVE :: ranseeds
      INTEGER(I4B), DIMENSION(:), POINTER, SAVE :: iran,jran,kran, 
     &   nran,mran,ranv
      REAL(DP), SAVE :: amm
      public :: vegas

      INTERFACE reallocate
        MODULE PROCEDURE reallocate_iv,reallocate_im
      END INTERFACE
      INTERFACE arth
        MODULE PROCEDURE arth_d,arth_i
      END INTERFACE

      CONTAINS

      FUNCTION reallocate_iv(p,n)
      INTEGER(I4B), DIMENSION(:), POINTER :: p, reallocate_iv
      INTEGER(I4B), INTENT(IN) :: n
      INTEGER(I4B) :: nold,ierr
      allocate(reallocate_iv(n),stat=ierr)
      if (ierr /= 0) call
     &  nrerror('reallocate_iv: problem in attempt to allocate memory')
      if (.not. associated(p)) RETURN
      nold=size(p)
      reallocate_iv(1:min(nold,n))=p(1:min(nold,n))
      deallocate(p)
      END FUNCTION reallocate_iv     
      FUNCTION reallocate_im(p,n,m)
      INTEGER(I4B), DIMENSION(:,:), POINTER :: p, reallocate_im
      INTEGER(I4B), INTENT(IN) :: n,m
      INTEGER(I4B) :: nold,mold,ierr
      allocate(reallocate_im(n,m),stat=ierr)
      if (ierr /= 0) call
     &  nrerror('reallocate_im: problem in attempt to allocate memory')
      if (.not. associated(p)) RETURN
      nold=size(p,1)
      mold=size(p,2)
      reallocate_im(1:min(nold,n),1:min(mold,m))=
     & p(1:min(nold,n),1:min(mold,m))
      deallocate(p)
      END FUNCTION reallocate_im 

      SUBROUTINE nrerror(string)
      CHARACTER(LEN=*), INTENT(IN) :: string
      write (*,*) 'nrerror: ',string
      STOP 'program terminated by nrerror'
      END SUBROUTINE nrerror  

      FUNCTION arth_d(first,increment,n)
      REAL(DP), INTENT(IN) :: first,increment
      INTEGER(I4B), INTENT(IN) :: n
      REAL(DP), DIMENSION(n) :: arth_d
      INTEGER(I4B) :: k,k2
      REAL(DP) :: temp
      if (n > 0) arth_d(1)=first
      if (n <= NPAR_ARTH) then
        do k=2,n
          arth_d(k)=arth_d(k-1)+increment
        end do
      else
        do k=2,NPAR2_ARTH
          arth_d(k)=arth_d(k-1)+increment
        end do
        temp=increment*NPAR2_ARTH
        k=NPAR2_ARTH
        do
          if (k >= n) exit
          k2=k+k
          arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
          temp=temp+temp
          k=k2
        end do
      end if
      END FUNCTION arth_d
      FUNCTION arth_i(first,increment,n)
      INTEGER(I4B), INTENT(IN) :: first,increment,n
      INTEGER(I4B), DIMENSION(n) :: arth_i
      INTEGER(I4B) :: k,k2,temp
      if (n > 0) arth_i(1)=first
      if (n <= NPAR_ARTH) then
        do k=2,n
          arth_i(k)=arth_i(k-1)+increment
        end do
      else
        do k=2,NPAR2_ARTH
          arth_i(k)=arth_i(k-1)+increment
        end do
        temp=increment*NPAR2_ARTH
        k=NPAR2_ARTH
        do
          if (k >= n) exit
          k2=k+k
          arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
          temp=temp+temp
          k=k2
        end do
      end if
      END FUNCTION arth_i

      SUBROUTINE ran_init(length)
      IMPLICIT NONE
      INTEGER(I4B), INTENT(IN) :: length
      INTEGER(I4B) :: new,j,hgt
      if (length < lenran) RETURN
      hgt=hg
      if (hg /= 2147483647) 
     &  call nrerror('ran_init: arith assump 1 fails')
      if (hgng >= 0)        
     &  call nrerror('ran_init: arith assump 2 fails')
      if (hgt+1 /= hgng)    
     &  call nrerror('ran_init: arith assump 3 fails')
      if (not(hg) >= 0)     
     &  call nrerror('ran_init: arith assump 4 fails')
      if (not(hgng) < 0)    
     &  call nrerror('ran_init: arith assump 5 fails')
      if (hg+hgng >= 0)     
     &  call nrerror('ran_init: arith assump 6 fails')
      if (not(-1_i4b) < 0)  
     &  call nrerror('ran_init: arith assump 7 fails')
      if (not(0_i4b) >= 0)  
     &  call nrerror('ran_init: arith assump 8 fails')
      if (not(1_i4b) >= 0)  
     &  call nrerror('ran_init: arith assump 9 fails')
      if (lenran > 0) then
        ranseeds=>reallocate(ranseeds,length,5)
        ranv=>reallocate(ranv,length-1)
        new=lenran+1
      else
        allocate(ranseeds(length,5))
        allocate(ranv(length-1))
        new=1
        amm=nearest(1.0_sp,-1.0_sp)/hgng
        if (amm*hgng >= 1.0 .or. amm*hgng <= 0.0) 
     &     call nrerror('ran_init: arth assump 10 fails')
      end if
      ranseeds(new:,1)=seq
      ranseeds(new:,2:5)=spread(arth(new,1,size(ranseeds(new:,1))),2,4)
      do j=1,4
        call ran_hash(ranseeds(new:,j),ranseeds(new:,j+1))
      end do
      where (ranseeds(new:,1:3) < 0) 
     &   ranseeds(new:,1:3)=not(ranseeds(new:,1:3))
      where (ranseeds(new:,4:5) == 0) ranseeds(new:,4:5)=1
      if (new == 1) then
        iran0=ranseeds(1,1)
        jran0=ranseeds(1,2)
        kran0=ranseeds(1,3)
        mran0=ranseeds(1,4)
        nran0=ranseeds(1,5)
        rans=nran0
      end if
      if (length > 1) then
        iran => ranseeds(2:,1)
        jran => ranseeds(2:,2)
        kran => ranseeds(2:,3)
        mran => ranseeds(2:,4)
        nran => ranseeds(2:,5)
        ranv = nran
      end if
      lenran=length
      END SUBROUTINE ran_init

      SUBROUTINE ran_hash(il,ir)
      IMPLICIT NONE
      INTEGER(I4B), DIMENSION(:), INTENT(INOUT) :: il,ir
      INTEGER(I4B), DIMENSION(size(il)) :: is
      INTEGER(I4B) :: j
      do j=1,4
        is=ir
        ir=ieor(ir,ishft(ir,5))+1422217823
        ir=ieor(ir,ishft(ir,-16))+1842055030
        ir=ieor(ir,ishft(ir,9))+80567781
        ir=ieor(il,ir)
        il=is
      end do
      END SUBROUTINE ran_hash

      SUBROUTINE ran1(harvest)
      IMPLICIT NONE
      REAL(DP), INTENT(OUT) :: harvest
      if (lenran < 1) call ran_init(1)
      rans=iran0-kran0
      if (rans < 0) rans=rans+2147483579_i4b
      iran0=jran0
      jran0=kran0
      kran0=rans
      nran0=ieor(nran0,ishft(nran0,13))
      nran0=ieor(nran0,ishft(nran0,-17))
      nran0=ieor(nran0,ishft(nran0,5))
      if (nran0 == 1) nran0=270369_i4b
      mran0=ieor(mran0,ishft(mran0,5))
      mran0=ieor(mran0,ishft(mran0,-13))
      mran0=ieor(mran0,ishft(mran0,6))
      rans=ieor(nran0,rans)+mran0
      harvest=amm*merge(rans,not(rans), rans<0 )
      END SUBROUTINE ran1

      SUBROUTINE vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
      IMPLICIT NONE
      REAL(DP), DIMENSION(:), INTENT(IN) :: region
      INTEGER(I4B), INTENT(IN) :: init,ncall,itmx,nprn
      REAL(DP), INTENT(OUT) :: tgral,sd,chi2a
      INTERFACE
        FUNCTION func(pt,wgt)
        IMPLICIT NONE
        double precision, DIMENSION(:), INTENT(IN) :: pt
        double precision, INTENT(IN) :: wgt
        double precision :: func
        END FUNCTION func
      END INTERFACE
      REAL(DP), PARAMETER :: ALPH=1.5_sp,TINY=1.0e-30_sp
      INTEGER(I4B), PARAMETER :: MXDIM=10,NDMX=100
      INTEGER(I4B), SAVE :: i,it,j,k,mds,nd,ndim,ndo,ng,npg
      INTEGER(I4B), DIMENSION(MXDIM), SAVE :: ia,kg
      REAL(DP), SAVE :: calls,dv2g,dxg,f,f2,f2b,fb,rc,
     &  ti,tsi,wgt,xjac,xn,xnd,xo,harvest
      REAL(DP), DIMENSION(NDMX,MXDIM), SAVE :: d,di,xi
      REAL(DP), DIMENSION(MXDIM), SAVE :: dt,dx,x
      REAL(DP), DIMENSION(NDMX), SAVE :: r,xin
      REAL(DP), SAVE :: schi,si,swgt
      ndim=size(region)/2
      ! normal entry
      if (init <= 0) then
        mds=1  ! stratified sampling, mds=0 for importance samp. only
        ndo=1
        xi(1,:)=1.0
      end if
      ! inherit grid from previous call, not answer
      if (init <= 1) then
        si=0.0
        swgt=0.0
        schi=0.0
      end if
      ! inherit grid and answer from previous call
      if (init <= 2) then
        nd=NDMX
        ng=1
        if (mds /= 0) then
          ng=(ncall/2.0_sp+0.25_sp)**(1.0_sp/ndim)
          mds=1
          if ((2*ng-NDMX) >= 0) then
            mds=-1
            npg=ng/NDMX+1
            nd=ng/npg
            ng=npg*nd
          end if
        end if
        k=ng**ndim
        npg=max(ncall/k,2)
        calls=real(npg,sp)*real(k,sp)
        dxg=1.0_sp/ng
        dv2g=(calls*dxg**ndim)**2/npg/npg/(npg-1.0_sp)
        xnd=nd
        dxg=dxg*xnd
        dx(1:ndim)=region(1+ndim:2*ndim)-region(1:ndim)
        xjac=1.0_sp/calls*product(dx(1:ndim))
        if (nd /= ndo) then
          r(1:max(nd,ndo))=1.0
          do j=1,ndim
            call rebin(ndo/xnd,nd,r,xin,xi(:,j))
          end do
          ndo=nd
        end if
        if (nprn >= 0) write(*,200) ndim,calls,it,itmx,nprn,
     &    ALPH,mds,nd,(j,region(j),j,region(j+ndim),j=1,ndim)
      end if
      ! main iteration loop, can enter here with everything unchanged
      do it=1,itmx
        ti=0.0
        tsi=0.0
        kg(:)=1
        d(1:nd,:)=0.0
        di(1:nd,:)=0.0
        iterate: do
          fb=0.0
          f2b=0.0
          do k=1,npg
            wgt=xjac
            do j=1,ndim
              call ran1(harvest)  ! call random generator
              xn=(kg(j)-harvest)*dxg+1.0_sp
              ia(j)=max(min(int(xn),NDMX),1)
              if (ia(j) > 1) then
                xo=xi(ia(j),j)-xi(ia(j)-1,j)
                rc=xi(ia(j)-1,j)+(xn-ia(j))*xo
              else
                xo=xi(ia(j),j)
                rc=(xn-ia(j))*xo
              end if
              x(j)=region(j)+rc*dx(j)
              wgt=wgt*xo*xnd
            end do
            f=wgt*func(x(1:ndim),wgt)
            f2=f*f
            fb=fb+f
            f2b=f2b+f2
            do j=1,ndim
              di(ia(j),j)=di(ia(j),j)+f
              if (mds >= 0) d(ia(j),j)=d(ia(j),j)+f2
            end do
          end do
      !if(f2b*npg .lt. 0d0) call nrerror('vegas sqrt error 1!')
          f2b=sqrt(f2b*npg)
          f2b=(f2b-fb)*(f2b+fb)
          if (f2b <= 0.0) f2b=TINY
          ti=ti+fb
          tsi=tsi+f2b
          if (mds < 0) then
            do j=1,ndim
              d(ia(j),j)=d(ia(j),j)+f2b
            end do
          end if
          do k=ndim,1,-1
            kg(k)=mod(kg(k),ng)+1
            if (kg(k) /= 1) cycle iterate
          end do
          exit iterate
        end do iterate
        tsi=tsi*dv2g  ! compute result from this iteration
        wgt=1.0_sp/tsi
        si=si+real(wgt,dp)*real(ti,dp)
        schi=schi+real(wgt,dp)*real(ti,dp)**2
        swgt=swgt+real(wgt,dp)
        tgral=si/swgt
        chi2a=max((schi-si*tgral)/(it-0.99_dp),0.0_dp)
      !if(swgt .lt. 0d0) call nrerror('vegas sqrt error 2!')
        sd=sqrt(1.0_sp/swgt)
      !if(tsi .lt. 0d0) call nrerror('vegas sqrt error 3!')
        tsi=sqrt(tsi)
        if (nprn >= 0) then
          write(*,201) it,ti,tsi,tgral,sd,chi2a
          if (nprn /= 0) then
            do j=1,ndim
              write(*,202) j,(xi(i,j),di(i,j),
     &          i=1+nprn/2,nd,nprn)
            end do
          end if
        end if
        ! grid refinement
        do j=1,ndim
          xo=d(1,j)
          xn=d(2,j)
          d(1,j)=(xo+xn)/2.0_sp
          dt(j)=d(1,j)
          do i=2,nd-1
            rc=xo+xn
            xo=xn
            xn=d(i+1,j)
            d(i,j)=(rc+xn)/3.0_sp
            dt(j)=dt(j)+d(i,j)
          end do
          d(nd,j)=(xo+xn)/2.0_sp
          dt(j)=dt(j)+d(nd,j)
        end do
        where (d(1:nd,:) < TINY) d(1:nd,:)=TINY
        do j=1,ndim
          r(1:nd)=((1.0_sp-d(1:nd,j)/dt(j))
     &      /(log(dt(j))-log(d(1:nd,j))))**ALPH
          rc=sum(r(1:nd))
          call rebin(rc/xnd,nd,r,xin,xi(:,j))
        end do
      end do
      ! print procedures
200   format(/' input parameters for vegas:  ndim=',i3,'  ncall=',f12.0
     &  /28x,'  it=',i5,'  itmx=',i5
     &  /28x,'  nprn=',i3,'  alph=',f5.2/28x,'  mds=',i3,'   nd=',i4
     &  /(30x,'xl(',i2,')= ',g11.4,' xu(',i2,')= ',g11.4))
201   format(/' iteration no.',I3,': ','integral =',g14.7,' +/- ',g9.2,
     &  /' all iterations:   integral =',g14.7,' +/- ',g9.2,
     &  ' chi**2/it''n =',g9.2)
202   format(/' data for axis ',I2/'    X       delta i       ',
     &  '   x       delta i       ','    x       delta i       ',
     &  /(1x,f7.5,1x,g11.4,5x,f7.5,1x,g11.4,5x,f7.5,1x,g11.4))
        CONTAINS
        ! vegas subroutine for rebin xi->xin defined by r vector
        SUBROUTINE rebin(rc,nd,r,xin,xi)
        IMPLICIT NONE
        REAL(DP), INTENT(IN) :: rc
        INTEGER(I4B), INTENT(IN) :: nd
        REAL(DP), DIMENSION(:), INTENT(IN) :: r
        REAL(DP), DIMENSION(:), INTENT(OUT) :: xin
        REAL(DP), DIMENSION(:), INTENT(INOUT) :: xi
        INTEGER(I4B) :: i,k
        REAL(DP) :: dr,xn,xo
        k=0
        xo=0.0
        dr=0.0
        do i=1,nd-1
          do
            if (rc <= dr) exit
            k=k+1
            dr=dr+r(k)
          end do
          if (k > 1) xo=xi(k-1)
          xn=xi(k)
          dr=dr-rc
          xin(i)=xn-(xn-xo)*dr/r(k)
        end do
        xi(1:nd-1)=xin(1:nd-1)
        xi(nd)=1.0
        END SUBROUTINE rebin
      END SUBROUTINE vegas

      END MODULE vint
