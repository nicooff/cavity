c-----------------------------------------------------------------------
c   
c   2D Poiseuille flow
c   Velocity inlet
c   Pressure outlet
c
c-----------------------------------------------------------------------
      subroutine uservp (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      utrans = 0.0
      udiff = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userf  (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      ffx = 0.0
      ffy = 0.0
      ffz = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userq  (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      qvol = 0.0
      source = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userchk
      include 'SIZE'
      include 'TSTEP'           ! ISTEP, IOSTEP, NBDINP, TIME, DT
      include 'SOLN'            ! VX, VY, VZ, T
      include 'INPUT'           ! IF3D, IFTO, IFPSCO, PARAM
      include 'CTIMER'          ! tcrsl

      return
      end
c-----------------------------------------------------------------------
      subroutine userbc (ix,iy,iz,iside,ieg)
c     NOTE ::: This subroutine MAY NOT be called by every process

C     Set boundary conditions

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

c     Smoothing function for the velocity along the lid in order to avoid a
c     gap in the x-velocity at the corners:

c     ux = 0 if x <= delta0 or x >= 1-delta0
c     ux is smoothed if delta0 < x < delsmo or 1-delsmo < x < 1-delta0
c     ux=1 if delsmo <= x <= 1-delsmo

      real delta0
      real delsmo
      
      delta0 = 0.001 
      delsmo = 0.1

      if (X.le.delta0) then
         UX= 0.0
      elseif(X.lt.delsmo) then
         arg = X/delsmo
         UX=1./(1.+exp(1./(arg-1.)+1./arg))
      elseif (X.le.1.0-delsmo) then
         UX=1.0
      elseif (X.lt.(1-delta0)) then
         arg = (1.0-X)/delsmo
         UX= 1./(1.+exp(1./(arg-1.)+1./arg))
      else
         UX=0.0
      endif
      UY = 0.0
      UZ = 0.0

      temp = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine useric (ix,iy,iz,ieg)

C     Set initial conditions

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      ux   = 0.0
      uy   = 0.0
      uz   = 0.0

      temp = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat
      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat2
      include 'SIZE'
      include 'TOTAL'
c     param(66) = 4
c     param(67) = 4
      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat3
      return
      end
c-----------------------------------------------------------------------
c
c automatically added by makenek
      subroutine usrsetvert(glo_num,nel,nx,ny,nz) ! to modify glo_num
      integer*8 glo_num(1)
      return
      end
c
c automatically added by makenek
      subroutine cmt_switch ! to set IFCMT logical flag
      include 'SIZE'
      include 'INPUT'
      IFCMT=.false.
      return
      end
c
c automatically added by makenek
      subroutine usrflt(rmult) ! user defined filter
      include 'SIZE'
      real rmult(lx1)
      call rone(rmult,lx1)
      return
      end
c
c automatically added by makenek
      subroutine userflux ! user defined flux
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'
      real fluxout(lx1*lz1)
      return
      end
c
c automatically added by makenek
      subroutine userEOS ! user defined EOS 
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      return
      end
