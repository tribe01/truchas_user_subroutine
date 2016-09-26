double precision function qzahpath (v, p) bind(c)
  use lib_array
  use Path
  implicit none

  ! input variables
  ! v( t, x, y, z)
  ! model parameters, eta = eta_b*eta_e)
  ! p=(speed, eta, pb, db, s, ztop, qwarn)

  double precision v(*), p(*)

  logical :: isread
  character(len=80) :: filename
  integer :: idx
  double precision :: x,gammax,gammay,gammaz
  double precision :: x2, y2, z2, z
  double precision :: pln, prefac, db2
  double precision :: tfil=0

  pln=4.0*2.30258509299
  db2=p(4)*p(4)
  prefac=p(2)*p(3)*pln/(3.14159265359*db2*p(5))

  ! check if path is read into memory
  isread=isPathSet()
  if(.not.isread)then
     write(*,*) 'Path is not set'
     filename="xyz.dat"
     call initPath(filename)
     ! size of the read path
     ! iPathN=number of linear segments
     ! CAM(1:iPathN)=incremental cumulative distance along the path
     write(*,*) 'iPathN= ', iPathN, ' pathLength= ', pathLength
     write(*,*) 'CAM(1)= ', CAM(1), 'CAM(iPathN)= ', CAM(iPathN)
  endif

  x=p(1)*v(1)      ! current distance along the length of the curve, speed * time

  ! to avoid recalculating coordinates for the same time
  if(lastTime /= v(1))then
     ! locate starting index of a segment for given distance along path length 
     idx = locate(CAM(1:iPathN), x)
     if(idx .le. 0)then
 !       write(*,*) 'Length ', x, 'out of bounds.'
        qzahpath=0D0
        return
     endif

!     write(*,*) 'idx = ', idx, CAM(idx), CAM(idx+1)

     ! interpolate x coordinate from XAM based on CAM and length
     gammax=interp1d(CAM(idx:idx+1),XAM(idx:idx+1),x,.true.,tfil)
     !  write(*,*) 'L= ', x, ' X= ', gammax, ' (', XAM(idx), ':', XAM(idx+1), ')'

     ! interpolate y coordinate
     gammay=interp1d(CAM(idx:idx+1),YAM(idx:idx+1),x,.true.,tfil)
     !  write(*,*) 'L= ', x, ' Y= ', gammay, ' (', YAM(idx), ':', YAM(idx+1), ')'
     lastTime=v(1)
     lastLength=x
     lastX=gammax
     lastY=gammay
  endif

  gammax=lastX
  gammay=lastY
  gammaz=p(6)
  qzahpath=0D0

  x2=( v(2) - gammax )**2
  y2=( v(3) - gammay )**2
  z2=( v(4) - gammaz )**2
  z=sqrt(z2)/p(5)
  if(z .ge. 1.0)then
     qzahpath=0D0
  else
     ! Truchas Physics and Algorithms, LA-UR-08-0819
     ! Prod. Eng. Res. Devel. (2010) 4:15-23, Eq. 5
     qzahpath=prefac*exp(-pln*(x2+y2)/db2)*(-3.0*z*z + 2.0*z + 1.0)
     if(qzahpath .gt. p(7))then
        write(*,*) 'Max heat exceeded ', qzahpath, prefac, pln, x2, y2, db2, z
     endif
  endif

  return
end function qzahpath
