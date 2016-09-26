module Path
    
    implicit none
    
    SAVE
    
    logical :: isReadFile=.false.
    character(len=80) :: pathfile
    integer :: iPathSize=5096
    integer :: iPathN=0
    integer :: lastPathN=0
    double precision :: pathLength=0
    double precision :: lastTime=-1D0
    double precision :: lastLength=-1D0
    double precision :: lastX
    double precision :: lastY
    double precision :: lastPower
    double precision, dimension(5096) :: XAM, YAM, CAM, POW

  contains

  subroutine initPath(pathf)
    character(*), intent(in)::  pathf
    integer :: i, ok
    double precision :: x,y,lpath,d
    
    pathfile       = trim(pathf)  
    open(1,file=pathfile,status='old',iostat=ok)
    if (ok /= 0) then
       write(0,*) 'Cannot open file ', pathfile
       stop
    end if
        
    lpath=0D0
    i=0
    do
       read(1,*,iostat=ok) x, y
       if (ok /= 0) exit
       i=i+1
       XAM(i)=x
       YAM(i)=y
       if(i==1)then
          d=0D0
       else
          d=sqrt( (x-XAM(i-1))**2 + (y-YAM(i-1))**2 )
       endif
       lpath=lpath+d
       CAM(i)=lpath
       iPathN=i
       pathLength=lpath
!       write(*,*) 'x=',x,' y=',y,' C=',CAM(i)
    enddo
    isReadFile=.true.
    
    return
  end subroutine initPath

  subroutine initTime(pathf)
    character(*), intent(in)::  pathf
    integer :: i, ok
    double precision :: x,y,lpath,d,pwr
    
    pathfile       = trim(pathf)  
    open(1,file=pathfile,status='old',iostat=ok)
    if (ok /= 0) then
       write(0,*) 'Cannot open file ', pathfile
       stop
    end if
        
    lpath=0D0
    i=0
    do
! x, y, time duration at xy, power
       read(1,*,iostat=ok) x, y, d, pwr
       if (ok /= 0) exit
       i=i+1
       XAM(i)=x
       YAM(i)=y

       lpath=lpath+d
! Store cumulative time into array otherwise used for cumulative path
! length
       CAM(i)=lpath
! Store power at the point
       POW(i)=pwr
       iPathN=i
! Total tile
       pathLength=lpath
       write(*,1001) 'i=',i,' x=',x,' y=',y,' C=',CAM(i),' P=',POW(i)
1001   format(A3,i3,A4,e12.4,A4,e12.4,A4,e12.4,A4,e12.4)
    enddo
    isReadFile=.true.
    
    return
  end subroutine initTime

subroutine initTimeMultiPt(pathf,d)
    character(*), intent(in)::  pathf
    integer :: i, ok
    double precision :: x,y,lpath,d

    pathfile       = trim(pathf)
    open(1,file=pathfile,status='old',iostat=ok)
    if (ok /= 0) then
       write(0,*) 'Cannot open file ', pathfile
       stop
    end if

    lpath=0D0
    i=0
    do
! x, y, time duration at xy, power
       read(1,*,iostat=ok) x, y
       if (ok /= 0) exit
       i=i+1
       XAM(i)=x
       YAM(i)=y

       lpath=lpath+d
! Store cumulative time into array otherwise used for cumulative path
! length
       CAM(i)=lpath
       iPathN=i
! Total tile
       pathLength=lpath
       write(*,1001) 'i=',i,' x=',x,' y=',y,' C=',CAM(i)
1001   format(A3,i3,A4,e12.4,A4,e12.4,A4,e12.4)
    enddo
    isReadFile=.true.

    return
  end subroutine initTimeMultiPt

  logical function isPathSet() result(pstate)
    pstate=isReadFile
  end function isPathSet

end module Path
