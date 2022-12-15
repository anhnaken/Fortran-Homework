PROGRAM rocketequation
!
!
! Purpose: the program of this program is to deal with functions and subroutines 
!
! History:
! Version: 1.0, 	Programmer: Ken Luu		Date: 04/03/17		Description: Created from the depths of Mordor
!
! Input: 
! Quantity:			Units:			Description:
! - - - - - 		- - - - -		- - - - - - - 
! t_initial			hours			the input time 
! t_increment		hours			the incremental time
! t_final			hours			the final time 
!
! Output:
! Quantity:			Units:			Description:
! - - - - - -		- - - - - 		- - - - - - - 
! xi				km				the x-axis that is changing with the loop 
! h_t				km				the altitude
! v_t				m/s				the velocity
! a_t				m/s^2			the acceleration
!
!
! ******** CODING BEGINS ******** !


	IMPLICIT NONE
	REAL :: h, v, a, hmax, hinital, xi, h_t, v_t, a_t, t, tmax
	INTEGER :: i, t_final, t_increment, t_initial 
	
	h(t) = -0.12*t**(4) + 12.*t**(3) - 380.*t**(2) + 4100.*t + 220. !the position function
	v(t) = 4*(-0.12)*t**(3) + 36.*t**(2) - 2.*(380)*t +4100. ! the velocity function
	a(t) = 12.*(-0.12)*t**(2) + 72.*t - 2.*(380.) ! the acceleration function
	
	WRITE(*,*) "Input the initial time:" ! starts at 0 
	READ(*,*) t_initial
	WRITE(*,*) t_initial 
	
	WRITE(*,*) "Input the incremental time:" ! the input is 1 hour 
	READ(*,*) t_increment
	WRITE(*,*) t_increment 
	
	WRITE(*,*) "Inprut the final time:" !the final time is 48 hours 
	READ(*,*) t_final
	WRITE(*,*) t_final
	
	Open(unit=10, file="PositionVsTime.dat", status="unknown")
	Open(unit=11, file="VelocityVsTime.dat", status="unknown")
	Open(unit=12, file="AccelerationVsTime.dat", status="unknown")
	
	hmax = h(float(t_initial)) !initializing the height 
	
	Do i = t_initial, t_final, t_increment !starting our do loop for the ballon
	xi = 1.*i

	h_t = h(xi) ! the position 
	v_t = v(xi) ! the velocity
	a_t = a(xi) ! the acceleration
	
	If(h_t > hmax) Then !logic statements for the ballon 
	hmax = h_t; tmax = xi !hmax is the maximum height that the ballon can have 
	Else
	hmax = hmax; tmax = tmax
	End If
	
	v_t = v_t * 1./3600. !conversion to seconds
	
	a_t = a_t * (1./3600.)**(2) !conversion to seconds
	
	Write(10,*) xi, h_t !writing to output file
	Write(11,*) xi, v_t	!writing to output file
	Write(12,*) xi, a_t	!writing to output file
	
	End Do

	
 Print 101, tmax, hmax
 101 format( "Max time:", F10.6, 2X, "Max height:" F10.2)
 
 Close(10)
 Close(11)
 Close(12)
 
 End PROGRAM rocketequation
 
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
