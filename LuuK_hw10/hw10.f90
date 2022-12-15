!
	PROGRAM Just_a_PoeBoy_from_a_Poe_Family
!
! Purpose: The purpose of this program is analyze the radially driven and lengthening pendulum
!			using the language of fortran and solving differential equations
!
! History:
!	Programmer: Ken Luu		Date: 04/09/17		Version: 1.0		Description: Created from the depths of Mordor
!
! Input:
!	Quantity:		Units:		Description:
!	None			None		There are no inputs from keyboard
!
! Output:
!	Quantity:		Units:				Description:
!	x				meters				the x value from the picture
!	y				meters				the y value from the picture, or the height 
!	r_t				meter*sec			lengthening of the pendulum
!	g				meter/sec^2			acceleration of gravity
!	p1				rad					theta(0)
!	p2				rad/s				thetadot(0)
!	t_final			seconds				the given final time
!	delta_t			seconds				the step size that is given
!	
! Intrinsic Functions: 
!	SIN(X), with X being a real number
!	COS(X), with X being a real number
!
! ************ CODING BEINGS ************* !

	IMPLICIT NONE 
	REAL :: g, x, y, p1, p2, t, delta_t, t_final, r_t !variable declarations 
	
	Open(unit=10, file="Theta_t.dat", status="unknown") !opening a file to plot the theta 
	Open(unit=11, file="xy_postn.dat", status="unknown") !opening a file to plot the xy positions
	
	g = 9.81 !m/s^2
	delta_t = 0.001 ! seconds 
	t_final = 200.0 ! seconds
	
	Call Theta(x, y, g, r_t, t_final, delta_t, p1, p2) !calling a subroutine 
	
	Close(10)
	Close(11)
	
	End PROGRAM Just_a_PoeBoy_from_a_Poe_Family
	
	SUBROUTINE Theta(x, y, g, r_t, t_final, delta_t, p1, p2) !the called subroutine 
	
	IMPLICIT NONE
	REAL :: x, y, g, r_t, t_final, delta_t, p1, p2, t !variable declarations 
	
	p1 = 1.0 !rad
	p2 = 0.0 !rad/s
	t = 0.0 ! seconds
	
	Do While (t <= t_final) !we are doing a do while loop for 0<t<t_final
	t = t + delta_t ! t written with a change in step size, delta_t 
	
	r_t = 0.1*t + 10. !this is function for the lengthening pendulum as given by the worksheet
	
	p1 = p1 + p2*delta_t !this is our theta function, given as pi(t+delta_t)
	
	p2 = p2 - ((2.0 * 0.1 * p2) + g*sin(p1))/(r_t) * delta_t !this is the second function that is given
	
	x = sin(p1)*r_t !solving by using trig, we got the x value
	y = -cos(p1)*r_t	!solving by using trig, we got the y value
	
	Write(10,*) t, p1  !writing the data to the open file
	Write(11,*) x, y   !writing data to the open file 
	
	End Do
	
	
	RETURN
	End SUBROUTINE Theta
	
	
	
	