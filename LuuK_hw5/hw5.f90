!
	PROGRAM wavefunctions
!
! 
! The purpose of this program is to calculate the wave functions and the wave functions
! squared of the wave functions. We continue to use do loops
!
! History
!	Version		Programmer		Date:				Description
! - - - - - -  	- - - - - -		- - - - - -			- - - - - - - - - - - -
!	1.0			Ken Luu			02-28-2017			Created from my hands 
!
! Input:
!	Quantity		Units				Description
!	- - - - -  		- - - -	- - 		- - - - - - - - - - - - -
!	N				unit-less			the step size 120
!	Theta input		radians				the initial angle of 60 degrees
!	a				nm					the first radial distance
!	b				nm					Rmax
! Output:
!	Quantity		Units 				Description
! 	- - - - - 		- - - - - -			- - - - - - - - - - - - - - - - - - - - - 
!	psi100			nm					the calculated wave functions
!	psi200			nm					the calculated wave functions
!	psi300			nm					the calculated wave functions
!	psi310			nm					the calculated wave functions
!	psi320			nm					the calculated wave functions
!	psi100sqrd		nm					the calculated wave functions (squared)
!	psi200sqrd		nm					the calculated wave functions (squared)
!	psi300sqrd		nm					the calculated wave functions (squared)
!	psi310sqrd		nm					the calculated wave functions (squared)
!	psi320sqrd		nm					the calculated wave functions (squared)
!
! Special Requirements:
!	Cosine function (intrinsic function named cos(x), with real x)
! 	exponential function (exp(x) with real x)
!
	IMPLICIT NONE
	REAL :: psi100, psi200, psi300, psi310, psi320, theta, theta_input, a_o, a, b
	REAL :: psi100sqrd, psi200sqrd, psi300sqrd, psi310sqrd, psi320sqrd
	REAL :: r
	REAL, parameter :: pi = acos(-1.)
	INTEGER :: N, i, j, k, l, m
	
	OPEN(unit=10, file="psi100.dat", status="unknown")
	OPEN(unit=20, file="psi200.dat", status="unknown")
	OPEN(unit=30, file="psi300.dat", status="unknown")
	OPEN(unit=40, file="psi310.dat", status="unknown")
	OPEN(unit=50, file="psi320.dat", status="unknown")
	OPEN(unit=60, file="psi100squared.dat",  status="unknown")
	OPEN(unit=70, file="psi200squared.dat",  status="unknown")
	OPEN(unit=80, file="psi300squared.dat",  status="unknown")
	OPEN(unit=90, file="psi310squared.dat",  status="unknown")
	OPEN(unit=100,file="psi320squared.dat",  status="unknown")
	
	WRITE(*,*) "Enter the theta input value:" !60 degrees or pi/3
	READ(*,*) theta_input
	WRITE(*,*) theta_input
	
	WRITE(*,*) "Enter the a_o value:" !bohr radius
	READ(*,*) a_o
	WRITE(*,*) a_o
	
	WRITE(*,*) "Enter the N value:" !N is 100
	READ(*,*) N
	WRITE(*,*) N
	
	
	! I know we can do all of these data in one single do loop but I kind of got carried away and forgot
	
	DO i = 0, N			! here we do the first wave function and its squared value
	a = 0.0
	b = 1.2
	
	r = a + ((b-a)/float(N))*float(i) 
	psi100 = 1./(2*sqrt(pi)) * (1./sqrt(pi)) * (1./a_o)**(3./2.) * exp(-r/a_o)
	psi100sqrd = (abs(psi100))**(2.)
	
	WRITE(*,*) r, psi100, psi100sqrd
	WRITE(10,*) r, psi100
	WRITE(60,*) r, psi100sqrd
	END DO 
	
	DO j = 0, N			! here we do the second wave function and its squared value
	a = 0.0
	b = 1.2
	
	r = a + ((b-a)/float(N))*float(j) 
	psi200 = (1./(2*sqrt(pi))) * (1./(4*sqrt(2.*pi))) * (1./a_o)**(3./2.) * (2.-r/a_o) * exp(-r/(2.*a_o))
	psi200sqrd = (abs(psi200))**(2.)
	
	WRITE(*,*) r, psi200, psi200sqrd
	WRITE(20,*) r, psi200
	WRITE(70,*) r, psi200sqrd
	END DO
	
	DO k = 0, N			! here we do the third wave function and its squared value 
	a = 0.0
	b = 1.2
	
	r = a + ((b-a)/float(N))*float(k) 
	psi300 = (1./(81.*sqrt(3*pi)))*(1./a_o)**(3./2.)*(27.-(18.*(r/a_o))+(2.*(r/a_o)**(2.)))*exp(-r/(3.*a_o))	
	psi300sqrd = (abs(psi300))**(2.)
	
	WRITE(*,*) r, psi300, psi300sqrd
	WRITE(30,*) r, psi300
	WRITE(80,*) r, psi300sqrd
	END DO
	
	DO l = 0, N			! here we do the 4th wave function and its squared value
	a = 0.0
	b = 1.2

	theta = theta_input*(pi/180.)
	r = a + ((b-a)/float(N))*float(l) 
	psi310 = (1./81.)*(sqrt(2./pi))*(1./a_o)**(3./2.)*(6.-(r/a_o))* (r/a_o) * exp(-r/(3.*a_o)) * cos(theta)
	psi310sqrd = (abs(psi310))**(2.)
	
	WRITE(*,*) r, psi310, psi310sqrd
	WRITE(40,*) r, psi310
	WRITE(90,*) r, psi310sqrd
	END DO
	
	DO m = 0, N			! here we do the 5th wave function and its squared value
	a = 0.0
	b = 1.2
	
	theta = theta_input*(pi/180.)
	r = a + ((b-a)/float(N))*float(m) 
	psi320 = (1./(81.*sqrt(6.*pi)))*(1./a_o)**(3./2.)*(r**(2.)/a_o**2.)*exp(-r/(3.*a_o))*(3.*(cos(theta))**(2.) - 1.)
	psi320sqrd = (abs(psi320))**(2.) 
	
	WRITE(*,*) r, psi320, psi320sqrd
	WRITE(50,*) r, psi320
	WRITE(100,*) r, psi320sqrd
	END DO
	
	close(10) !we now close all of our open statements that took data, now to graph 
	close(20)
	close(30)
	close(40)
	close(50)
	close(60)
	close(70)
	close(80)
	close(90)
	close(100)
	
	END PROGRAM wavefunctions  
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	