!
	PROGRAM Maxwell_Boltzmann
!
! Purpose: The purpose of this program is to compute f(v;T) with respect to the Maxwell-Boltzmann
! distribution
!
! History:
! Programmer: Ken Luu	Date: 04/04/17	Version: 1.0	Description: Created from the depths of Mordor
!
!	Input:
!	Quantity:		Units:		Description:
!	- - - - - - -	- - - - - 	- - - - - - - 
!	Temperature		C			inputting the Celsius temperature but converting to Kelvin from the code
!
!	Output:
!	Quantity:		Units:		Description:
!	- - - - - - - 	- - - - -	- - - - - - - 
!	f(v,T)			unit-less	this is our output of the giving function that we want to calculate
!	trap			unit-less	this is the trapezoid rule to calculate the integral
!
!	Intrinsic functions:
!	exponential function :EXP(X) where 'X' is a real number
!
!	Coding Begins

	IMPLICIT NONE
	REAL :: a, b, vprime, pi, k, T, m, m_N, m_O, m_Ar, constant, sum, vi, vj, h, f
	REAL :: trap, f_vt
	INTEGER :: i, N, j
	
	f(vprime) = exp(-(m*vprime**(2))/(2.*k*T)) * vprime**(2) !our function
	pi = acos(-1.)
	
	Open(unit=10, file="f(v,T).dat", status="unknown") !opening an open statement to graph in xmgrace 
	
	
	a = 0. !our lower interval 
	b = 5000. !max interval
	N = 100 !step size 

	k = 8.319 !Boltzmann constant 
	m_N = 28./1000. !converting Nitrogen g/ml to kg/mol
	m_O = 32./1000. !converting Oxygen g/mol to kg/mol
	m_Ar = 40./1000. !converting Argon g/mol to kg/mol
	m = (m_N*.78) + (m_O*.21) + (m_Ar*.01) ! the average mass
	
	
	WRITE(*,*) "Enter the value of T (in Celsius):" !inputting the Celsius temperature
	READ(*,*) T
	WRITE(*,*) T
	
	T = T + 273.15 !converting the input Celsius temperature to Kelvin
	constant = 4.*pi*(m/(2.*pi*k*T))**(3./2.) !this is the constant that is being multiplied to the integral
	
	Do j = 0, 5000, 100 !our do loop to graph
	
	vj = float(j) !our x-coordinates for the graph
		
		sum = 0.0 !initializing the integral sum 
		Do i = 1, N-1 !calculating the integral
		vi = (vj/float(N))*float(i) !small step size for the integral 
		sum = sum + f(vi)
		End Do
		
		trap = (vj/float(N))/2.*(f(a) + f(vj) + 2.*sum) !the trapezoidal rule 
		f_vt = constant * trap !the function we want
		
		WRITE(10,*) vj, f_vt !writing out the x-coordinate and the function to graph 
	End Do
	 
	
	End Program Maxwell_Boltzmann 
		
		
		
		
		
		
		
		
		
	
	