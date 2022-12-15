!
	PROGRAM Magnetic_Permeability
!
!	Purpose: The purpose of this program is to calculate magnetic permeability using subroutines
!		     with the use of subroutines and best fit line.
!
!	History:
!	Programmer:		Version:		Date:		Description:
!	- - - - - - - 	- - -  - -		- - - - -	- - - - - - -
!	Ken Luu			1.0				04/18/17	Created from the depths of Mordor
!
!	Input:
!	Quantity:		Units:			Description:
!	- - - - - -		- - - - - 		- - - - - - - - 
!	None			None			There are no input data
!
!	Output:
!	Quantity:			Units:			Description:
!	- - - - - 			- - - - -		- - - - - - - - 
!	a					Unit-less		this is the a for the best fit line
!	b					Unit-less		this is the b for the best fit line
!	x(N), y(N), r(N)	Unit-less		these are my declared arrays 
!	sumr				Unit-less		this is a name for my sum
!	sumy				Unit-less		a name for a sum
!	sumry				Unit-less		a name for a sum
!	sumrr				Unit-less		a name for a sum
!	Mu_0				N*A^(-2)		the permeability 
!	PERMEABILITY 		N*A^(-2)		the permeability but this is the called function
!	xj					unit-less		the step size in the do loop
!	y_bestfit(N)		unit-less		this is the best fit line 
!	I 					Amps			the given current 
!	r					unit-less 		A dummy variable so the line can be linear 
!
!	***** CODING BEGINS *****

	IMPLICIT NONE 
	INTEGER, PARAMETER :: N = 5 ! I start out with N being 5 so I don't have to keep inputting 5
	INTEGER :: j ! this is for the do loop
	REAL :: a, b, x(N), y(N), sumr, sumy, sumry, sumrr, r(N), Mu_0, PERMEABILITY, xj
	REAL :: y_bestfit(N)
	REAL, PARAMETER :: pi = ACOS(-1.0) !parameterizing pi so that it can be used  
	REAL, PARAMETER :: I = 2.7 ! amps
	
	
	Open(unit=10, file="expdata.dat", status="unknown") !Opening a file so that I can plot the original line
	Open(unit=11, file="linearfit.dat", status="unknown")	!Opening another file so I can plot the best fit line 
	
	Data x /10.0, 20.0, 30.0, 40.0, 50.0/ !using a data statement, I am writing the data given in the homework
	Data y /5.4, 2.7, 1.8, 1.4, 1.0/ !using a data statement I am doing the same as I did in "x"

	r = 1./x ! I let r equals this so that it can be linear, because it cannot be 1/x
	
	
	Call SUMMATIONS(r, y, sumr, sumy, sumry, sumrr) !calling a subroutine 
	
	a = (sumy*sumrr - sumr*sumry)/(N*sumrr - ((sumr)**2)) !this is the "a" for the best fit line equation
	b = (N*sumry - sumr*sumy)/(N*sumrr - (sumr**2)) !the b, or the slope 
	
	
	Mu_0 = PERMEABILITY(b, I, pi) ! calling a function to solve for permeability 
	
	
	
	Do j = 1, N !opening the do loop to write to the output file
	
	r(j) = 1./x(j) ! this is depending on the j
	y_bestfit(j) = a + b*r(j) !the best fit line depends on the j 
	
	Write(10,*) r(j), y(j) !writing to the first file to plot
	Write(11,*) r(j), y_bestfit(j)  !writing to the second file to plot 
	
	End DO
	
	Close(10)
	Close(11)
	
	Write(*,101) Mu_0
	101 Format("The permeability is:", 2x, F10.5) !writing the permeability so that we can see the Mu
	
	END PROGRAM  Magnetic_Permeability
	
	SUBROUTINE SUMMATIONS(r, y, sumx, sumy, sumxy, sumxx) !here is the subroutine that solves for the sum
	IMPLICIT NONE
	INTEGER, PARAMETER :: N=5
	REAL :: r(N), y(N), sumx, sumy, sumxy, sumxx 
		
! These are my implicit do loops 
	
	sumx = sum(r) 
	sumy = sum(y)
	sumxy = sum(r*y)
	sumxx = sum(r*r)
	
	
	RETURN
	End SUBROUTINE SUMMATIONS 
	
	
	FUNCTION PERMEABILITY(b, I, pi) !here is the function that solves the permeability 
	IMPLICIT NONE
	
	REAL :: PERMEABILITY, b, I, pi
	
	PERMEABILITY = ((b * 2.0*pi)/I)/(100.) !dividing by 100 because I want it in meters 
	
	RETURN 
	END FUNCTION PERMEABILITY
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	 