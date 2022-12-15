
!
! Purpose: The purpose of this program is to solve the coupled differential
! 		   equations for three radioactive atomic nuclei, A,B,C 
!
!
!	History:
!	Programmer:		Version:		Date:			Description:
!	- - - - - -		- - - - -		- - - - - 		- - - - - - - 
!	Ken Luu			1.0				04/25/17		Created from the depths of Mordor
!
!	Input:
!	- - - - 
!	Quantity:				Units:			Description:
!	- - - - -				- - - - -		- - - - - - -
!	There are no inputs		Unit-less		No inputs
!
!
!
!	Output:
!	- - - - - 
!	Quantity:		Units:			Description:
!	- - - - - -		- - - - -		- - - - - - - -
!	A				seconds			this is the decay function for A
!	B				seconds			this is the decay function for B
!	C				seconds			this is the decay function for C
!	T_final			seconds 		this is the given final time
!	t				seconds			this is the initialization of t for the do loop
!	dt				seconds			this is the given interval steps 
!	SumVal			seconds			this is the sum of the three decays
!	KB				unit-less		the given constant
!	KA				unit-less		the given constant 
!
! RESULTS FOR A,B,C : the results that I get for these sums are decreasing as time goes on
!					  this makes sense because as time goes on, the decaying material will lose its mass
!					  and it will become smaller and smaller until there is no substance to decay anymore
!				      if we wait long enough it will be relatively stable but still decay at a low rate. 
!
!
!
! ***** CODING BEGINS *****
!


! Creating a module so I do not have to enter KA or KB
! it is not necessary but we learned it and I think it is really useful

Module someconstants 
	
	Implicit None 
	Real, Parameter :: KA = 0.1
	Real, Parameter :: KB = 0.1
	
	End Module someconstants 
	
! Starting the program called "Winterfell" 	
	
	Program Winterfell
	Use someconstants
	
	
	Implicit None 

! variable declarations 

	Real :: dt, A, B, C, SumVal, ABC, T_final, t
	
! Opening up files so I can graph by plots 

	Open(unit=10, file="A_t.dat", status="unknown")
	Open(unit=11, file="B_t.dat", status="unknown")
	Open(unit=12, file="C_t.dat", status="unknown")
	
	
	
	! Initializing t
	t = 0. 
	
	
	T_final = 50. !seconds
	dt = 0.25 ! seconds 
	
!initial conditions 
	
	A = 1000. !given from the worksheet 
	B = 0. ; C = 0. !given from the worksheets 
	
!starting the do loop to solve the differential equations
	
	Do While ( t <= T_final)
	
	t = t + dt !this is the changing time with respect to dt
	
	A = A + (-KA*A)*dt !this is the decay function for A
	
	B = B + (KA*A - KB*B)*dt ! this is the decay function for B
	
	C = C + (KB*B)*dt ! this is the decay function for C
	
	SumVal = A + B + C  ! this is the sum of the decay functions 

!writing my data to an output file so I can plot 	

	Write(10,*) t, A
	Write(11,*) t, B
	Write(12,*) t, C
	
! I am not formatting the values to make a table on screen output 
	
	Write(*,101) t, A, B, C, SumVal
101 Format("The time:", F10.3, /, "A(t):", F20.5, / , "B(t):", F20.5, / ,  "C(t):", F20.5, /, &
		   "The Sum of A,B,C:", F20.5)
		   
	End Do  
	
		
!closing the opening statments 
	
	Close(10)
	Close(11)
	Close(12)
	
	
	
	End Program Winterfell
	
	
	
	
	