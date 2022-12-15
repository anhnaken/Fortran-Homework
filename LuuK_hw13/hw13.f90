!
!
!
!	The purpose of this program is to deal with integral equations of the second kind
!
!	History:
!		Programmer:			Version:			Date:			Description:
! 	----------------- 		---------- 			-------			-----------------------------------
!	Ken Luu						1.0				04/29/17		Created from the depths of Mordor
!
!	Input:
! 			Quantity:			Units:			Description:
!			- - - - - -			- - - - -		- - - - - - - - - 	
!		there are no inputs		none			no keyboard inputs 
!
!
!	Output:
!			Quantity:					Units:			Description:
!			- - - - - - -				- - - - - - 	- - - - - - - - - 
!	xi		discretized values			unit-less		my discretized steps 
!	f		the function				unit-less		the function I solved for 
!	PSI1	the psi array				unitless		the "old" psi
!	PSI2	the psi array				unit-less		the "new" psi
!	it		the iteration				unitless		the iteration
!	itmax	the max iteration 			unitless		the iteration
!	trap	dummy variable for integral unitless
!	Kxy		the kernel 					unitless
!
!
!
!
!
!	***** CODING BEGINS *****

	Program Rach
	
	IMPLICIT NONE
	INTEGER, PARAMETER :: N = 100
	INTEGER :: it, itmax, l, j, i
	REAL :: PSI1(0:N), PSI2(0:N), f, x, y, fx, kxy, r, k, trap, integral, diff
	REAL :: delta, xi, yj, xk, xl
	
	Open(unit=10, file="Psiplots.dat", status="unknown")
	
	it = 0 ; itmax = 1; delta = 0.0001
	
	

! this do loop is for the initial psi array	
	Do i = 0, N
	
	xi = float(i)/float(N)
	f = fx(xi) 
	PSI1(i) = f 
	
	End Do
! doing an implicit do loop so that I can do the number of iterations 	

111 CONTINUE
	
it = it + 1

! this do loop is for the outer loop, comprising both the functions and the kernel 	
 Do i = 0, N
	
	xi = float(i)/float(N) 
    
    trap = 0. !initializing the sum for the trapezoidal rule  

!I am now calculating the integral 	

	 Do j = 1, N - 1
	
	yj = float(i)/float(N) 
	
	K = kxy(xi,yj) !this is our kernel 
	
	trap = trap + K*PSI1(j)
	 
	 End Do  
	!integral is the integrand that I am solving for 
	
	integral = (1./(2.*float(N))) * (Kxy(xi,0.)*PSI1(0) + Kxy(xi,1.)*PSI1(100) + 2.*trap)
	
	PSI2(i) = fx(xi) + integral  !this is my new psi with respect to the given function
	
	
	!writing output statements so that I can see some data points 
	
	Write(*,*) "Iteration number:", it, "Diff:", Diff
	
	Write(*,*) "Total number of iteration = ", itmax
	
	Write(*,*) "i=", i, "x=", xi, "psi_old", PSI1(i), "psi_new", PSI2(i) 
	
	
End Do 
	!initializing my "diff" so I can start my if statement (down)
	Diff = 0.
	! doing a do loop for the difference function 
	
	Do l = 0, N
	
	
	diff = (abs(PSI2(l) - PSI1(l))/(abs(PSI2(l))))
	
	
	diff = abs(diff)
	
	PSI1(l) = PSI2(l) ! this is setting the old value equaling the new value 
	
	End Do
	
	!this program is for the "N" value of iterations, if you want to get a different plot,
	!simply change the iteration, for example right now the max iterations is 15 but if you want the plot for 1
	!then put itmax = 1 and if you want the plot for 3 then put itmax = 3
	
	If (it == itmax) then 
	
	!this is my do loop so that I can write to an output statement 
	
	Do i = 0, N
	xi = float(i)/float(N)
	
	Write(10,*) xi, PSI2(i)
	
	End Do
	
	Write(*,*) "Error"
	
	Goto 777
	
	End if 
	
	! if the difference is greater than the delta then keep running the program 
	
	If (diff > delta) go to 111
	
	
	
777 stop 
	
	End Program Rach 
	
	
	!solving the function with respect to x 
	FUNCTION fx(x)
	Implicit None 
	Real :: fx, x 
	
	fx = (x**(2)*sin(x) - cos(x) +2.*sin(x))*exp(-x)/(1.+x**(2))
	
	Return 
	End FUNCTION fx
	
	!solving the kernel with respect to x,y

	FUNCTION kxy(x,y)
	Implicit None
	Real :: kxy, y, x
	
	kxy = y/(1.+x**(2))
	
	Return 
	End FUNCTION kxy

	
	
	
	
	
	
	