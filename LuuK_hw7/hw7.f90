!
	PROGRAM Nonlinear_rootfinding
!
!	Version:		Programmer: 			Date: 			Description:
!	1.0				Ken Luu					03/11/17		Created from the depths of Mordor 
!
!
!	Input:
!	Quantity:				Units:				Description:
!	-----------				------------ 		--------------
!	no input					nothing			the number of step size
!	
!
!	Output:
!	Quantity:				Units:				Description:
!	-----------				-------------		--------------
!	f(x)					unit-less			our output of each x-values
!
!	Special functions:
!   Square root(sqrt) with a real (x) value
!	exponential(exp) with a real (x) value					
!	Absolute value with a real (x) value



!	CODING BEGINS  !
	 
	IMPLICIT NONE
	
	REAL :: f, x, a, b, xi, h1, h2, Integral, yk, g, y, sum, f_xi, fprime, xnew, xold, delta
	INTEGER :: N, N2, i, k
	fprime(x) = 2.5*exp(-2.5*x) + sqrt(7.) + 7.5*x - 13.5*(x**2) - 17.*x*exp(-x**2) ! the derivative of the function
	f(x) = 2. - exp(-2.5*x) + sqrt(7.)*x + 3.5*(x**2) - 4.5*(x**3) - 17.*Integral  ! the given function	
	g(y) = y * exp(-y**2) !the integrand 
	
	Open(unit=10, file="Nonlinroots.dat", status="unknown")
	
	N = 100
	N2 = 10 ! for the integral 
	
	a=0. !this is the beginning x- value that we have to consider
	b=1.5 !this is the final x-value that we consider
	h1 = (b-a)/float(N) ! small height changes for the function itself
	
	
	Do i = 0, N
	xi = a + h1*float(i) ! this is the discretizing step, UPPER BOUND of the integral  
	x = xi 	
	h2 = (xi-a)/float(N2) ! the discretized height of the integral	
		
		sum = 0. ! initializing the integral 
		Do k = 1, N2 - 1 
			yk = a + h2*float(k) !discretizing the integral steps 
			sum = sum + g(yk)
		End Do
		sum = 2.*sum
		Integral = h2/2.*(g(a) + g(xi) + sum) ! the integral now depends on xi 
		
		WRITE(10,*) xi, f(x)
	End Do 
		
	
	xnew = 1. ! initializing the root finding algorithm, for the iterations  
	
	Do k = 1, 25 ! we are now doing Newton's method		
		xold = xnew! xi and xi + 1 
		h2 = (xold-a)/float(N2) ! the discretized height of the integral
		
		sum = 0. ! initializing the integral 
		Do i = 1, N2 - 1
			yk = a + h2*float(i) !discretizing the integral steps 
			sum = sum + g(yk)
		End Do
		
		sum = 2.*sum
		Integral = h2/2.*(g(a) + g(xnew) + sum) 
		 
	xnew = xold - (f(xold)/fprime(xold))
	
	delta = abs(abs(xnew) - abs(xold))
	
	Print 101, k, xold, f(xnew)
	101 format("Iteration:", I2, / , "root found = ", F10.6, 5X ,  "f(x)= " F10.6)  
	
	If(delta < 0.00001) Then
	exit ! Jump out of do loop
	End If
	End Do

 If (k == 25) Then
	WRITE(*,*) "Root cannot be found." 
	End If 
	
	Close(10)
	
End PROGRAM Nonlinear_rootfinding
	
	
	
	
	
	