!
	PROGRAM leadconcentrations
!
! 
!
! History
!	Version		Programmer		Date:				Description
! - - - - - -  	- - - - - -		- - - - - -			- - - - - - - - - - - -
!	1.0			Ken Luu			03-07-2017			Created from my hands 
!
! Input:
!	Quantity		Units				Description
!	- - - - -  		- - - -	- - 		- - - - - - - - - - - - -
!	p				unit-less			the step size 100
!					
!										
!										
! Output:
!	Quantity		Units 				Description
! 	- - - - - 		- - - - - -			- - - - - - - - - - - - - - - - - - - - - 
!	mean			micromole/10L		this is the <x> function
!	variance		micromole/10L		this is the sigma^(2)
!	std. devi		micromole/10L		this is the sigma function (standard deviation)
!
! Special Requirements:
!	Cosine function (intrinsic function named cos(x), with real (x)
! 	exponential function (exp(x) with real (x) 
!

	IMPLICIT NONE 
	REAL :: mean, variance, standdev, probdis, a, b, x_discretize
	INTEGER :: i, j, k, n, p
	REAL, parameter :: pi = acos(-1.)
	REAL :: x(69)
	INTEGER :: STATUS = 0
	
	WRITE(*,*) "Enter the p value: " !this is the N = 100 but I already used n down below as number of lines
	READ(*,*) p
	WRITE(*,*) p
	
	Open(unit=10, file="LeadConcentrations.dat", status="unknown")
	Open(unit=11, file="ProbabilityResults.dat", status="unknown")
	
	k = 1 !initialize k
40	Read(10,*,end=70) x(k) !this is our implicit do loop
	k = k + 1 
	goto 40 
70 continue 
	n = k -1 ! number of lines in data file
	
	If ( n < 2) Then
	WRITE(*,*) "the number of input data is insufficient to carry out a statistical analysis."
	
	Call EXIT(STATUS)  
	End If
 	
 	mean= 0. !initializing the first sum for the average of x
 	
 	Do j = 1, n !I am now calculating the mean of the first given equation
 	mean = mean +x(j)
 	End Do
 	mean = (1./float(n))*mean
 	
 	variance = 0. !initializing the sum of the variance
	
	Do j = 1, n  ! I am now calculating the variance function
	variance = variance + (x(j) - mean)**2
	End Do
	
	variance = (1./(float(n)-1.)) * variance ! this is our variance
	
	standdev = sqrt(variance) ! this is our standard deviation
	
	WRITE(*,*) "The Mean of this data set is:", mean
	WRITE(*,*) "The variance of this data set is:", variance
	WRITE(*,*) "The standard deviation is:", standdev
	WRITE(*,*) "The number of data points is:", n
	
	
	a = -2. !micromole/10L
	b = 5.  !micromole/10L
	
	
	Do j = 1, p
	
	x_discretize = a + ((b-a)/float(p))*float(j) ! I am discretizing the step size 
	
	probdis = 1./(2.*variance*pi) * exp(-(x_discretize - mean)**(2)/(2.*variance)) !this is my probability distribution function
	
	WRITE(*,*) x_discretize, probdis
	WRITE(11,*) x_discretize, probdis !now we get our results to graph on xmgrace
	
	End Do
	
	Close(10)
	Close(11)
	
	End PROGRAM leadconcentrations 
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	


	
	