	PROGRAM RC_Circuit
!History:
!	Version:	Programmer:		Date: 		Description:
!	- - - - -	- - - - - - 	- - - - -	- - - - - - - -
!	1.0			Ken Luu			03/21-17	Created from the depths of Mordor
!
!	Input:
!	Quantity:		Units:			Description:
!	- - - - - - 	- - - - -	 	- - - - - - - -
!	No-input		unit-less		No keyboard input
!
!	Output:
!	Quantity:				Units:			Description:
!	- - - - - -				- - - - -		- - - - - - - - 
!	q_new					Coulomb			this is the charge q
!	I and I_analytic		A (amps)		this is our calculated current
!	
!	Special required functions: 
!	- - - - - - - - - - - - - - - - - - - - - 
!	exponential : exp(x) with a real x value
!	
!
! CODING BEINGS BELOW:	
! ------------------------------------------------------------------------------	
	
	Implicit None
	Real :: C, delta_t, R, t1, t2, q_new, I, t_final, U, I_analytic, q_old, heavy1, heavy2, t 
	
	Open(unit=9, file="qt.dat", status="unknown") !this is the the charge q
	Open(unit=10, file="It.dat", status="unknown") ! this is for the current I
	Open(unit=11, file="Iexact.dat", status="unknown") !this is for the I analytic
	
	R = 100. !ohms, our resistor
	
	C = 4.7E-05 !As/V, our capacitance 
	
	U = 6. !measured in V (volts)
	
	t1 = 0. !the given t1 in the homework sheet
	
	t2 = 2.*R*C !the given t2 in the homework sheet
	
	delta_t = (R*C)/200. !this is the delta t that is given in the homework sheet
	
	t_final = 4.*R*C ! this is the final time that is given in the homework sheet
	
	
	t = 0. !initial value
	q_old = 0. !initial value
	
	Do While (t < t_final)
	
	t = t + delta_t	!this is for the iteration on t
		
	If (t > t2) U = 0.
		
	
		q_new = q_old + (U - q_old/C) * (1./R ) * delta_t ! we are  constructing the new q, hence "q_new"
		
		I = (q_new - q_old)/delta_t !this is the numerical calculated current
		
		heavy1 = t - t1 ! these are the ( ) related to the Heavy-side function 
		heavy2 = t - t2 ! these are the ( ) related to the Heavy-side function
		
		If (heavy1 < 0 ) then 
			heavy1 = 0.
			Else 
			heavy1 = 1.
		End If
	    
	    If(heavy2 < 0) then	
			heavy2 = 0.
			Else
			heavy2 = 1.
		End If
		
		!we now calculate the exact "I" or "I_analytic"
		
	I_analytic = U/R * (exp(-(t-t1)/(R*C)) * heavy1 - exp(-(t-t2)/(R*C)) * heavy2)
		
	WRITE(9,*) t, q_new !writing t and q to data file
	WRITE(10,*) t, I !writing t and I to data file
	WRITE(11,*) t, I_analytic !writing t and I analytic to data file
	
	
	q_old = q_new ! this is the steps of qi+1 and qi
	
	End Do
	
	Close(9)
	Close(10)
	Close(11)
		
	End Program RC_Circuit
		
		
		
		
		
		
		
		
		
		
		
			
		
		
		
		
	

