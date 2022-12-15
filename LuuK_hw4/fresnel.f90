! 
	PROGRAM fresnelcoefficients
! 
! The purpose of this program is to calculate the fresnel coefficients 
! while incorporating do loops 
!
! History
!	Version		Programmer		Date:				Description
! - - - - - -  	- - - - - -		- - - - - -			- - - - - - - - - - - -
!	1.0			Ken Luu			02-21-2017			Created from my hands 
!
! Input:
!	Quantity		Units				Description
!	- - - - -  		- - - -	- - 		- - - - - - - - - - - - -
!	N				unit-less			the step size 100
!	Theta_ini		radians				the initial angle (0 degrees)
!	Theta_fin		radians				the final angle	  (90 degrees)
! Output:
!	Quantity		Units 				Description
! 	- - - - - 		- - - - - -			- - - - - - - - - - - - - - - - - - - - - 
!	r_s				unit-less			The fresnel coefficient r_s
!	t_s				unit-less			The fresnel coefficient t_s
!	r_p				unit-less			The fresnel coefficient	r_p
!	t_p				unit-less			The fresnel coefficient t_p
!	R_s				unit-less			The reflectance R_s
!	T_s				unit-less			The transmittance T_s
!	R_p				unit-less			The reflectance R_p
!	T_p				unit-less			The transmittance T_p
!
! Special Requirements:
!	Cosine function (intrinsic function named cos(x), with real x)
!
! - - - - - - - - - - Variable declarations - - - - - - - - - -

	IMPLICIT NONE
	INTEGER :: N, a, b, c, d, theta_ini, theta_fin
	REAL :: r_s, t_s, r_p, t_p
	REAL :: Rs, Ts, Rp, Tp
	REAL :: n_i, n_t
	REAL :: theta_a, theta_b, theta_i, theta_t

	
	open(unit = 20, file = "fres_rs.dat", status = "unknown")
	open(unit = 30, file = "fres_ts.dat", status = "unknown")
	open(unit = 40, file = "fres_rp.dat", status = "unknown")
	open(unit = 50, file = "fres_tp.dat", status = "unknown")
	open(unit = 60, file = "reflect_Rs.dat", status = "unknown")
	open(unit = 70, file = "reflect_Ts.dat", status = "unknown")
	open(unit = 80, file = "reflect_Rp.dat", status = "unknown")
	open(unit = 90, file = "reflect_Tp.dat", status = "unknown")
	
	!here we enter in from keyboard
	!N is 100
	!theta_fin is 90
	!theta_ini is 0
	
	WRITE(*,*)"Enter the N value:" 
	READ(*,*) N
	WRITE(*,*) N
	
	WRITE(*,*)"Enter theta_fin value:" 
	READ(*,*) theta_fin
	WRITE(*,*) theta_fin
	
	WRITE(*,*)"Enter theta_ini value:"
	READ(*,*) theta_ini
	WRITE(*,*) theta_ini
	
	!first loop for the r_s and Rs
	Do a = 0, N
	
	n_i = 1.0
	n_t = 1.5
	
	theta_a = theta_ini*(acos(-1.)/180.)
	theta_b = theta_fin*(acos(-1.)/180.)
	theta_t = 5.0*((acos(-1.))/180.)
	theta_i = theta_a +((theta_b-theta_a)/float(N))*float(a)
	
	r_s = (n_i*cos(theta_i)-n_t*cos(theta_t))/(n_i*cos(theta_i)+(n_t*cos(theta_t)))
	Rs = (r_s)**2
	
	WRITE(*,*) a, theta_i, r_s, Rs
	WRITE(20,*) theta_i, r_s
	WRITE(60,*) theta_i, Rs
END DO

	!second loop to get t_s and Ts
	Do b = 0, N
	 
	n_i = 1.0
	n_t = 1.5
	
	theta_a = theta_ini*(acos(-1.)/180.)
	theta_b = theta_fin*(acos(-1.)/180.)
	theta_t = 5.0*((acos(-1.))/180.)
	theta_i = theta_a +((theta_b-theta_a)/float(N))*float(b)
	 
	 t_s = 2*n_i*cos(theta_i)/(n_i*cos(theta_i)+(n_t*cos(theta_t)))
	 
	 r_s = (n_i*cos(theta_i)-(n_t*cos(theta_t)))/(n_i*cos(theta_i)+(n_t*cos(theta_t)))
	 
	 Rs = (r_s)**2
	
	 Ts = 1. - Rs
	 WRITE(*,*) b, theta_i, r_s, t_s, r_s, Rs, Ts
	 WRITE(30,*) theta_i, t_s
	 WRITE(70,*) theta_i, Ts
END DO

	!third loop to get r_p and Rp
	Do c  = 0, N
	
	n_i = 1.0
	n_t = 1.5
	
	theta_a = theta_ini*(acos(-1.)/180.)
	theta_b = theta_fin*(acos(-1.)/180.)
	theta_t = 5.0*((acos(-1.))/180.)
	theta_i = theta_a +((theta_b-theta_a)/float(N))*float(c)
	
	r_p = (n_i*cos(theta_t)-n_t*cos(theta_i))/(n_i*cos(theta_t)+(n_t*cos(theta_i)))
	
	Rp = (r_p)**2
	WRITE(*,*) c, theta_i, r_p, Rp
	WRITE(40,*) theta_i, r_p
	WRITE(80,*) theta_i, Rp
END DO

	!fourth loop to get t_p and Tp
	Do d = 0, N
	
	n_i = 1.0; n_t = 1.5
	
	theta_a = theta_ini*(acos(-1.)/180.)
	theta_b = theta_fin*(acos(-1.)/180.)
	theta_t = 5.0*((acos(-1.))/180.)
	theta_i = theta_a +((theta_b-theta_a)/float(N))*float(d)
	
	t_p = (2*n_i*cos(theta_i))/(n_i*cos(theta_t)+(n_t*cos(theta_i)))
	
	r_p = (n_i*cos(theta_t)-(n_t*cos(theta_i)))/(n_i*cos(theta_t)+(n_t*cos(theta_i)))
	
	Rp = (r_p)**2
	
	Tp = 1.-Rp
	WRITE(*,*) d, theta_i, t_p, r_p, Rp, Tp
	WRITE(50,*) theta_i, t_p
	WRITE(90,*) theta_i, Tp
END DO
close(30)
close(40)
close(50)
close(60)
close(70)
close(80)
close(90)
 
	END PROGRAM fresnelcoefficients
	
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	
	 
