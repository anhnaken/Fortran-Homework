!
	PROGRAM Bethe_Weiz
!
! This program will be incorporating the Bethe-Weizsacker mass formula to express the atomic mass &
! it will also compute the binding, volume, surface, Coulomb, and asymmetric energy.
!
! History:
!	Version:	Programmer:		Date:			Description:
!	- - - - -	- - - - - -    	- - - - -		- - - - - - - - - -
!	1.0   		Ken Luu	      	02/08/17      	create from my hands
!
! Input:					Units:					Description:
! - - - - -	- - - - -		- - - - -				- - - - - - - - - - - - - - - - - - - -
! Z ( # of protons)			unit-less				the number of protons in a given element
! A (baryon number)			unit-less				the baryon numbers in a given element 
! a_v						MeV						experimental constant	
! a_s						MeV						experimental constant
! a_c						MeV						experimental constant
! a_A						MeV						experimental constant
! M_n						MeV						mass of neutron
! M_H						MeV						mass of hydrogen atom
!
! Output:					Units:					Description:
! - - - - -					- - - - - - -			- - - - - - - - - -
! M_ZA						MeV						mass of the atomic nucleus M
! E_B						MeV						the binding energy
! E_Volume					MeV						the volume energy
! E_Surface					MeV						the surface energy
! E_Coulomb					MeV						the Coulomb energy
! E_Asymmetry				MeV						the asymmetry energy
!
! - - - - - - - - - - - - Variable declarations - - - - - - - - - - - -
	IMPLICIT NONE 
	
	INTEGER :: Z
	INTEGER :: A
	REAL :: a_v = 15.8
	REAL :: a_s = 18.3
	REAL :: a_c = 0.71
	REAL :: a_A = 92.7
	REAL :: M_ZA
	REAL :: E_Volume
	REAL :: E_Surface  
	REAL :: E_Coulomb
	REAL :: E_Asymmetry 
	REAL :: M_n = 939.57
	REAL :: M_H = 938.78
	REAL :: E_B
	
! - - - - - - - - - - Code - - - - - - - - - - - - - - - - - - - - -

WRITE(*,*) "Input the proton number:" 
READ(*,*) Z
WRITE(*,*) Z

WRITE(*,*) "Input the baryon number:"
READ(*,*) A
WRITE(*,*) A

WRITE(*,*) "N = ", A - Z ! number of neutrons 
WRITE(*,*) "Z =", Z
WRITE(*,*) "A =", A

WRITE(*,*)"M_ZA =", Z*M_H + (A-Z)*M_n - a_v*A + a_s*A**(2./3.) + a_c*Z**(2)*A**(-1./3.) + a_A*(Z-A/2.)**(2) * A**(-1) 

E_Volume = -a_v
E_Surface = a_s*float(A)**(-1./3.)
E_Coulomb = a_c*float(Z)**2 * float(A)**(-4./3.)
E_Asymmetry = a_A*((float(Z)-float(A/2))**2)*float(A)**(-2)

WRITE(*,*)"E_B =", E_Volume + E_Surface + E_Coulomb + E_Asymmetry 

WRITE(*,*) "A =", A 
WRITE(*,*) "E_Volume =", -a_v 

WRITE(*,*) "A =", A
WRITE(*,*) "E_Surface =", a_s*A**(-1./3.) 

WRITE(*,*) "A =", A
WRITE(*,*) "E_Coulomb =", a_c*Z**2*A**(-4./3.) 

WRITE(*,*) "A =", A
WRITE(*,*) "E_Asymmetry =", a_A*((float(Z)-float(A/2))**2)*float(A)**(-2)

	END PROGRAM Bethe_Weiz





























