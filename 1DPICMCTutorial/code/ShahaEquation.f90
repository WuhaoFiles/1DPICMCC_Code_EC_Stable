!Module ShahaEquation
	!use Constants
	!Integer(4),parameter :: Gr = 2    !He2  first ionization of helium Gr+1/Gr = 1/2,
	!Integer(4),Parameter :: Gr1 = 1  !H2  Gr=2 Gr+1=1
	!Real(8),Parameter :: E_Ar = 15.7596
	!!ionization of hydrogen: ¦Ö = 13.54 eV , 2gi+1/gi = 1,
 !  !first ionization of helium: ¦Ö = 24.48 eV , 2gi+1/gi = 4,
 !  ! second ionization of helium: ¦Ö = 54.17 eV , 2gi+1/gi = 1.
	!! first ionization of Ar  ¦Ö = 15,7596
	!contains
	!subroutine SolveEquation()
	!    implicit none
	!	 Real(8) :: caculate_a
	!	 caculate_a = sqrt((2 * PI * ElectronMass * kB * TGas )**3)
	!	 
	!	 return
	!end subroutine SolveEquation
!end module