module SEE
  use ModuleParticleBoundary
  Use Constants
  Use ModuleParticleBundle
   implicit none	
	!Type :: SEEone
	!	Type(ParticleBundle),,Allocatable :: PB(:)
	!	END TYPE
	Type Gas
        Integer(4) ::  NSpecy,Shift
        Real(8) :: MGas,TGas=300.d0,PGas=50.d0,BetaMax 
    End Type  Gas		
	CONTAINS
	
	Subroutine  Selectron(PB,PBDO)
               Implicit none
               class(ParticleBundle),intent(inout) :: PB
               Type(ParticleBoundaryOne),intent(inout) :: PBDO
               Type(ParticleOne) :: ParticleTemp
               Integer :: i,j,k,ntemp,n3,n4
               Type(Gas),parameter :: SEEGas=(Gas(1,0,9.1095d-31,3.0*11605.d0,0.d0,0.d0)) 
               Real(8) :: XRandom=0.1,VFactor,Residue,GammaR
					VFactor = 1.d0/PB%VFactor
					do i = 1,PBDO%CountMinOne
					!do i = n1,PBDO%PBLower%NPar
						if(PB%Mass==ElectronMass) THEN  !Secondary electron emmision of electron
			            PBDO%Gamma = Gamma_Caculate(PBDO%PBLower%PO(i)%Energy(PBDO%PBLower%Mass,PBDO%PBLower%VFactor))
						ELSE !Secondary electron emmision of ion
							PBDO%Gamma = 0.01
						end if
						n3 = int(PBDO%Gamma)
						GammaR = PBDO%Gamma-n3
						Call Random_NUMBER(R)
							if (R<GammaR) then
								n3 = n3+1
							end if
							PBDO%SEECountMinOne = PBDO%SEECountMinOne+n3
							j=0
							DO while (j<n3)
								Call Random_NUMBER(R)
								ParticleTemp%X = PBDO%XMin+R*XRandom
                        ParticleTemp%Ax = 0.0
                        ParticleTemp%Ay = 0.0
                        ParticleTemp%Az = 0.0
                        Call Maxwellian(SEEGas,ParticleTemp)
                        ParticleTemp%Vx = VFactor*ParticleTemp%Vx
                        ParticleTemp%Vy = VFactor*ParticleTemp%Vy
                        ParticleTemp%Vz = VFactor*ParticleTemp%Vz
                        ParticleTemp%Vx = abs(ParticleTemp%Vx)
                        call PB%AddOne(ParticleTemp)
								Call PBDO%PBLower%AddOne(ParticleTemp)
								!PBDO%SEECountMinOne = PBDO%SEECountMinOne+1
								j=j+1
							end do
					end do
					
					do i = 1,PBDO%CountMaxOne
						if(PB%Mass==ElectronMass) THEN  !Secondary electron emmision of electron
			            PBDO%Gamma = Gamma_Caculate(PBDO%PBUpper%PO(i)%Energy(PBDO%PBUpper%Mass,PBDO%PBUpper%VFactor))
						ELSE !Secondary electron emmision of ion
							PBDO%Gamma = 0.01
						end if
						n4 = int(PBDO%Gamma)
						GammaR = PBDO%Gamma-n4
						Call Random_NUMBER(R)
							if (R<GammaR) then
								n4 = n4+1
							end if
							PBDO%SEECountMaxOne = PBDO%SEECountMaxOne+n4
							j=0
							DO while (j<n4)
								Call Random_NUMBER(R)
								ParticleTemp%X = PBDO%XMax-R*XRandom
                        ParticleTemp%Ax = 0.0
                        ParticleTemp%Ay = 0.0
                        ParticleTemp%Az = 0.0
                        Call Maxwellian(SEEGas,ParticleTemp)
                        ParticleTemp%Vx = VFactor*ParticleTemp%Vx
                        ParticleTemp%Vy = VFactor*ParticleTemp%Vy
                        ParticleTemp%Vz = VFactor*ParticleTemp%Vz
                        ParticleTemp%Vx = -abs(ParticleTemp%Vx)
                        !Call AddParticle(ParticleTemp,PB)
                        call PB%AddOne(ParticleTemp)
								Call PBDO%PBUpper%AddOne(ParticleTemp)
								!PBDO%SEECountMaxOne = PBDO%SEECountMaxOne+1
								j=j+1
							end do
					end do
            return
	end subroutine Selectron
	
	subroutine Maxwellian(InputGas,InputParticle)
       implicit none
       Type(Gas),intent(in) :: InputGas
       Type(ParticleOne),intent(inout) :: InputParticle
       real(8) ::  Mass,Temprature 
       real(8) :: V,Beta,FuncA,FuncB
       real(8) :: Theta,CosTheta,SinTheta,Fai 
       Mass=InputGas%MGas
       Temprature=InputGas%TGas
       Beta=1.d0/(kB*Temprature)
       FuncA=1.d0
       FuncB=0.d0
      do while(FuncA>FuncB)
         Call Random_NUMBER(R)
              FuncA=R*R
         Call Random_NUMBER(R)
              FuncB=-exp*R*Dlog(R)
      end do
      V=DSqrt(-3.d0*Dlog(R)/Beta/Mass)
      Call RandomVelocity(V,InputParticle%Vx,InputParticle%Vy,InputParticle%Vz)
     return 
	end subroutine Maxwellian
	
	subroutine RandomVelocity(V,Vx,Vy,Vz)
       implicit none
       Real(8),intent(in) ::  V
       Real(8),intent(inout) ::  Vx,Vy,Vz  
       Real(8) :: Fai,CosFai,SinFai
       Real(8) :: Theta,CosTheta,FcosTheta,SinTheta
       Call Random_NUMBER(R)
        CosTheta = IsotropicCosKai(Theta)
        SinTheta=Dsqrt(1.d0-cosTheta*cosTheta)
        Call Random_NUMBER(R)
        Fai=2.d0*PI*R
        Vx=V*CosTheta
        Vy=V*SinTheta*DCos(Fai)
        Vz=V*SinTheta*Dsin(Fai)
       return
	end subroutine RandomVelocity 
	
	Function IsotropicCosKai(Energy) 
       Implicit none
       Real(8) :: IsotropicCosKai,Energy
       Call Random_NUMBER(R)
       IsotropicCosKai=1.d0-2.d0*R
       If(abs(IsotropicCosKai)<MinReal) then
                 If(IsotropicCosKai>0.d0)   then
                     IsotropicCosKai=MinReal
                else
                     IsotropicCosKai=-MinReal
                end if
        end  if     
       return
	end Function IsotropicCosKai	
	
	function Gamma_Caculate(Energy)!Reference :: Simulations of multipactor-assisted breakdown in radio frequency plasmas
	   implicit none
		Real(8),intent(in) :: Energy
		Real(8) :: Gamma_caculate
		Real(8) :: Ep,Em,Gamma_m
		Em = 400.d0  !eV
		Ep = Energy/JtoeV
		Gamma_m = 2.4d0
		Gamma_caculate = Gamma_m*exp*exp*(Ep/Em)*Dexp(-2.0*sqrt(Ep/Em))
		return
	end function Gamma_Caculate

end module SEE