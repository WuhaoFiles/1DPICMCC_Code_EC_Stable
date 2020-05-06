Module DiagParticleBouncary
	use ModuleParticleBoundary
	use ModuleGrid
	use SEE
	implicit none
	
	 Type DiagParticleBoundaryOneData
          Real(8) :: ParticleNumber(2),ParticleFlux(2),ParticleCurrent(2),Energy(2),AverageEnergy(2),PowerCurrent(2),PowerFlux(2)
			 Real(8) :: SEEParticleNumber(2),SEEParticleFlux(2),SEEParticleCurrent(2),SeeEnergy(2),SEEAverageEnergy(2),SEEPowerCurrent(2),SEEPowerFlux(2)
    EndType DiagParticleBoundaryOneData
	
	contains
	subroutine DiagParticleBoundaryOne(GD,PBDO,CF,Mode)
	    class(Grid0D(*)),intent(inout) :: GD
		 Class(ParticleBoundaryOne),intent(in) :: PBDO(:)
		 Class(ControlFlow), intent(in) :: CF
		 integer(4),intent(in) :: Mode
		 Type(DiagParticleBoundaryOneData) :: TempDPBDO
		 integer(4) :: i,shift
		 Real(8) :: TempPower(3)=0.d0,TemplePowerTotal
		 Real(8) :: TempParticleFlux(3)=0.d0
		 Select Case (Mode)
		 case(0)
			 Shift=1
			  TemplePowerTotal=0.d0
			 do i=1,CF%Ns+1
				 call DiagParticleBoundaryCaculate(PBDO(i),CF,TempDPBDO)
				 TempParticleFlux(1) = TempDPBDO%ParticleFlux(1) + TempDPBDO%ParticleFlux(2)
				 TempParticleFlux(2) = TempDPBDO%SEEParticleFlux(1) + TempDPBDO%SEEParticleFlux(2)
				 TempParticleFlux(3) = TempParticleFlux(1) - TempParticleFlux(2)
				 TempPower(1) = TempDPBDO%PowerFlux(1)+TempDPBDO%PowerFlux(2)
				 TempPower(2) = TempDPBDO%SEEPowerFlux(1)+TempDPBDO%SEEPowerFlux(2)
				 TempPower(3) = TempPower(1)-TempPower(2)
				 TemplePowerTotal = TemplePowerTotal + TempPower(3)
				 !if (i==1) then
				 Call GD%Update( TempParticleFlux(1),Shift)
				 Call GD%Update( TempParticleFlux(2),Shift)
				 Call GD%Update( TempParticleFlux(3),Shift)
				 !end if
				 Call GD%Update(TempPower(1),Shift)
				 Call GD%Update(TempPower(2),Shift)
				 Call GD%Update(TempPower(3),Shift)

			 end do
			    call GD%Update(TemplePowerTotal,Shift)
			 GD%Timer=GD%Timer+1
		 case(1)
			  Call GD%Rescale
           Call GD%Dump(1)
			  !Call GD%Reset()
			  GD%Timer = 0
			  GD%Value = 0.d0
		 case(2)
			 Call GD%Dump(0)
		 end  Select
	  return
	end subroutine DiagParticleBoundaryOne
	
	subroutine DiagParticleBoundaryCaculate(PBDO,CF,DPBDO)
	   implicit none
		Type(DiagParticleBoundaryOneData),INTENT(INOUT) :: DPBDO
		Class(ParticleBoundaryOne),intent(in) :: PBDO
		Class(ControlFlow), intent(in) :: CF
		integer(4) :: i,j
		
		DPBDO%ParticleNumber   = 0.d0
		DPBDO%SeeParticleNumber= 0.d0
		DPBDO%ParticleCurrent  = 0.d0
		DPBDO%SeeParticleCurrent = 0.d0
		DPBDO%ParticleFlux    = 0.d0
		DPBDO%SeeParticleFlux = 0.d0
		DPBDO%Energy    = 0.d0
		DPBDO%SEEEnergy = 0.d0
		DPBDO%AverageEnergy   = 0.d0
		DPBDO%SEEAverageEnergy= 0.d0
		DPBDO%PowerCurrent    = 0.d0
		DPBDO%SeePowerCurrent = 0.d0
		DPBDO%PowerFlux       = 0.d0
		DPBDO%SeePowerFlux    = 0.d0
		!Lower electrode
		do i=1,PBDO%CountMinOne
			DPBDO%Energy(1)= DPBDO%Energy(1)+PBDO%PBLower%PO(i)%Energy(PBDO%PBLower%Mass,PBDO%PBLower%VFactor)
		end do
		do i=1,PBDO%SEECountMinOne
			j=PBDO%CountMinOne+i
			DPBDO%SeeEnergy(1)= DPBDO%SeeEnergy(1)+PBDO%PBLower%PO(j)%Energy(PBDO%PBLower%Mass,PBDO%PBLower%VFactor)
		end do
		!Upper electrode
		do i=1,PBDO%CountMaxOne
			DPBDO%Energy(2) = DPBDO%Energy(2) + PBDO%PBUpper%PO(i)%Energy(PBDO%PBUpper%Mass,PBDO%PBUpper%VFactor)
		end do
		do i=1,PBDO%SEECountMaxOne
			j=PBDO%CountMaxOne+i
			DPBDO%SEEEnergy(2)= DPBDO%SEEEnergy(2)+PBDO%PBUpper%PO(j)%Energy(PBDO%PBUpper%Mass,PBDO%PBUpper%VFactor)
		end do
		DPBDO%ParticleNumber(1)	= DBLE(PBDO%CountMinOne) * PBDO%PBLower%Weight*CF%Dx
		DPBDO%ParticleNumber(2)	= DBLE(PBDO%CountMAXOne) * PBDO%PBUpper%Weight*CF%Dx
		DPBDO%ParticleCurrent	= DPBDO%ParticleNumber / CF%Dt 
		DPBDO%ParticleFlux	= DPBDO%ParticleCurrent * CF%ElectrodeArea
		DPBDO%Energy(1)	   = DPBDO%Energy(1) * PBDO%PBLower%Weight*CF%Dx
		DPBDO%Energy(2)	   = DPBDO%Energy(2) * PBDO%PBUpper%Weight*CF%Dx
		DPBDO%PowerCurrent   = DPBDO%Energy / CF%Dt 
		DPBDO%PowerFlux      = DPBDO%PowerCurrent * CF%ElectrodeArea
		DPBDO%AverageEnergy	= DPBDO%Energy/ (JtoeV * DPBDO%ParticleNumber)
		
		DPBDO%SEEParticleNumber(1) = DBLE(PBDO%SEECountMinOne) * PBDO%PBLower%Weight*CF%Dx
		DPBDO%SEEParticleNumber(2)	= DBLE(PBDO%SEECountMaxOne) * PBDO%PBUpper%Weight*CF%Dx
		DPBDO%SEEParticleCurrent = DPBDO%SEEParticleNumber / CF%Dt 
		DPBDO%SEEParticleFlux	 = DPBDO%SEEParticleCurrent * CF%ElectrodeArea
		DPBDO%SEEEnergy		   = DPBDO%SEEEnergy * PBDO%PBLower%Weight*CF%Dx
		DPBDO%SEEPowerCurrent   = DPBDO%SEEEnergy / CF%Dt 
		DPBDO%SEEPowerFlux      = DPBDO%SEEPowerCurrent * CF%ElectrodeArea
		DPBDO%SeeAverageEnergy  = DPBDO%seeEnergy/ (JtoeV*DPBDO%SEEParticleNumber)
	  return
	end subroutine DiagParticleBoundaryCaculate
	
end module DiagParticleBouncary