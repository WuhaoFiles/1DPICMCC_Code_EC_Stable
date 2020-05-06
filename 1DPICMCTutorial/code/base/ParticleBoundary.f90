Module ModuleParticleBoundary
  Use Constants
  use Numrical
  Use ModuleParticleBundle
  Implicit none
      Type ParticleBoundaryOne
             !Integer(4) :: XStart=0,XEnd=1
             Integer(4) :: ParticleBoundaryModel=15
             Real(8) :: XMin=0.d0,XMax=dble(NxMax-1)!,XLength=99.d0 
             Integer :: CountMinOne=0,CountmaxOne=0
				 Integer :: SEECountMinOne=0,SEECountmaxOne=0
             Integer :: CountMin=0,Countmax=0
             Real(8) :: Gamma=0.01     !ion Secondary Electron emission rate
             Integer(4) :: SEENparMin,SEENparMax
             Type(ParticleBundle) :: PBLower,PBUpper
				 !Real(8),allocatable :: PBO_Saving(:,:)
             Contains
             Procedure :: AllInit=>InitializationParticleBoundaryOne
		End Type ParticleBoundaryOne
								 
						 
       
    contains
    subroutine InitializationParticleBoundaryOne(PBDO,PB,CF)
        implicit none
        Class(ParticleBoundaryOne),intent(inout) :: PBDO
        Type(ParticleBundle),intent(in) :: PB
        Class(ControlFlow), intent(in) :: CF
             PBDO%PBLower= PB
             PBDO%PBUpper= PB
             PBDO%PBLower%NPar=0
             PBDO%PBUpper%NPar=0
             PBDO%XMin=Dble(CF%NxL)
             PBDO%XMax=Dble(CF%NxU)

        return
  end subroutine InitializationParticleBoundaryOne
    
    
   subroutine ParticleAborption(PB,PBDO)
        implicit none
        Type(ParticleBundle),intent(inout) :: PB
        Type(ParticleBoundaryOne),intent(inout) :: PBDO
        Integer :: i
		  PBDO%PBLower%Weight = PB%Weight
		  PBDO%PBUpper%Weight = PB%Weight
        Select case (PBDO%ParticleBoundaryModel)
            case(10)
                do i=PB%NPar,1,-1
                     If (PB%PO(i)%X<=PBDO%XMin.or.PB%PO(i)%X>=PBDO%XMax) then
                        !Write(*,*) "Xmin"
                         Call PB%DelOne(i)  
                     end if
                end do
            case(11)
                PBDO%CountMinOne=0
                PBDO%CountMaxOne=0
                do i=PB%NPar,1,-1
                     If (PB%PO(i)%X<=PBDO%XMin) then
                         PBDO%CountMinOne=PBDO%CountMinOne+1
                         !Write(*,*) "Xmin"
                         Call PB%DelOne(i)  
                     else If(PB%PO(i)%X>=PBDO%XMax) then
                         PBDO%CountMaxOne=PBDO%CountMaxOne+1
                         !Write(*,*) "Xmax"
                        Call PB%DelOne(i)                       
                     end if
                end do
                PBDO%CountMin=PBDO%CountMin+PBDO%CountMinOne
                PBDO%CountMax=PBDO%CountMax+PBDO%CountMaxOne
            case(12)
                PBDO%CountMinOne=0
                PBDO%CountMaxOne=0
                PBDO%PBLower%NPar=0
                PBDO%PBUpper%NPar=0
                do i=PB%NPar,1,-1
                     If (PB%PO(i)%X<=PBDO%XMin) then
                         PBDO%CountMinOne=PBDO%CountMinOne+1
                         Call PBDO%PBLower%AddOne(PB%PO(i))
                         Call PB%DelOne(i)  
                     else If(PB%PO(i)%X>=PBDO%XMax) then
                         PBDO%CountMaxOne=PBDO%CountMaxOne+1
                         Call PBDO%PBUpper%AddOne(PB%PO(i))
                         Call PB%DelOne(i)                          
                     end if
					 end do
                PBDO%CountMin=PBDO%CountMin+PBDO%CountMinOne
                PBDO%CountMax=PBDO%CountMax+PBDO%CountMaxOne
				case(14)
                PBDO%CountMinOne=0
                PBDO%CountMaxOne=0
                PBDO%PBLower%NPar=0
                PBDO%PBUpper%NPar=0
					 PBDO%SEECountMaxOne = 0
					 PBDO%SEECountMinOne = 0
                do i=PB%NPar,1,-1
                     If (PB%PO(i)%X<=PBDO%XMin) then
                         PBDO%CountMinOne=PBDO%CountMinOne+1
                         Call PBDO%PBLower%AddOne(PB%PO(i))
                         Call PB%DelOne(i)  
                     else If(PB%PO(i)%X>=PBDO%XMax) then
                         PBDO%CountMaxOne=PBDO%CountMaxOne+1
                         Call PBDO%PBUpper%AddOne(PB%PO(i))
                         Call PB%DelOne(i)                          
                     end if
                end do
                PBDO%CountMin=PBDO%CountMin+PBDO%CountMinOne
                PBDO%CountMax=PBDO%CountMax+PBDO%CountMaxOne 
					 
					case(15)
                PBDO%CountMinOne=0
                PBDO%CountMaxOne=0
                PBDO%PBLower%NPar=0
                PBDO%PBUpper%NPar=0
					 PBDO%SEECountMaxOne = 0
					 PBDO%SEECountMinOne = 0
					 PB%BoundaryLossEnergy = 0.D0
                do i=PB%NPar,1,-1
                     If (PB%PO(i)%X<=PBDO%XMin) then
                         PBDO%CountMinOne=PBDO%CountMinOne+1
								 PB%BoundaryLossEnergy = PB%BoundaryLossEnergy+PB%PO(i)%Energy(PB%Mass,PB%VFactor)!!!!!!!
                         Call PBDO%PBLower%AddOne(PB%PO(i))
                         Call PB%DelOne(i)  
                     else If(PB%PO(i)%X>=PBDO%XMax) then
                         PBDO%CountMaxOne=PBDO%CountMaxOne+1
								 PB%BoundaryLossEnergy = PB%BoundaryLossEnergy+PB%PO(i)%Energy(PB%Mass,PB%VFactor)!!!!!!!
                         Call PBDO%PBUpper%AddOne(PB%PO(i))
                         Call PB%DelOne(i)                          
                     end if
					 end do
					 PB%BoundaryLossEnergy = PB%BoundaryLossEnergy*PB%weight*PB%Dx
                PBDO%CountMin=PBDO%CountMin+PBDO%CountMinOne
                PBDO%CountMax=PBDO%CountMax+PBDO%CountMaxOne 
        End Select 

        return
	end subroutine ParticleAborption
	
  end  Module ModuleParticleBoundary