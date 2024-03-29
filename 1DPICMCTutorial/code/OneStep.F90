Module ModuleOneStep
      Use ModuleParticleBundle
		use SEE
      Use ModuleSpecyOne
      Use ModuleMCCPublic
      Use ModuleOneStepField
      Use ModuleMCCInitialization
      
      !Use MoveModule
!      Use Diagnostics
      Implicit none
      ! This section defines the particles.
                  Type(ControlFlow) :: ControlFlowGlobal
      
                  Type(ParticleBundle),save,Allocatable ::  ParticleGlobal(:)
                  Type(ParticleBoundaryOne),save,Allocatable  :: ParticleBDOneGlobal(:)
                  !Type(ParticleBoundary),save  :: ParticleBDGlobal
                
    contains
    Subroutine AllInitilalization()
             Implicit none
             Integer(4) :: i
             Call InitializationControlFlow(ControlFlowGlobal)
             Call GasInit(ControlFlowGlobal)
             Call InitializationField(ControlFlowGlobal)
             Allocate(ParticleGlobal(0:ControlFlowGlobal%Ns))
             Allocate(ParticleBDOneGlobal(0:ControlFlowGlobal%Ns))
             DO i=0,ControlFlowGlobal%Ns
                    Call ParticleGlobal(i)%AllInit(SpecyGlobal(i),ControlFlowGlobal)
                    Call ParticleBDOneGlobal(i)%AllInit(ParticleGlobal(i),ControlFlowGlobal)
             End do
             Call MCCBundleInit(ControlFlowGlobal,SpecyGlobal,GasGlobal)
				 
				
            Return  
    End Subroutine AllInitilalization

    Subroutine OneStep()
             !  Use ,only : R
               Implicit none
               !Integer(4) :: Ntemp                   !Ntemp=ParticleGlobal(0)%NPar
            !   Real(8)::GamaE=0.2d0,GamaI=0.2d0
                                    !Write(*,*) ParticleBDOneGlobal%CountMinOne,ParticleBDOneGlobal%CountMin,ParticleGlobal%NPar
               Integer(4) :: i,j
               do i=0,ControlFlowGlobal%Ns
                     Call ParticleGlobal(i)%MoveEM(FieldGlobal)
                     !Call ParticleGlobal(i)%MoveES(FieldGlobal)
                     !Call ParticleMove(ParticleGlobal(i),FieldGlobal)
                     Call ParticleAborption(ParticleGlobal(i),ParticleBDOneGlobal(i))
							if (i==0) then
								   Call Selectron(ParticleGlobal(0),ParticleBDOneGlobal(0))
								end if
                     !Call Selectron(ParticleGlobal(i),ParticleBDOneGlobal(i))
                     !Call WeightingOne(ParticleGlobal(i),FieldOneGlobal(i))
                     Call ParticleGlobal(i)%WeightP2C(FieldOneGlobal(i))
					end do

               !Call FieldOneStep(ControlFlowGlobal%Ns,FieldOneGlobal,FieldGlobal,FieldBoundaryGlobal,FieldSolverGlobal)
               Call FieldOneStep(ControlFlowGlobal,FieldOneGlobal,FieldGlobal,FieldBoundaryGlobal,FieldSolverGlobal,ParticleGlobal,ParticleBDOneGlobal,External_Circuit_Global)
               Call MCC(ControlFlowGlobal%Ns,ControlFlowGlobal%Ng,ParticleGlobal,SpecyGlobal,GasGlobal,MCCBundleGlobal) 
               
               !Write(*,*) "Period After",ParticleGlobal%NPar
					!ControlFlowGlobal%Timer =  ControlFlowGlobal%Timer + 1
              return
    End  subroutine OneStep
    
        Subroutine OneStepRestart()
               !Use FileIO 
               Implicit none
               Integer(4) :: i
               !Write(*,*) Period,ParticleGlobal%NPar,"Period"!,FieldBoundaryGlobal%Qmin,FieldBoundaryGlobal%Qmax
               Call DumpFieldSolver(FieldSolverGlobal,0)
               Call DumpField(FieldGlobal,0)
               Call DumpFieldOne(1,FieldOneGlobal,0)
               do i=0,ControlFlowGlobal%Ns
                     Call ParticleGlobal(i)%Dump(0)
               End do
               Call FieldBoundayFinalization(FieldBoundaryGlobal)
              return
        End  subroutine OneStepRestart
 
    End Module ModuleOneStep
    
    
       

    !

    !
    !

        
        
        
        

