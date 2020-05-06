Module ModuleDiagOneStep
      Use ModuleControlFlow
      Use DiagnosticsCollisionRate
      Use DiagnosticsEEPF
      Use DiagnosticsMomentum
      Use ModuleOneStepField
      Use ModuleOneStep
      Use DiagnosticsTestParticle
		USE DiagParticleBouncary
		use DiagExternalCircuit
		use ModuleTrackingParticle
      
      !Use DiagnosticsTestParticle
      Implicit none
		Type(Grid0D(Ns=17)),save :: G0DDiagParticleField
      Type(Grid1D(Nx=NxMax,Ns=11)),save :: G1DDiagParticleField
      Type(Grid2D(Nx=NxMax,Ny=100,Ns=11)),save :: G2DDiagParticleField
      
		Type(Grid0D(Ns=15)),save :: G0DDiagParticleCR
      Type(Grid1D(Nx=NxMax,Ns=6)),save :: G1DDiagParticleCR
      Type(Grid2D(Nx=NxMax,Ny=100,Ns=6)),save :: G2DDiagParticleCR

      Type(Grid1D(Nx=500,Ns=2)),save :: G1DDiagParticleEDF
      Type(Grid2D(Nx=500,Ny=100,Ns=2)),save :: G2DDiagParticleEDF
		
		Type(Grid1D(Nx=500,Ns=2)),save :: G1DDiagBoundaryPEEDF
		
		Type(Grid0D(Ns=13)),save :: G0DDiagParticleBoundary
		Type(Grid0D(Ns=4)),save :: G0DDiagExternalCircuit
		
      !Tracing Particle Information in Diagnosis OneStep
      type(Tracking_Particle_Information) :: Tracking_Particle_Information1
      type(Tracking_Particle_Temp) :: TP_Temp_Global
		
      contains
      Subroutine DiagInitilalization(CF)
            Implicit none
            Class(ControlFlow), intent(in) :: CF
            !Integer(4),intent(in) ::  Ns
            !Type(ParticleBundle),intent(in) :: PB(0:Ns)
				     Call G0DDiagParticleField%Init(CF)
                 Call G1DDiagParticleField%Init(CF)
                 Call G2DDiagParticleField%Init(CF)
					  CALL G0DDiagParticleCR%Init(CF)
                 Call G1DDiagParticleCR%Init(CF)
                 Call G2DDiagParticleCR%Init(CF)
                 Call G1DDiagParticleEDF%Init(CF)
                 Call G2DDiagParticleEDF%Init(CF)
					  call G0DDiagParticleBoundary%Init(CF)
					  call G0DDiagExternalCircuit%Init(CF)
					  CALL G1DDiagBoundaryPEEDF%Init(CF)
          return  
		End Subroutine DiagInitilalization
      
     Subroutine DiagInitilalization2()
            Implicit none
            !Class(ControlFlow), intent(in) :: CF
            !Integer(4),intent(in) ::  Ns
            !Type(ParticleBundle),intent(in) :: PB(0:Ns)
				Call Diag_Tracking_Partcile_Initilalization(ControlFlowGlobal,ParticleGlobal,Tracking_Particle_Information1,TP_Temp_Global)
            !Call DiagParticleTestParticle(ParticleGlobal(0),ParticleBDOneGlobal(0),FieldGlobal,-1)	    
          return  
      End Subroutine DiagInitilalization2

		
      Subroutine  DiagOneStep()
          Implicit none   
             Call DiagParticleFieldPeriod(G1DDiagParticleField,G0DDiagParticleField,ControlFlowGlobal,ParticleGlobal,FieldGlobal,0)
				 !Call DiagParticleFieldPeriod(G2DDiagParticleField,G0DDiagParticleField,ControlFlowGlobal,ParticleGlobal,FieldGlobal,0)
				 
             Call DiagParticleEDFOne(G1DDiagParticleEDF,ParticleGlobal(0),0)
				 !Call DiagParticleEDFOne(G2DDiagParticleEDF,ParticleGlobal(0),0)
				 
             Call DiagParticleCollisionRateOne(G1DDiagParticleCR,G0DDiagParticleCR,ControlFlowGlobal,ParticleGlobal,MCCBundleGlobal,0)
				 !Call DiagParticleCollisionRateOne(G2DDiagParticleCR,G0DDiagParticleCR,ControlFlowGlobal,ParticleGlobal,MCCBundleGlobal,0)
				 
				 call DiagParticleBoundaryOne(G0DDiagParticleBoundary,ParticleBDOneGlobal,ControlFlowGlobal,0)
				 
				 call DiagExternalCircuit1Caculate(G0DDiagExternalCircuit,ControlFlowGlobal,FieldGlobal, External_Circuit_Global,0)

        return  
      End Subroutine DiagOneStep
      
      Subroutine  DiagOneStep2(i,j)
          Implicit none   
           integer(4) :: i,j
			   call Tracking_Particle(ParticleGlobal,Tracking_Particle_Information1,ControlFlowGlobal,i,j,TP_Temp_Global)
           return  
      End Subroutine DiagOneStep2
       !
       Subroutine DiagOneStepFinal()
       Implicit none  
             Call DiagParticleFieldPeriod(G1DDiagParticleField,G0DDiagParticleField,ControlFlowGlobal,ParticleGlobal,FieldGlobal,1)
				 !Call DiagParticleFieldPeriod(G2DDiagParticleField,G0DDiagParticleField,ControlFlowGlobal,ParticleGlobal,FieldGlobal,1)
				 
             Call DiagParticleEDFOne(G1DDiagParticleEDF,ParticleGlobal(0),1)
				 !Call DiagParticleEDFOne(G2DDiagParticleEDF,ParticleGlobal(0),1)
				 
             Call DiagParticleCollisionRateOne(G1DDiagParticleCR,G0DDiagParticleCR,ControlFlowGlobal,ParticleGlobal,MCCBundleGlobal,1)
				 !Call DiagParticleCollisionRateOne(G2DDiagParticleCR,G0DDiagParticleCR,ControlFlowGlobal,ParticleGlobal,MCCBundleGlobal,1)
				 
				 call DiagParticleBoundaryOne(G0DDiagParticleBoundary,ParticleBDOneGlobal,ControlFlowGlobal,1) 
				 call DiagExternalCircuit1Caculate(G0DDiagExternalCircuit,ControlFlowGlobal,FieldGlobal, External_Circuit_Global,1)
          return  
       End Subroutine DiagOneStepFinal
       
      Subroutine DiagOneStepFinal2()
       Implicit none  
          Call DiagParticleTestParticle(ParticleGlobal(0),ParticleBDOneGlobal(0),FieldGlobal,1)	 
          return  
		End Subroutine DiagOneStepFinal2

		subroutine DiagOneStep_Evolution(i,j)
		   implicit none 
			integer(4) :: i,j
             Call DiagParticleFieldPeriod(G1DDiagParticleField,G0DDiagParticleField,ControlFlowGlobal,ParticleGlobal,FieldGlobal,0)
             Call DiagParticleEDFOne(G1DDiagParticleEDF,ParticleGlobal(0),0)
             Call DiagParticleCollisionRateOne(G1DDiagParticleCR,G0DDiagParticleCR,ControlFlowGlobal,ParticleGlobal,MCCBundleGlobal,0)	
				 call DiagParticleBoundaryOne(G0DDiagParticleBoundary,ParticleBDOneGlobal,ControlFlowGlobal,0)
				 call DiagExternalCircuit1Caculate(G0DDiagExternalCircuit,ControlFlowGlobal,FieldGlobal, External_Circuit_Global,0)
				  !call Tracking_Particle(ParticleGlobal,Tracking_Particle_Information1,ControlFlowGlobal,i,j,TP_Temp_Global)
		   return
		end subroutine DiagOneStep_Evolution
		
		subroutine DiagOneStep_Evolution_Final()
		   implicit none 
			    Call DiagParticleFieldPeriod(G1DDiagParticleField,G0DDiagParticleField,ControlFlowGlobal,ParticleGlobal,FieldGlobal,1)
             Call DiagParticleEDFOne(G1DDiagParticleEDF,ParticleGlobal(0),1)
             Call DiagParticleCollisionRateOne(G1DDiagParticleCR,G0DDiagParticleCR,ControlFlowGlobal,ParticleGlobal,MCCBundleGlobal,1)
				 call DiagParticleBoundaryOne(G0DDiagParticleBoundary,ParticleBDOneGlobal,ControlFlowGlobal,1)
				 call DiagExternalCircuit1Caculate(G0DDiagExternalCircuit,ControlFlowGlobal,FieldGlobal, External_Circuit_Global,1)
		   return
		end subroutine DiagOneStep_Evolution_Final
		
  End Module ModuleDiagOneStep