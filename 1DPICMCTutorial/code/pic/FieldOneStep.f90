Module ModuleOneStepField
  Use ModuleField
  use ExternalCircuit
  Use ModuleFieldBoundary
  Use Numrical
     
     Type(Field) :: FieldGlobal
     Type(FieldSolver) :: FieldSolverGlobal
     Type(FieldBoundary) :: FieldBoundaryGlobal
     Type(FieldOne),Allocatable :: FieldOneGlobal(:)
	  Type(External_Circuit) :: External_Circuit_Global
                  
    contains
         subroutine InitializationField(CF)
            Implicit none
            Type(ControlFlow),intent(inout) :: CF
            Logical ::  Status
				CALL FieldGlobal%Initial(CF)
            Call FieldBoundaryGlobal%Init(CF)
            Call FieldGlobal%Load(Status)
				call External_Circuit_Global%InitialEC(CF)
            Allocate(FieldOneGlobal(0:CF%Ns))
			End subroutine InitializationField
         
    
			
         subroutine FieldOneStep(CF,FO,FG,FB,FS,PB,PBDO,EC)
            Implicit none
				tYPE(ControlFlow),INTENT(in) :: CF
            Type(FieldOne),intent(in) :: FO(0:CF%Ns)
            Type(Field),intent(inout) :: FG
            Type(FieldSolver), intent(inout):: FS
            Type(FieldBoundary), intent(inout):: FB
		      Type(ParticleBundle),intent(in) :: PB(:)
		      type(ParticleBoundaryOne),intent(inout) :: PBDO(:)
				class(External_Circuit),intent(inout) :: EC
            Call AccumulationField(FG,CF%Ns,FO)
            Call FB%Updater
				call EC%Update(CF,FG,FB,PB,PBDO)
            Call UpdaterCoeFieldSolver(FS,FG,FB,EC)
            Call UpdaterFieldSolver(FS)
            Call UpdaterField(FG,FS,FB,EC)
				call EC%Dump_EC(CF,FG,FB,PBDO,PB)
            return
			End subroutine FieldOneStep
        

			
        subroutine AccumulationField(FG,Ns,FO)
            Implicit none
            Type(Field),intent(inout) :: FG
            Integer(4),intent(in) :: Ns
            Type(FieldOne),intent(in) :: FO(0:Ns)
            Integer(4) :: i!,Nx
            FG%Rho=0.d0
            FG%Chi=0.d0
            !Nx=FG%Nx
                    do i=0,Ns
                            FG%Rho=FG%Rho+FO(i)%RhoOne
                            FG%Chi=FG%Chi+FO(i)%ChiOne
                    End do
            return
		  End subroutine AccumulationField
    
		  subroutine UpdaterCoeFieldSolver(FS,FG,FB,EC)
            Implicit none
            Type(FieldSolver), intent(inout):: FS
				Type(External_Circuit),intent(in) :: EC
            Type(Field), intent(in) :: FG
            Type(FieldBoundary), intent(in) :: FB
            Integer(4) :: i
            Real(8) :: GeoFactor
             Real(8) :: ATemp, BTemp,CTemp
            Associate (Ns=>FS%Ns,Nx=>FG%Nx,dx=>FG%dx)
                    do i=1,FS%Ns
                              ATemp= 1.d0 + 0.5d0 * (FG%Chi(i)+FG%Chi(i+1))
                              !ATemp=1.d0+Max(Chi(i),Chi(i+1))
                              FS%CoeA(i) = ATemp
                              !CTemp=1.d0+Max(Chi(i),Chi(i+1))
                              CTemp = 1.d0 + 0.5d0 * (FG%Chi(i+1)+FG%Chi(i+2))
                              FS%CoeC(i) = CTemp
                              BTemp =  -(ATemp + CTemp)
                              FS%CoeB(i) = BTemp
                    End do
                    FS%CoeB(1) = FS%CoeB(1) - FS%CoeA(1)/EC%b0
                    GeoFactor  = -dx*dx / Epsilon
                    FS%Source(1:Nx-2) = FG%Rho(2:Nx-1) * GeoFactor
						  FS%Source(1)      = FS%Source(1)   - GeoFactor*FS%CoeA(1)*EC%d0/EC%b0 !Phi(1)  * (1 + 1/2*(FG%Chi(1)+FG%Chi(2))
                    !FS%Source(1)     = FS%Source(1)    - FB%V1*FS%CoeA(1) !Phi(1)  * (1 + 1/2*(FG%Chi(1)+FG%Chi(2))
                    FS%Source(Nx-2)   = FS%Source(Nx-2) - FB%V2*FS%CoeC(Nx-2) !Phi(Nx) * (1 + 1/2*(FG%Chi(Nx-1)+FG%Chi(Nx))
            End Associate
            return
        End subroutine UpdaterCoeFieldSolver
		  
          subroutine UpdaterFieldSolver(FS)
            Implicit none
           Class(FieldSolver), intent(inout):: FS
           Call Tridag(FS%CoeA,FS%CoeB,FS%CoeC,FS%Source,FS%Solve,Fs%Ns)
           return
        End subroutine UpdaterFieldSolver

        
        subroutine UpdaterField(FG,FS,FB,EC)
            Implicit none
            Type(Field),intent(inout) :: FG
            Type(FieldSolver), intent(in):: FS
            Type(FieldBoundary), intent(in):: FB
				Type(External_Circuit),intent(in) :: EC
            Integer(4) :: i
            Associate (Nx=>FG%Nx)
                FG%Phi(2:Nx-1)=FS%Solve(1:Nx-2)
					 FG%Phi(1) = ((-FG%dx*FG%dx/( Epsilon*(1+0.5*( FG%Chi(1)+FG%Chi(2) )) )) * EC%d0 - FG%Phi(2))/EC%b0
                !FG%Phi(1)  = FB%V1
                FG%Phi(FG%Nx) = FB%V2
                FG%Ex(1) = (3.d0*FG%Phi(1)-4.d0*FG%Phi(2)+FG%Phi(3))/(2.d0*FG%dx)
                do i=2,Nx-1
                    FG%Ex(i)=(FG%Phi(i-1)-FG%Phi(i+1))/(2.d0*FG%dx)
                end do
                FG%Ex(Nx)=-1.d0*(FG%Phi(Nx-2)-4.d0*FG%Phi(Nx-1)+3.d0*FG%Phi(Nx))/(2.d0*FG%dx)
            End Associate
            return
		  End subroutine UpdaterField
        
		  
		  

        
End Module ModuleOneStepField