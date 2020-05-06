Module ExternalCircuit
  use	ModuleFieldBoundary
  use ModuleParticleBoundary
  use Constants
  use ModuleControlFlow
  use DiagnosticsMomentum    !Field power caculate
  implicit none 
	Type External_Circuit
		Real(8) :: Resist  = 0.d0     !OuMu
		Real(8) :: Capacit = 200.0d-12  !Faraday
		Real(8) :: CapacitCCP(2) = 0.d0  !Faraday
		Real(8) :: A0 = 1.0d0     !Area of electrode
		Real(8) :: Alpha0					!1/C
		Real(8) :: Sigma(3) = 0.d0	   !Sigma(1) means N Timer,Sigma(2) means N-Dt Timer,Sigma(3) means N-2Dt Timer
		Real(8) :: Q_capa(3) = 0.d0            ! Q_conduction
		Real(8) :: Q_accu(3) = 0.d0            ! Q_accumulation
		Real(8) :: V_t(2) = 0.d0
		Real(8) :: Phi_1(2) = 0.d0
		Real(8) :: Phi_N(2) = 0.d0
		REAL(8) :: b0 = 0.d0
		REAL(8) :: d0 = 0.d0
		Real(8),allocatable :: dumpVariable(:,:)
	contains
	  procedure :: InitialEC => Initialize_External_Circuit
	  procedure :: Update => Update_External_Circuit
	  procedure :: Dump_EC => Dump_External_Circuit
	end type External_Circuit
	
	contains
	subroutine Initialize_External_Circuit(EC,CF)
	   implicit none
		class(External_Circuit),intent(inout) :: EC
		tYPE(ControlFlow),INTENT(in) :: CF
		EC%Sigma(1)  = 0.d0
		EC%Q_accu(1) = 0.d0
		EC%Alpha0 = 1.0 / EC%Capacit
		EC%A0 = CF%ElectrodeArea
		EC%CapacitCCP(2) = Epsilon * EC%A0/ZLength   !Vacuum Capacit of CCP
	   EC%CapacitCCP(1) = EC%CapacitCCP(2)
		allocate(EC%dumpVariable(17,CF%Period))
	   return
	end subroutine Initialize_External_Circuit
	
	subroutine Update_External_Circuit(EC,CF,FG,FB,PB,PBDO)
	   implicit none
		class(External_Circuit),intent(inout) :: EC
		Type(FieldBoundary),intent(in) :: FB
		tYPE(ControlFlow),INTENT(in) :: CF
		Type(Field),intent(in) :: FG
		Type(ParticleBundle),intent(in) :: PB(:)
		type(ParticleBoundaryOne),intent(inout) :: PBDO(:)
		integer(4) :: i,n
		Real(8) :: U1
		EC%V_t(2) = EC%V_t(1)
		EC%V_t(1) = FB%V1 - FB%V2
		IF (CF%Timer == 0) then
			U1 = EC%V_t(1)/(1+EC%CapacitCCP(2)/EC%Capacit)
			!WRITE(*,*) "U1",U1
			EC%Phi_1(1) = FB%V2 + U1
		   EC%Phi_N(1) = FB%V2
				
			EC%Q_capa(1) = (EC%V_t(1) - (EC%Phi_1(1)  - EC%Phi_N(1))) / EC%Alpha0
			EC%Q_capa(2) = EC%Q_capa(1)
			EC%Q_accu(2) = EC%Q_accu(1)
			EC%Sigma(1)  = EC%Q_capa(1)/EC%A0
			EC%Sigma(2)  = EC%Q_capa(2)/EC%A0
			EC%b0 = -1 - CF%Dx/(EC%Alpha0	* Epsilon*(1+0.5*(FG%Chi(1)+FG%Chi(2)))*EC%A0)
		   EC%d0 = FG%Rho(1)/2 + EC%Sigma(2)/FG%Dx + (EC%Q_accu(1)-EC%Q_capa(2) + EC%V_t(1)/EC%Alpha0)/(EC%A0*FG%Dx)
		else
		   EC%Phi_1(2) = FG%Phi(1)
		   EC%Phi_N(2) = FG%Phi(NxMax)
		   EC%Q_accu(2) = EC%Q_accu(1)
		   EC%Sigma(3)  = EC%Sigma(2)
			!EC%Sigma(2) = EC%Sigma(1)
	   	EC%Q_capa(3) = EC%Q_capa(2)
			EC%Q_accu(1) = 0.d0
	   	DO i=1,CF%Ns+1
			   EC%Q_accu(1) = EC%Q_accu(1) + (PBDO(i)%CountMinOne-PBDO(i)%SEECountMinOne) * PB(i)%Weight * CF%Dx * PB(i)%Charge
			end do
			EC%Q_accu(1) = EC%A0 * EC%Q_accu(1)
	   	EC%Q_capa(2) = (EC%V_t(2) + (EC%Phi_N(2)  - EC%Phi_1(2))) / EC%Alpha0
		   EC%Sigma(2)  = EC%Sigma(3) + (EC%Q_accu(2) + EC%Q_capa(2)-EC%Q_capa(3))/EC%A0
		end if
			EC%b0 = -1 - CF%Dx/(EC%Alpha0	* Epsilon*(1+0.5*(FG%Chi(1)+FG%Chi(2)))*EC%A0)
		   EC%d0 = FG%Rho(1)/2  + EC%Sigma(2)/FG%Dx + (EC%Q_accu(1)-EC%Q_capa(2) + EC%V_t(1)/EC%Alpha0)/(EC%A0*CF%Dx)
			!write(*,*) "b0=",EC%b0,"d0=",EC%d0,"FG%Rho(1)",FG%Rho(1)
		return
	end subroutine Update_External_Circuit
	
	subroutine Dump_External_Circuit(EC,CF,FG,FB,PBDO,PB)
			implicit none
			tYPE(ControlFlow),INTENT(in) :: CF
			Type(Field),intent(inout) :: FG
			Type(FieldBoundary), intent(in) :: FB
			class(External_Circuit),intent(inOUT) :: EC
			type(ParticleBoundaryOne),intent(inout) :: PBDO(:)
			Type(ParticleBundle),intent(in) :: PB(:)
			real(8) :: rtime,RPeriodTime,FieldPower,TempConstant(2),ParticleAddPower,CollisionPowerLoss,BoundaryPowerloss
			real(8) :: TempPs=0.d0,TempPccp=0.d0,TempPc=0.d0,TempCurrent=0.d0
			Real(8),save :: FieldEnergy0 
			integer :: i,j,n,m
			character(len=50) :: filename1 = "1 EC update1.dat"
			character(len=50) :: filename2 = "1 EC update2.dat"

			!character(len=50) :: filename3 = "1000 Voltage Amplitude Output.dat"
			m= CF%Period
			n = mod(CF%Timer,m)
			EC%dumpVariable(:,n+1) = 0.d0
			EC%dumpVariable(1,n+1) = CF%Timer*CF%Dt
		   EC%dumpVariable(2,n+1) = FG%Phi(1)-FG%Phi(NxMax)
			EC%dumpVariable(3,n+1) = FB%V1-FB%V2
			EC%dumpVariable(4,n+1) = EC%dumpVariable(3,n+1)-EC%dumpVariable(2,n+1)  !Votage of Copatsiter
			EC%dumpVariable(5,n+1) = EC%dumpVariable(4,n+1)*EC%Capacit
			call FieldPowerCaculate(FG,CF)
			EC%dumpVariable(10,n+1) = FG%FieldPower
			EC%dumpVariable(11,n+1) = FG%FieldEnergy(1)
			if(n>1.AND.n<(m-1)) then   !!get the energy of electric field produced by external circuit voltage
				if(EC%dumpVariable(3,n+1)*EC%dumpVariable(3,n)<0.d0) then
					if(abs(EC%dumpVariable(3,n+1))>abs(EC%dumpVariable(3,n))) then
						FieldEnergy0 = EC%dumpVariable(11,n)
					else
						FieldEnergy0 = EC%dumpVariable(11,n+1)
					end if
				end if
			end if
			EC%dumpVariable(12,n+1) = FieldEnergy0
			EC%dumpVariable(13:14,n+1) = FG%subFieldEnergy
			EC%dumpVariable(15,n+1) = FG%Phi(int((NxMax-1)/2))

			ParticleAddPower   = sum((PB(:)%MccEnergy(1)-PB(:)%Energy(1))*CF%ElectrodeArea/CF%Dt)
			BoundaryPowerloss  = sum(PB(:)%BoundaryLossEnergy)*CF%ElectrodeArea/CF%Dt
			CollisionPowerLoss = sum((PB(:)%MccEnergy(2)-PB(:)%Energy(1))*CF%ElectrodeArea/CF%Dt)
			EC%dumpVariable(16,n+1) = ParticleAddPower
			EC%dumpVariable(17,n+1) = CollisionPowerLoss
			if(n>1.AND.n<(m-1)) then
				EC%dumpVariable(17,n) = EC%dumpVariable(17,n+1) 
				end if!!!!!!For the real time collisionpowerlosss count will late onestep
			if(n == (m-1)) THEN
				do i=2,m-1
			      EC%dumpVariable(6,i) = (EC%dumpVariable(5,i+1)-EC%dumpVariable(5,i-1))/(CF%Dt+CF%Dt)   !I current
				end do
				EC%dumpVariable(6,1) = EC%dumpVariable(6,2)+EC%dumpVariable(6,2)-EC%dumpVariable(6,3)
				EC%dumpVariable(6,m) = EC%dumpVariable(6,m-1)+EC%dumpVariable(6,m-1)-EC%dumpVariable(6,m-2)
				EC%dumpVariable(7,:) = EC%dumpVariable(6,:)*EC%dumpVariable(3,:)  !P source
				EC%dumpVariable(8,:) = EC%dumpVariable(6,:)*EC%dumpVariable(2,:)  !P ccp
				EC%dumpVariable(9,:) = EC%dumpVariable(6,:)*EC%dumpVariable(4,:)  !P ccp
				
			   open (10,file=filename1,position='APPEND')
				   do i= 1,CF%Period
				      Write(10,FMt="(*(es21.14,1x))")  (EC%dumpVariable(j,i),j=1,17)
				   end do
				close(10)
				
				TempPs=0.d0
				TempPccp=0.d0
				TempPc=0.d0
				TempCurrent=0.d0
				do i=1,m
					TempPs = TempPs + EC%dumpVariable(6,i)*EC%dumpVariable(3,i)
					TempPccp = TempPccp + EC%dumpVariable(6,i)*EC%dumpVariable(2,i)
					TempPc = TempPc + EC%dumpVariable(6,i)*EC%dumpVariable(4,i)
					TempCurrent = TempCurrent + EC%dumpVariable(6,i)
				end do
				open (100,file=filename2,position='APPEND')
				      Write(100,FMt="(*(es21.14,1x))") EC%dumpVariable(1,int(m/2)),TempCurrent/CF%Period,TempPs/CF%Period,TempPccp/CF%Period,TempPc/CF%Period
				close(100)
			end if
			return
	end subroutine Dump_External_Circuit

	
	end module ExternalCircuit