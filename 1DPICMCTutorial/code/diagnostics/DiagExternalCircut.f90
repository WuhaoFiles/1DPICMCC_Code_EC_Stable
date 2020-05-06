Module DiagExternalCircuit
	  use ModuleControlFlow
	  use ExternalCircuit
	  use ModuleGrid
	  implicit none 
	  Type DiagExternalCircuit1
		  Real(8) :: Uccp=0.d0,Uccpmax=0.d0,Pccp=0.d0
		  Real(8) :: Ucmax=0.d0,Uc=0.d0,Pc=0.d0,Qc=0.d0
		  Real(8) :: Usmax=0.d0,Us=0.d0,Ps=0.d0
		  Real(8) :: Urmax=0.d0,Ur=0.d0,Pr=0.d0
		  Real(8) :: ULmax=0.d0,UL=0.D0,PL=0.d0
		  Real(8) :: Current=0.d0
	  end type DiagExternalCircuit1
	contains
	subroutine DiagExternalCircuit1Caculate(GD,CF,FG,EC,mode)
	   implicit none
		Class(Grid0D(*)),intent(inout):: GD
		class(ControlFlow),intent(in) :: CF
		class(External_Circuit),intent(inout) :: EC
		integer(4),intent(in) :: mode
		Type(Field),intent(in) :: FG
		Type(DiagExternalCircuit1) :: TempDEC1
		integer(4) :: Shift
		shift=1
		select case(mode)
		case(0)
			call ExternalCircuitParameterCaculate(CF,EC,TempDEC1)
			Call GD%Update(TempDEC1%Current,Shift)
			Call GD%Update(TempDEC1%Ps,Shift)
			Call GD%Update(TempDEC1%Pccp,Shift)
			Call GD%Update(TempDEC1%Pc,Shift)
			GD%Timer=GD%Timer+1
		case(1)
			Call GD%Rescale
         Call GD%Dump(1)
			Call GD%Reset()
			!GD%Timer = 0
			!GD%Value = 0.d0
		case default 				
		End Select
		return
	End subroutine  DiagExternalCircuit1Caculate
	
	subroutine ExternalCircuitParameterCaculate(CF,EC,TempDEC1)
	    implicit none
		 class(ControlFlow),intent(in) :: CF
		 class(External_Circuit),intent(inout) :: EC
		 Type(DiagExternalCircuit1),intent(inOUT) :: TempDEC1
		 integer(4) :: m,n,i
		 !Real(8) :: TempCurrent=0.d0,TempPower=0.d0,TempSum=0.d0
		 	m= CF%Period
			n = mod(CF%Timer,m)
		   TempDEC1%Uccp = EC%dumpVariable(2,n+1)
			TempDEC1%Us   = EC%dumpVariable(3,n+1)
			TempDEC1%Uc   = EC%dumpVariable(4,n+1)  !Votage of Copatsiter
			TempDEC1%Qc   = EC%dumpVariable(5,n+1)
			TempDEC1%Current = 0.d0
			TempDEC1%Pccp = 0.d0
			TempDEC1%Ps = 0.d0
			TempDEC1%Pc = 0.d0
			if(n == (m-1)) THEN
				do	i=1,m
	    		  TempDEC1%Current = TempDEC1%Current + EC%dumpVariable(6,i)
				  TempDEC1%Pccp    = TempDEC1%Pccp + EC%dumpVariable(2,i)*EC%dumpVariable(6,i)
				  TempDEC1%Ps      = TempDEC1%Ps   + EC%dumpVariable(3,i)*EC%dumpVariable(6,i)
				  TempDEC1%Pc      = TempDEC1%Pc   + EC%dumpVariable(4,i)*EC%dumpVariable(6,i)
	      	end do	
			end if
		 return
	end subroutine ExternalCircuitParameterCaculate
	
	End module 