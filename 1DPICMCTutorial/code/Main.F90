! This version is finished by 2016/12/09, the code can run well and it can be a produciable code, but still improvements are possible, please refer next version.
! Note Due to the crosssections are different from the ones used in our code, there are some differences in the results.    
    
Program PIC_MCC_for_CCP
   Use ModuleOneStep
   Use ModuleDiagOneStep

   Implicit none
   Integer(4) :: i,j,k,m
   real(8) Cpu1,Cpu2 
   
      Integer(4) :: NRun=2000,NDiagShort=10,NDiagLong=0,NDiagTestParticle=1
		!Integer(4) :: NRun=10,NDiagShort=0,NDiagLong=0
     !Integer(4) :: NRun=10000,NDiagShort=200,NDiagLong=200
		
		!NRun =  ControlFlowGlobal%NRun
		!NDiagShort = ControlFlowGlobal%NDiagShort
		!NDiagLong  = ControlFlowGlobal%NDiagLong
		

	
   Call AllInitilalization()
   DO j=1,NRun
     do i=1,ControlFlowGlobal%Period
        Call OneStep()
         If (ParticleGlobal(0)%Npar>ParticleGlobal(0)%NParNormal) then
             do k=0,1
					Write(*,*) ParticleGlobal(k)%Npar,k,"before"
					open (10,file='1 Weightingchange.dat',position='APPEND') 
					    Write(10,FMt="(*(es21.14,1x))") ControlFlowGlobal%Timer*ControlFlowGlobal%Dt,ParticleGlobal(0)%Weight*ControlFlowGlobal%Dx, ParticleGlobal(1)%Weight*ControlFlowGlobal%Dx
				   close(10)
					Call ParticleBundleNormalization(ParticleGlobal(k),ParticleGlobal(k)%Npar/2)
					open (10,file='1 Weightingchange.dat',position='APPEND') 
					    Write(10,FMt="(*(es21.14,1x))") ControlFlowGlobal%Timer*ControlFlowGlobal%Dt,ParticleGlobal(0)%Weight*ControlFlowGlobal%Dx, ParticleGlobal(1)%Weight*ControlFlowGlobal%Dx
				   close(10)
					Write(*,*) ParticleGlobal(k)%Npar,k,"after" 
             end do
			End If 
		ControlFlowGlobal%Timer =  ControlFlowGlobal%Timer + 1
	   ENd DO

      open (10,position='append',file='ParticleNumber.dat')
      write (10,*) ParticleGlobal%NPar,ParticleGlobal%weight
      close (10)
   
      Call OneStepRestart()
      Write(*,*) 'Period ',j,ParticleGlobal%NPar
	   write(*,*) "ControlFlowGlobal%Timer=",ControlFlowGlobal%Timer
	ENd Do
   !Call CPU_TIME(CPU2)
   !Write(*,*) 'Period ',CPU2-CPU1,ParticleGlobal%NPar
   !Write(*,*) 'Period ', j,' Complete!'  
	
   Call DiagInitilalization(ControlFlowGlobal)
     do j=1,NDiagShort
         do i=1,ControlFlowGlobal%Period
             Call OneStep()
             Call DiagOneStep()
				 ControlFlowGlobal%Timer =  ControlFlowGlobal%Timer + 1
			End do
			!Call OneStepRestart()
         Write(*,*) 'Diag1Period ',j,ParticleGlobal%NPar
	      write(*,*) "ControlFlowGlobal%Timer=",ControlFlowGlobal%Timer	
     ENd Do
     Call DiagOneStepFinal()
	  
	  Call DiagInitilalization2() !for Test Particle
     do j=1,NDiagTestParticle
         do i=1,ControlFlowGlobal%Period
             Call OneStep()
             Call DiagOneStep2(i,j)
				 ControlFlowGlobal%Timer = ControlFlowGlobal%Timer + 1
			End do
			!Call OneStepRestart()
         Write(*,*) 'Diag2Period ',j,ParticleGlobal%NPar
	      write(*,*) "ControlFlowGlobal%Timer=",ControlFlowGlobal%Timer
	  ENd Do
	  
stop
end  Program

