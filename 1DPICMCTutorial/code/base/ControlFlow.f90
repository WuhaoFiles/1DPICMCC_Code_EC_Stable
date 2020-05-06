Module ModuleControlFlow
     Use Constants
     Implicit none
              Integer(4),Parameter :: NxMax=257
              Real(8),parameter :: ZLength=0.080d0
              Real(8),parameter :: Inputdx=ZLength/dble(NxMax-1)
              Real(8),parameter :: Inputdt=5.0d-11  !4.d-12
              Real(8),Parameter :: ElectrodeRidus = 0.1 !m
              Integer(4),Parameter  ::  DefaultNameIndex=10000
              Integer(4),Parameter  ::  DefaultNameIndexInit=20000
              Integer(4),Parameter  ::  ModeMultiplier=1000
 
     !  Ns--NSpecy,Ng---NGas
     Type ControlFlow
           Real(8)  :: Dx=Inputdx,Dt=Inputdt
           Integer(4) :: ParticlePerGrid=400
           Real(8)  :: InitDensity=1.d16
           Real(8)  :: ElectrodeArea = 0.03144592653589
           Integer(4) :: Ns=0,Ng=0
           Integer(4) :: Nx=NxMax,NxL=0,NxU=NxMax-1
           Integer(4) :: Timer=0,Period=0
           Integer(4) :: NRun=2000,NDiagShort=100,NDiagLong=0
           Logical :: ReStartParticles=.False.
           !contains
              !procedure :: Init=>InitializationControlFlow
           End Type ControlFlow
       contains     
        Subroutine InitializationControlFlow(CF)
               Type(ControlFlow) :: CF
                Logical :: Alive
                Character(len=99) :: Filename
					 CF%ElectrodeArea = PI * ElectrodeRidus * ElectrodeRidus
                NAMELIST /ControlFlow/ CF
                Filename="./input/controlflow.txt"
                  Inquire(file=Filename,exist=alive)
                   If(alive) then
                       OPEN(20,FILE=Filename)
                       Read(20,NML=ControlFlow)
                       Close (20)
                   Else
                      Write(*,*)  "ControlFlow Load", Trim(Filename),"ERROR! The program will abort!"
                      !Stop
                   ENd If
        End subroutine InitializationControlFlow
End Module ModuleControlFlow