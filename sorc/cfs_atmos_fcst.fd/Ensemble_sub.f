 SUBROUTINE GridCompCreate(vm, gcGFS, GridCompName, Cf,             &
                           Total_Member, pe_member, Member_Id,rc)

 USE ESMF_Mod

 IMPLICIT none

 INTEGER, intent(in) :: Total_Member, pe_member(Total_member)
!
 TYPE(ESMF_VM),                                   INTENT(inout) :: vm    ! the ESMF virtual machine.
 TYPE(ESMF_config),                               INTENT(inout) :: Cf    ! ESMF config
 TYPE(ESMF_GridComp),    DIMENSION(Total_member), INTENT(out)   :: gcGFS
 CHARACTER(ESMF_MAXSTR), DIMENSION(Total_member), INTENT(in)    :: GridCompName
 INTEGER,                                         INTENT(out)   :: rc, Member_Id

 INTEGER, POINTER :: petlist(:, :)
 INTEGER          :: npe, me, i, pe_max

 rc = ESMF_SUCCESS

!  Get the number of provided tasks.
!-----------------------------------
 CALL ESMF_VMGet(vm, petCount = npe, localPet = me, rc = rc)

!
 pe_max = 1
 do i=1,Total_member
  pe_max = max(pe_max,pe_member(i))
 enddo

!  Set up the Pet List.
!----------------------
ALLOCATE(petlist(pe_max, Total_member))

! Create the the pet list and the local communicator for each ensemble member.
!-----------------------------------------------------------------------------
CALL SetUp_Member_Communicator(petlist, Total_Member, pe_member,  &
			       pe_max, Member_Id, me)

DO i = 1, Total_member
!gcGFS(i) = ESMF_GridCompCreate (vm,                                      &
gcGFS(i) = ESMF_GridCompCreate (                                         &
			     name         = GridCompName(i),             &
			     gridcomptype = ESMF_ATM,                    &
			     petList      = petlist(1:pe_member(i), i),  &
                             config       = Cf,                          &
			     rc           = rc)
END DO

END SUBROUTINE GridCompCreate



SUBROUTINE GFS_SetServices(gcGFS, Total_Member, rc)

USE ESMF_Mod
USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices

IMPLICIT none

interface
!DEC$ ATTRIBUTES NO_ARG_CHECK :: GFS_StandAlone_SetServices
end interface

INTEGER, INTENT(in)                                         :: Total_member
TYPE(ESMF_GridComp), DIMENSION(Total_member), INTENT(inout) :: gcGFS
INTEGER,                                      INTENT(out)   :: rc
INTEGER :: i

rc   = ESMF_SUCCESS

DO i = 1, Total_member
 CALL ESMF_GridCompSetServices(gcGFS(i),GFS_StandAlone_SetServices, rc)
END DO

END SUBROUTINE GFS_SetServices





SUBROUTINE GFS_Initialize(gcGFS, impGFS, expGFS, clock,        &
                          Total_Member, Member_Id, rc)

USE ESMF_Mod
USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices

IMPLICIT none

INTEGER, intent(in) :: Total_Member, Member_Id
TYPE(ESMF_GridComp), DIMENSION(Total_member), INTENT(inout) :: gcGFS
TYPE(ESMF_State),                             INTENT(inout) :: impGFS   
TYPE(ESMF_State),                             INTENT(inout) :: expGFS  
TYPE(ESMF_Clock),                             INTENT(inout) :: clock  
INTEGER,                                      INTENT(out)   :: rc
INTEGER                                                     :: i

rc   = ESMF_SUCCESS
DO i = 1, Total_member
 IF(Member_Id == i) THEN
  CALL ESMF_GridCompInitialize (gcGFS(i),                       &
	 	  	        importstate = impGFS,           &
			        exportstate = expGFS,           &
			        clock       = clock,            &
			        phase       = ESMF_SINGLEPHASE, &
			        rc          = rc)
 END IF
END DO

END SUBROUTINE GFS_Initialize





SUBROUTINE GFS_Run(gcGFS, impGFS, expGFS, clock,         &
	    Total_Member, Member_Id, rc)

USE ESMF_Mod
USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices

IMPLICIT none

INTEGER, intent(in) :: Total_Member, Member_Id
TYPE(ESMF_GridComp), DIMENSION(Total_member), INTENT(inout) :: gcGFS
TYPE(ESMF_State),                             INTENT(inout) :: impGFS
TYPE(ESMF_State),                             INTENT(inout) :: expGFS
TYPE(ESMF_Clock),                             INTENT(inout) :: clock
INTEGER,                                      INTENT(out)   :: rc
INTEGER                                                     :: i

rc   = ESMF_SUCCESS
DO i = 1, Total_member
IF(Member_Id == i) THEN
 CALL ESMF_GridCompRun (gcGFS(i),                       &
			importstate = impGFS,           &
			exportstate = expGFS,           &
			clock       = clock,            &
			phase       = ESMF_SINGLEPHASE, &
			rc          = rc)
END IF
END DO

END SUBROUTINE GFS_Run





SUBROUTINE GFS_Finalize(gcGFS, impGFS, expGFS, clock,         &
		 Total_Member, Member_Id, rc)

USE ESMF_Mod
USE GFS_GridComp_ESMFMod, ONLY: GFS_StandAlone_SetServices => SetServices

IMPLICIT none

INTEGER, intent(in) :: Total_Member, Member_Id
TYPE(ESMF_GridComp), DIMENSION(Total_member), INTENT(inout) :: gcGFS
TYPE(ESMF_State),                             INTENT(inout) :: impGFS
TYPE(ESMF_State),                             INTENT(inout) :: expGFS
TYPE(ESMF_Clock),                             INTENT(inout) :: clock
INTEGER,                                      INTENT(out)   :: rc
INTEGER                                                     :: i

rc   = ESMF_SUCCESS
DO i = 1, Total_member
 IF(Member_Id == i) THEN
  CALL ESMF_GridCompFinalize (gcGFS(i),                       &
			      importstate = impGFS,           &
			      exportstate = expGFS,           &
			      clock       = clock,            &
			      phase       = ESMF_SINGLEPHASE, &
			      rc          = rc)
 END IF
END DO

END SUBROUTINE GFS_Finalize





SUBROUTINE SetUp_Member_Communicator(petlist, Total_Member, pe_member,  &
			             pe_max,  Member_Id, me)

!
IMPLICIT none

 INTEGER, intent(in) :: Total_Member, pe_max, pe_member(Total_Member)
 INTEGER, DIMENSION(pe_max, Total_member), intent(out) :: petlist 
 INTEGER, intent(out)                                  :: Member_Id

 INTEGER :: me
 INTEGER :: i, j, i1

 i1 = 0
 DO j = 1, Total_member
     DO i = 1, pe_member(j)
         petlist(i, j) = i1
         IF(me == i1) THEN
             Member_Id = j
         END IF
         i1 = i1+1
     END DO
 END DO

 END SUBROUTINE SetUp_Member_Communicator
