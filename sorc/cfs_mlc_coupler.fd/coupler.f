!=======================================================================
!
      program Coupler
!
      use coupler_module
!
      implicit none
     
      integer iret
! ------------------------------------------------------------------

!     print *,'C: Calling coupler_init'

      call coupler_init(iret)
      if (iret.ne.0 ) CALL GLOB_ABORT(iret,'error in coupler INIT ',iret)

!     print *,'C: After Calling coupler_init iret=',iret
!
!     print *,'C: Calling coupler_run'

      call coupler_run(iret)
      if (iret.ne.0 ) CALL GLOB_ABORT(iret,'error in coupler RUN ',iret)

!     print *,'C: After Calling coupler_run iret=',iret
!
      print *,'C: Calling coupler_finalize'
!
      call coupler_finalize(iret)
      if (iret.ne.0 ) CALL GLOB_ABORT(iret,'error in coupler finalize ',iret)

!     print *,'C: After Calling coupler_finalize iret=',iret
!
      stop
      end
!
!=======================================================================
