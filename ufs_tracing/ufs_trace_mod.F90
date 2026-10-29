module ufs_trace_mod

  use mpi_f08

  implicit none

  private

  interface
      function get_time_ns() bind(c, name='get_time_ns')
          use iso_c_binding
          integer(kind=c_int64_t) :: get_time_ns
      end function get_time_ns
  end interface

  logical :: initialized = .false.
  integer :: unit = -1

  public ufs_trace_init
  public ufs_trace
  public ufs_trace_finalize
  public get_time_ns

contains

  subroutine ufs_trace_init()

    integer :: ierr
    integer :: rank
    character(len=32) :: fname

    if (.not.initialized) then
      call MPI_Comm_rank (MPI_COMM_WORLD, rank, ierr)
      write(fname,'("ufs_trace_",I8.8,".trace")') rank

      open(newunit=unit, file=trim(fname))
      initialized = .true.
    end if

  end subroutine ufs_trace_init


  subroutine ufs_trace(component, routine, ph, ts)
    character(len=*), intent(in) :: component, routine, ph
    integer(kind=8), optional, intent(in) :: ts

    character(len=*), parameter :: GFMT = '("{""name"":""",A ,""", &
                                                ""ph"":""",A ,""", &
                                                ""ts"":""",I0,""", &
                                               ""pid"":""",A ,""", &
                                               ""tid"":""",A ,"""},")'

    character(len=*), parameter :: XFMT = '("{""name"":""",A ,""", &
                                                ""ph"":""",A ,""", &
                                                ""ts"":""",I0,""", &
                                               ""dur"":""",I0,""", &
                                               ""pid"":""",A ,""", &
                                               ""tid"":""",A ,"""},")'

    integer(kind=8) :: cur_time


    if (.not.initialized) then
       return
    end if

    ! cur_time = MPI_Wtime() * 1000000
    cur_time = get_time_ns() / 1000 ! micro seconds

    if (present(ts)) then
       if (trim(ph) == "X") then
          write(unit, FMT=XFMT) routine, ph, ts, (cur_time - ts), "1", component
       else
          write(unit, FMT=GFMT) routine, ph, ts, "1", component
       end if
    else
       write(unit, FMT=GFMT) routine, ph, cur_time, "1", component
    end if

  end subroutine ufs_trace

  subroutine ufs_trace_finalize()

    if (.not.initialized) then
       return
    end if

    close(unit)

    initialized = .false.
    unit = -1

  end subroutine ufs_trace_finalize

end module ufs_trace_mod
