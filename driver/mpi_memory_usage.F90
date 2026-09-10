module mpi_memory_usage
   use mpi
   implicit none
   private
   public :: print_mpi_memory_usage

   interface
      subroutine get_maxrss(maxrss_kb) bind(C, name='get_maxrss')
         use, intrinsic :: iso_c_binding
         integer(c_long) :: maxrss_kb
      end subroutine get_maxrss
   end interface

contains

   subroutine print_mpi_memory_usage()
      use, intrinsic :: iso_c_binding
      implicit none
      integer :: rank, size, ierr, i, j
      integer(c_long) :: maxrss_kb
      character(len=256) :: hostname
      integer :: hostname_len

      ! Local storage
      character(len=256), allocatable :: all_hostnames(:)
      integer(c_long), allocatable :: all_maxrss(:)
      character(len=256), allocatable :: unique_nodes(:)
      integer, allocatable :: node_ranks(:,:)
      integer(c_long), allocatable :: node_totals(:)
      integer(c_long) :: program_total
      integer :: num_unique_nodes, node_idx, rank_count
      logical :: is_new_node
      integer :: un

      call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
      call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

      allocate(all_hostnames(size))
      allocate(all_maxrss(size))

      call get_hostname(hostname, hostname_len)
      call get_maxrss(maxrss_kb)

      ! Gather all hostnames and MaxRSS values to rank 0
      call MPI_Gather(hostname, 256, MPI_CHARACTER, &
                      all_hostnames, 256, MPI_CHARACTER, &
                      0, MPI_COMM_WORLD, ierr)

      call MPI_Gather(maxrss_kb, 1, MPI_LONG, &
                      all_maxrss, 1, MPI_LONG, &
                      0, MPI_COMM_WORLD, ierr)

      if (rank == 0) then
         open(newunit=un, file ='mpi_memory_usage_summary.txt', action='write')

         write(un,'(A)') '=========================================='
         write(un,'(A)') 'MPI Memory Usage Summary'
         write(un,'(A)') '=========================================='
         write(un,'(A)') ''

         ! Print per-rank information
         write(un,'(A)') 'Per-Rank MaxRSS:'
         write(un,'(A)') '  Rank    Hostname                          MaxRSS (KiB)'
         write(un,'(A)') '  ------  --------------------------------  ------------'
         do i = 1, size
            write(un,'(2X,I6,2X,A32,2X,I12)') i-1, trim(all_hostnames(i)), all_maxrss(i)
         end do
         write(un,'(A)') ''

         ! Aggregate by node
         allocate(unique_nodes(size))
         allocate(node_ranks(size, size))
         allocate(node_totals(size))

         num_unique_nodes = 0
         node_ranks = 0
         node_totals = 0
         program_total = 0

         do i = 1, size
            is_new_node = .true.
            do j = 1, num_unique_nodes
               if (trim(all_hostnames(i)) == trim(unique_nodes(j))) then
                  is_new_node = .false.
                  node_idx = j
                  exit
               end if
            end do

            if (is_new_node) then
               num_unique_nodes = num_unique_nodes + 1
               node_idx = num_unique_nodes
               unique_nodes(node_idx) = all_hostnames(i)
            end if

            ! Add this rank's memory to the node total
            node_totals(node_idx) = node_totals(node_idx) + all_maxrss(i)
            program_total = program_total + all_maxrss(i)

            ! Track which ranks run on this node
            rank_count = 0
            do j = 1, size
               if (node_ranks(node_idx, j) > 0) rank_count = rank_count + 1
            end do
            node_ranks(node_idx, rank_count + 1) = i
         end do

         ! Print per-node summary
         write(un,'(A)') 'Per-Node Memory Totals:'
         write(un,'(A)') '  Node Name                         Total MaxRSS (KiB)  Ranks'
         write(un,'(A)') '  --------------------------------  ------------------  -----'
         do i = 1, num_unique_nodes
            rank_count = 0
            do j = 1, size
               if (node_ranks(i, j) > 0) rank_count = rank_count + 1
            end do
            write(un,'(2X,A32,2X,I18,2X,I5)') trim(unique_nodes(i)), node_totals(i), rank_count
         end do
         write(un,'(A)') ''
         write(un,'(A,I12,A)') 'Per-Program Memory Total: ', program_total, ' (KiB)'
         write(un,'(A,I12,A)') '                          ', program_total/1024, ' (MiB)'
         if (program_total/1024/1024 > 0) then
         write(un,'(A,I12,A)') '                          ', program_total/1024/1024, ' (GiB)'
         end if
         if (program_total/1024/1024/1024 > 0) then
         write(un,'(A,I12,A)') '                          ', program_total/1024/1024/1024, ' (TiB)'
         end if
         write(un,'(A)') '============================================'

         deallocate(unique_nodes, node_ranks, node_totals)
         close(un)
      end if

      deallocate(all_hostnames, all_maxrss)

   end subroutine print_mpi_memory_usage

   subroutine get_hostname(hostname, hostname_len)
      implicit none
      character(len=*), intent(out) :: hostname
      integer, intent(out) :: hostname_len
      integer :: ierr

      call MPI_Get_processor_name(hostname, hostname_len, ierr)
   end subroutine get_hostname

end module mpi_memory_usage
