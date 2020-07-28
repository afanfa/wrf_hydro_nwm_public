module netcdf_layer_base
  use netcdf
  use pnetcdf
  implicit none
  include "mpif.h"
  
  type :: NetCDF_serial_
   contains
     procedure, pass(self) :: create_file => create_file_serial
     procedure, pass(self) :: open_file => open_file_serial
  end type NetCDF_serial_

  type :: NetCDF_parallel_
     integer :: MPI_communicator
     integer :: default_info = MPI_INFO_NULL
   contains
     procedure, pass(self) :: create_file => create_file_parallel
     procedure, pass(self) :: open_file => open_file_parallel
     procedure, pass(self) :: set_communicator
  end type NetCDF_parallel_

contains

  function create_file_serial (self, path, cmode, initialsize, chunksize, ncid) result(res)
    class(NetCDF_serial_),  intent(in) :: self
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
    integer, optional,   intent(in   ) :: initialsize
    integer, optional,   intent(inout) :: chunksize
    integer,             intent(  out) :: ncid
    integer                            :: res
    
    res = nf90_create(path = path, cmode = cmode, ncid = ncid)
    
  end function create_file_serial

  function open_file_serial (self, path, mode, ncid) result(res)
    implicit none
    class(NetCDF_serial_),  intent(in) :: self
    character (len = *), intent(in) :: path
    integer, intent(in) :: mode
    integer, intent(out) :: ncid
    integer :: res

    res = nf90_open(path, mode, ncid)

  end function open_file_serial
  
  function create_file_parallel(self, path, cmode, initialsize, chunksize, ncid) result(res)
    class(NetCDF_parallel_),intent(in) :: self
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
    integer, optional,   intent(in   ) :: initialsize
    integer, optional,   intent(inout) :: chunksize
    integer,             intent(  out) :: ncid
    integer                            :: res
    
    res = nf90mpi_create(self%MPI_COMMUNICATOR, path, cmode, self%default_info, ncid)
 
  end function create_file_parallel

  function open_file_parallel (self, path, mode, ncid) result(res)
    implicit none
    class(NetCDF_parallel_),  intent(in) :: self
    character (len = *), intent(in   ) :: path
    integer, intent(in) :: mode
    integer, intent(out) :: ncid
    integer :: res

    res = nf90mpi_open(self%MPI_COMMUNICATOR, path, mode, self%default_info, ncid)

  end function open_file_parallel

  subroutine set_communicator(self,mpi_comm)
    class(NetCDF_parallel_), intent(inout) :: self
    integer, intent(in) :: mpi_comm

    self%MPI_COMMUNICATOR = mpi_comm

  end subroutine set_communicator

end module netcdf_layer_base
