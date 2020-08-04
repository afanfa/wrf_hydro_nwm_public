module io_manager_base
  use netcdf_layer_base
  implicit none
  
  type :: IOManager_
     logical :: parallel = .false.
     type(NetCDF_serial_),allocatable :: netcdf_serial
     type(NetCDF_parallel_),allocatable :: netcdf_parallel
   contains
     procedure, pass(self) :: set_communicator
  end type IOManager_

  interface IOManager_
     module procedure IOManager_init
  end interface IOManager_
    
contains

  type(IOManager_) function IOManager_init()
    implicit none

    allocate(IOManager_init%netcdf_serial)
    allocate(IOManager_init%netcdf_parallel)
    
  end function IOManager_init

  subroutine set_communicator(self,comm)
    implicit none

    class(IOManager_) :: self
    integer :: comm

    call self%netcdf_parallel%set_comm(comm)

  end subroutine set_communicator
  
end module io_manager_base
