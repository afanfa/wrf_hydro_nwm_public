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
     integer :: default_info
   contains
     procedure, pass(self) :: create_file => create_file_parallel
     procedure, pass(self) :: open_file => open_file_parallel
     procedure, pass(self) :: close_file => close_file_parallel
     procedure, pass(self) :: put_var_nb => put_var_nb_real
     procedure, pass(self) :: def_dim
     procedure, pass(self) :: def_var
     procedure, pass(self) :: put_att_scalar_int
     procedure, pass(self) :: put_att_scalar_real
     procedure, pass(self) :: put_att_char
     procedure, pass(self) :: inq_varid
     procedure, pass(self) :: enddef 
     procedure, pass(self) :: buffer_attach
     procedure, pass(self) :: buffer_detach
     procedure, pass(self) :: wait_all
     procedure, pass(self) :: set_comm
     generic, public :: put_att => put_att_scalar_int, put_att_scalar_real, put_att_char
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
  
  function create_file_parallel(self, path, cmode, ncid) result(res)
    class(NetCDF_parallel_),intent(in) :: self
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
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

  function def_dim(self,ncid,name,len,dimid) result(res)
    implicit none

    class(NetCDF_parallel_),       intent( in) :: self
    integer,                       intent( in) :: ncid
    character(len=*),              intent( in) :: name
    integer,                       intent( in) :: len
    integer,                       intent(out) :: dimid
    integer :: res
    integer(kind=MPI_OFFSET_KIND) :: new_len

    new_len = len

    res = nf90mpi_def_dim(ncid,name,new_len,dimid)

  end function def_dim

  function def_var(self,ncid,name,xtype,dimids,varid) result(res)
    implicit none
    class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    character(len = *),    intent( in) :: name
    integer,               intent( in) :: xtype
    integer, dimension(:), intent( in) :: dimids ! omitted for scalar
    integer,               intent(out) :: varid
    integer :: res
    
    res = nf90mpi_def_var(ncid, name, xtype, dimids, varid)

  end function def_var

  function put_att_scalar_int(self,ncid,varid,name,values) result(res)
    implicit none
    class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    integer,               intent( in) :: varid
    character(len = *),    intent( in) :: name
    integer,               intent( in) :: values
    integer :: res
    
    res = nf90mpi_put_att(ncid, varid, name, values)
   
  end function put_att_scalar_int

  function put_att_scalar_real(self,ncid,varid,name,values) result(res)
    implicit none
    class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    integer,               intent( in) :: varid
    character(len = *),    intent( in) :: name
    real,               intent( in) :: values
    integer :: res
    
    res = nf90mpi_put_att(ncid, varid, name, values)
   
  end function put_att_scalar_real
  
  function put_att_char(self,ncid,varid,name,values) result(res)
    implicit none
    class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    integer,               intent( in) :: varid
    character(len = *),    intent( in) :: name
    character(len = 19),   intent( in) :: values
    integer :: res
    
    res = nf90mpi_put_att(ncid, varid, name, values)
   
  end function put_att_char

  function inq_varid(self, ncid, name, varid) result(res)
    implicit none
    class(NetCDF_parallel_),       intent( in) :: self
    character(len=*), intent( in) :: name
    integer, intent(in) :: ncid
    integer,          intent(out) :: varid
    integer :: res

    res = nf90mpi_inq_varid(ncid, name, varid)
    
  end function inq_varid

  function enddef(self,ncid) result(res)
    implicit none
    class(NetCDF_parallel_),       intent( in) :: self
    integer, intent(in) :: ncid
    integer :: res

    res = nf90mpi_enddef(ncid)

  end function enddef

  function buffer_attach(self,ncid, bufsize) result(res)
    implicit none
    class(NetCDF_parallel_), intent( in) :: self
    integer, intent(in) :: ncid
    integer, intent(in) :: bufsize
    integer :: res
    integer(kind=MPI_OFFSET_KIND) :: new_bufsize

    new_bufsize = bufsize

    res = nf90mpi_buffer_attach(ncid, new_bufsize)

  end function buffer_attach

  function buffer_detach(self,ncid) result(res)
    implicit none
    class(NetCDF_parallel_), intent( in) :: self
    integer, intent(in) :: ncid
    integer :: res

    res = nf90mpi_buffer_detach(ncid)

  end function buffer_detach

  function put_var_nb_real(self,ncid,varid,values,req,start,count) result(res)
    use pnetcdf
    implicit none
    class(NetCDF_parallel_), intent( in) :: self
    integer, intent(in) :: ncid
    integer, intent(in) :: varid
    real, intent(inout) :: values(:,:)
    integer, intent(out) :: req
    integer(kind=MPI_OFFSET_KIND), dimension(:), intent(in   ) :: start
    integer(kind=MPI_OFFSET_KIND), dimension(:), intent(in   ) :: count
    integer :: res

    res = nf90mpi_bput_var(ncid, varid, values, req, start, count)
   
  end function put_var_nb_real

  function wait_all(self,ncid, num_vars, req, st) result(res)
    class(NetCDF_parallel_), intent( in) :: self
    integer, intent(in) :: ncid
    integer,               intent(in)    :: num_vars
    integer, dimension(:), intent(inout) :: req
    integer, dimension(:), intent(out)   :: st
    integer :: res

    res = nf90mpi_wait_all(ncid, NUM_VARS, req, st)

  end function wait_all

  function close_file_parallel(self,ncid) result(res)
    implicit none
    class(NetCDF_parallel_),       intent( in) :: self
    integer, intent(in) :: ncid
    integer :: res

    res = nf90mpi_close(ncid)

  end function close_file_parallel

  subroutine set_comm(self,mpi_comm)
    class(NetCDF_parallel_), intent(inout) :: self
    integer, intent(in) :: mpi_comm

    self%MPI_COMMUNICATOR = MPI_COMM_WORLD
    self%default_info = MPI_INFO_NULL
  end subroutine set_comm

end module netcdf_layer_base
