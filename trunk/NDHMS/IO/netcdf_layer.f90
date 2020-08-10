module netcdf_layer_base
  use netcdf
  implicit none
  include "mpif.h"

  type :: NetCDF_layer_
     procedure (nf90_def_dim), pointer, nopass :: def_dim => nf90_def_dim
     procedure (nf90_inq_varid), pointer, nopass :: inq_varid => nf90_inq_varid
     procedure (nf90_enddef), pointer, nopass :: enddef => nf90_enddef
     procedure (nf90_close), pointer, nopass :: close_file => nf90_close
   contains
     procedure, nopass :: def_var
     procedure, nopass :: put_att_scalar_int
     procedure, nopass :: put_att_scalar_real
     procedure, nopass :: put_att_char
     generic, public :: put_att => put_att_scalar_int, put_att_scalar_real, put_att_char
  end type NetCDF_layer_
  
  type, extends(NetCDF_layer_) :: NetCDF_serial_
   contains
     procedure, pass(self) :: create_file => create_file_serial
     procedure, pass(self) :: open_file => open_file_serial
  end type NetCDF_serial_

  type, extends(NetCDF_layer_) :: NetCDF_parallel_
     integer :: MPI_communicator
     integer :: default_info
   contains
     procedure, pass(self) :: create_file => create_file_parallel
     procedure, pass(self) :: open_file => open_file_parallel
     procedure, pass(self) :: var_par_access
     !procedure, pass(self) :: put_var_nb => put_var_nb_real
     ! procedure, pass(self) :: put_att_scalar_int
     ! procedure, pass(self) :: put_att_scalar_real
     ! procedure, pass(self) :: put_att_char
     ! procedure, pass(self) :: buffer_attach
     ! procedure, pass(self) :: buffer_detach
     ! procedure, pass(self) :: wait_all
     procedure, pass(self) :: set_comm
!     generic, public :: put_att => put_att_scalar_int, put_att_scalar_real, put_att_char
  end type NetCDF_parallel_

contains

  function create_file_serial (self, path, cmode, ncid) result(res)
    class(NetCDF_serial_),  intent(in) :: self
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
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
    integer                            :: old_mode
    
    res = nf90_create(path, cmode, ncid, comm = self%MPI_COMMUNICATOR, info = self%default_info)

    if(res /= 0) write(*,*) "Error during file creation ",res, nf90_strerror(res), path

    res = nf90_set_fill(ncid, NF90_NOFILL, old_mode)
    if(res /= 0) write(*,*) "Error during file nofill ",res, nf90_strerror(res)
 
  end function create_file_parallel

  function open_file_parallel (self, path, mode, ncid) result(res)
    implicit none
    class(NetCDF_parallel_),  intent(in) :: self
    character (len = *), intent(in   ) :: path
    integer, intent(in) :: mode
    integer, intent(out) :: ncid
    integer :: res

    res = nf90_open(path, mode, ncid, comm = self%MPI_COMMUNICATOR, info = self%default_info)

  end function open_file_parallel

  ! function def_dim(self,ncid,name,len,dimid) result(res)
  !   implicit none

  !   class(NetCDF_parallel_),       intent( in) :: self
  !   integer,                       intent( in) :: ncid
  !   character(len=*),              intent( in) :: name
  !   integer,                       intent( in) :: len
  !   integer,                       intent(out) :: dimid
  !   integer :: res
  !   integer(kind=MPI_OFFSET_KIND) :: new_len

  !   new_len = len

  !   res = nf90mpi_def_dim(ncid,name,new_len,dimid)

  ! end function def_dim

  function def_var(ncid,name,xtype,dimids,varid) result(res)
    implicit none
    !class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    character(len = *),    intent( in) :: name
    integer,               intent( in) :: xtype
    integer, dimension(:), intent( in) :: dimids ! omitted for scalar
    integer,               intent(out) :: varid
    integer :: res
    
    res = nf90_def_var(ncid, name, xtype, dimids, varid)

  end function def_var

  function put_att_scalar_int(ncid,varid,name,values) result(res)
    implicit none
!    class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    integer,               intent( in) :: varid
    character(len = *),    intent( in) :: name
    integer,               intent( in) :: values
    integer :: res
    
    res = nf90_put_att(ncid, varid, name, values)
   
  end function put_att_scalar_int

  function put_att_scalar_real(ncid,varid,name,values) result(res)
    implicit none
!    class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    integer,               intent( in) :: varid
    character(len = *),    intent( in) :: name
    real,               intent( in) :: values
    integer :: res
    
    res = nf90_put_att(ncid, varid, name, values)
   
  end function put_att_scalar_real
  
  function put_att_char(ncid,varid,name,values) result(res)
    implicit none
 !   class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    integer,               intent( in) :: varid
    character(len = *),    intent( in) :: name
    character(len = 19),   intent( in) :: values
    integer :: res
    
    res = nf90_put_att(ncid, varid, name, values)
   
  end function put_att_char

  function var_par_access(self,ncid,varid,access) result(res)
    implicit none
    class(NetCDF_parallel_),       intent( in) :: self
    integer,               intent( in) :: ncid
    integer,               intent( in) :: varid
    integer,               intent( in) :: access
    integer :: res
    
    res = nf90_var_par_access(ncid, varid, access)
   
  end function var_par_access

  ! function inq_varid(self, ncid, name, varid) result(res)
  !   implicit none
  !   class(NetCDF_parallel_),       intent( in) :: self
  !   character(len=*), intent( in) :: name
  !   integer, intent(in) :: ncid
  !   integer,          intent(out) :: varid
  !   integer :: res

  !   res = nf90mpi_inq_varid(ncid, name, varid)
    
  ! end function inq_varid

  ! function enddef(self,ncid) result(res)
  !   implicit none
  !   class(NetCDF_parallel_),       intent( in) :: self
  !   integer, intent(in) :: ncid
  !   integer :: res

  !   res = nf90mpi_enddef(ncid)

  ! end function enddef

  ! function buffer_attach(self,ncid, bufsize) result(res)
  !   implicit none
  !   class(NetCDF_parallel_), intent( in) :: self
  !   integer, intent(in) :: ncid
  !   integer, intent(in) :: bufsize
  !   integer :: res
  !   integer(kind=MPI_OFFSET_KIND) :: new_bufsize

  !   new_bufsize = bufsize

  !   res = nf90mpi_buffer_attach(ncid, new_bufsize)

  ! end function buffer_attach

  ! function buffer_detach(self,ncid) result(res)
  !   implicit none
  !   class(NetCDF_parallel_), intent( in) :: self
  !   integer, intent(in) :: ncid
  !   integer :: res

  !   res = nf90mpi_buffer_detach(ncid)

  ! end function buffer_detach

  ! function put_var_nb_real(self,ncid,varid,values,req,start,count) result(res)
  !   use pnetcdf
  !   implicit none
  !   class(NetCDF_parallel_), intent( in) :: self
  !   integer, intent(in) :: ncid
  !   integer, intent(in) :: varid
  !   real, intent(inout) :: values(:,:)
  !   integer, intent(out) :: req
  !   integer(kind=MPI_OFFSET_KIND), dimension(:), intent(in   ) :: start
  !   integer(kind=MPI_OFFSET_KIND), dimension(:), intent(in   ) :: count
  !   integer :: res

  !   res = nf90mpi_bput_var(ncid, varid, values, req, start, count)
   
  ! end function put_var_nb_real

  ! function wait_all(self,ncid, num_vars, req, st) result(res)
  !   class(NetCDF_parallel_), intent( in) :: self
  !   integer, intent(in) :: ncid
  !   integer,               intent(in)    :: num_vars
  !   integer, dimension(:), intent(inout) :: req
  !   integer, dimension(:), intent(out)   :: st
  !   integer :: res

  !   res = nf90mpi_wait_all(ncid, NUM_VARS, req, st)

  ! end function wait_all

  ! function close_file(ncid) result(res)
  !   implicit none
  !   class(NetCDF_parallel_),       intent( in) :: self
  !   integer, intent(in) :: ncid
  !   integer :: res

  !   res = nf90mpi_close(ncid)

  ! end function close_file_parallel

  subroutine set_comm(self,mpi_comm)
    class(NetCDF_parallel_), intent(inout) :: self
    integer, intent(in) :: mpi_comm
    integer :: info, ierr

    self%MPI_COMMUNICATOR = MPI_COMM_WORLD
    call MPI_Info_create(info,ierr)
    call MPI_Info_set(info, "striping_unit", "4194304", ierr) 
    self%default_info = info
  end subroutine set_comm

end module netcdf_layer_base
