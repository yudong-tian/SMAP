program reproj

! Read SMAP SPL1CTB.002 brightness temperature, for aft-looking for now

      use hdf5 

      implicit none

      ! for reprojection
      real, parameter :: lat0=-89.75, lon0=-179.75, res=0.5
      integer, parameter :: nc=720, nr=360  ! lat/lon grid
      integer :: ic, ir 
      real (kind=4), allocatable :: tbh(:), tbv(:), lon(:), lat(:) 
      real (kind=4) :: otbv(nc, nr), otbh(nc, nr) 

      ! declarations
      integer (kind=4) :: fid,swid,status,astat
      integer (hsize_t) :: rank,dims(1),maxdims(1), datatype,i,j, nx
      character (len=255) :: dimlist
      integer (kind=4), allocatable :: start(:),stride(:)

      !======= choose the file and field to read
      character (len=128) :: filename
      character*100,   parameter    :: group_name = "Global_Projection" 
      character*100,   parameter    :: lon_name = "cell_lon" 
      character*100,   parameter    :: lat_name = "cell_lat" 
      character*100,   parameter    :: tbh_name = "cell_tb_h_aft" 
      character*100,   parameter    :: tbv_name = "cell_tb_v_aft" 
      integer(hid_t)                :: file_id, group_id 
      integer(hid_t)                :: lon_id, lat_id, tbv_id, tbh_id 
      integer(hid_t)                :: dataspace

      filename = "2015.12.31/SMAP_L1C_TB_04877_A_20151231T001615_R12240_001.h5"
      !======= open the interface 
      call h5open_f(status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF interface" 
      
      call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF file" 
      
      call h5gopen_f(file_id, group_name, group_id, status)
      if (status .ne. 0) write(*, *) "Failed to get group: ", group_name 

      call h5dopen_f(group_id, lon_name, lon_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: ", lon_name 

      call h5dget_space_f(lon_id, dataspace, status)
      if (status .ne. 0) write(*, *) "Failed to get dataspace id" 

      CALL h5sget_simple_extent_dims_f(dataspace, dims, maxdims, status)
      if (status .lt. 0) write(*, *) "Failed to get dims, status=", status 

      otbv = -9999.0
      otbh = -9999.0

      nx = dims(1) 
      write(*, *)"nx = ", nx
      allocate(tbv(nx)) 
      allocate(tbh(nx)) 
      allocate(lat(nx)) 
      allocate(lon(nx)) 

      call h5dread_f(lon_id, H5T_NATIVE_REAL, lon, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read lon" 

      call h5dopen_f(group_id, lat_name, lat_id, status)
      if (status .ne. 0) write(*, *) "Failed to get lon_id" 

      call h5dread_f(lat_id, H5T_NATIVE_REAL, lat, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read lat" 

      call h5dopen_f(group_id, tbv_name, tbv_id, status)
      if (status .ne. 0) write(*, *) "Failed to get tbv_id" 

      call h5dread_f(tbv_id, H5T_NATIVE_REAL, tbv, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read tbv" 

      call h5dopen_f(group_id, tbh_name, tbh_id, status)
      if (status .ne. 0) write(*, *) "Failed to get tbh_id" 

      call h5dread_f(tbh_id, H5T_NATIVE_REAL, tbh, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read tbh" 

      call h5fclose_f(file_id, status)  
      call h5close_f(status) 

      ! reprojection
      do j=1, nx 
             ir = nint ( (lat(j) - lat0 )/res ) + 1
             ic = nint ( (lon(j) - lon0 )/res ) + 1
             otbv(ic, ir) = tbv(j) 
             otbh(ic, ir) = tbh(j) 
      end do 
     
      write(*, *) "Saving binary format ...", nc, nr
      open(22, file="tbs.2gd4r", form="unformatted", access="direct", recl=nc*nr*4) 
          write(22, rec=1) otbv
          write(22, rec=2) otbh
      close(22) 
       
end program reproj 
