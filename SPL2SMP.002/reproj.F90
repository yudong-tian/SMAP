program reproj

! Read SMAP SPL2SMP.002 soil moisture retrievals and reproject to 0.25-deg lat/lon grid 

      use hdf5 

      implicit none

      ! for reprojection
      real, parameter :: lat0=-89.75, lon0=-179.75, res=0.5
      integer, parameter :: nc=720, nr=360  ! lat/lon grid
      integer :: ic, ir, iargc
      real (kind=4), allocatable :: sm(:), lon(:), lat(:) 
      real (kind=4) :: osm(nc, nr) 

      ! declarations
      integer (kind=4) :: fid,swid,status,astat
      integer (hsize_t) :: rank,dims(1),maxdims(1), datatype,i,j, nx
      character (len=255) :: dimlist
      integer (kind=4), allocatable :: start(:),stride(:)

      !======= choose the file and field to read
      character (len=128) :: filename, ofile ! input and output file names 
      character*100,   parameter    :: sm_gr_name = "Soil_Moisture_Retrieval_Data"
      character*100,   parameter    :: sm_field_name = "soil_moisture"
      character*100,   parameter    :: lon_name = "longitude"
      character*100,   parameter    :: lat_name = "latitude"
      integer(hid_t)                :: file_id, sm_gr_id,sm_field_id
      integer(hid_t)                :: lon_id, lat_id 
      integer(hid_t)                :: dataspace

      i =  iargc()
      If (i.ne.2) Then   ! wrong cmd line args, print usage
         write(*, *)"Usage:"
         write(*, *)"reproj input_h5_file output_bin_file"
         stop
      End If

     call getarg(1, filename)
     call getarg(2, ofile)
      
      !======= open the interface 
      call h5open_f(status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF interface" 
      
      call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, status) 
      if (status .ne. 0) write(*, *) "Failed to open HDF file" 
      
      call h5gopen_f(file_id,sm_gr_name,sm_gr_id, status)
      if (status .ne. 0) write(*, *) "Failed to get group: ", sm_gr_name 

      call h5dopen_f(sm_gr_id,sm_field_name,sm_field_id, status)
      if (status .ne. 0) write(*, *) "Failed to get dataset: ", sm_field_name 

      call h5dget_space_f(sm_field_id, dataspace, status)
      if (status .ne. 0) write(*, *) "Failed to get dataspace id" 

      CALL h5sget_simple_extent_dims_f(dataspace, dims, maxdims, status)
      if (status .lt. 0) write(*, *) "Failed to get dims, status=", status 

      osm = -9999.0

      nx = dims(1) 
      write(*, *)"nx = ", nx
      allocate(sm(nx)) 
      allocate(lat(nx)) 
      allocate(lon(nx)) 

      call h5dread_f(sm_field_id, H5T_NATIVE_REAL, sm, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read sm" 

      call h5dopen_f(sm_gr_id, lon_name, lon_id, status)
      if (status .ne. 0) write(*, *) "Failed to get lon_id" 

      call h5dread_f(lon_id, H5T_NATIVE_REAL, lon, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read lon" 

      call h5dopen_f(sm_gr_id, lat_name, lat_id, status)
      if (status .ne. 0) write(*, *) "Failed to get lat_id" 

      call h5dread_f(lat_id, H5T_NATIVE_REAL, lat, dims, status)
      if (status .ne. 0) write(*, *) "Failed to read lat" 

      call h5fclose_f(file_id, status)  
      call h5close_f(status) 

      ! reprojection
      do j=1, nx 
             ir = nint ( (lat(j) - lat0 )/res ) + 1
             ic = nint ( (lon(j) - lon0 )/res ) + 1
             osm(ic, ir) = sm(j) 
      end do 
     
      write(*, *) "Saving binary format ...", nc, nr
      open(22, file=ofile, form="unformatted", access="direct", recl=nc*nr*4) 
          write(22, rec=1) osm 
      close(22) 
       
end program reproj 
