module DomainVarTransferModule

    use NoahowpGridTypeModule
    use NoahowpType

    implicit none
  
  contains
  
  subroutine DomainVarInTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(in)    :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowp%domain%DT = noahowpgrid%DT
    noahowp%domain%startdate = noahowpgrid%startdate 
    noahowp%domain%enddate = noahowpgrid%enddate    
    noahowp%domain%nowdate = noahowpgrid%nowdate           
    noahowp%domain%start_datetime = noahowpgrid%start_datetime  
    noahowp%domain%end_datetime = noahowpgrid%end_datetime  
    noahowp%domain%curr_datetime = noahowpgrid%sim_datetimes(noahowpgrid%itime)    
    noahowp%domain%sim_datetimes = noahowpgrid%sim_datetimes
    noahowp%domain%itime = noahowpgrid%itime          
    noahowp%domain%ntime = noahowpgrid%ntime          
    noahowp%domain%time_dbl = noahowpgrid%time_dbl
    noahowp%domain%iloc = ix 
    noahowp%domain%jloc = iy 
    noahowp%domain%lat = noahowpgrid%lat(ix,iy)       
    noahowp%domain%lon = noahowpgrid%lon(ix,iy)       
    noahowp%domain%ZREF = noahowpgrid%ZREF(ix,iy)         
    noahowp%domain%terrain_slope = noahowpgrid%terrain_slope(ix,iy) 
    noahowp%domain%azimuth = noahowpgrid%azimuth(ix,iy)          
    noahowp%domain%vegtyp = noahowpgrid%vegtyp(ix,iy)    
    noahowp%domain%croptype = noahowpgrid%croptype(ix,iy)        
    noahowp%domain%isltyp = noahowpgrid%isltyp(ix,iy)       
    noahowp%domain%IST = noahowpgrid%IST(ix,iy)      
    noahowp%domain%zsoil(:) = noahowpgrid%zsoil(ix,iy,:)    
    noahowp%domain%dzsnso(:) = noahowpgrid%dzsnso(ix,iy,:)   
    noahowp%domain%zsnso(:) = noahowpgrid%zsnso(ix,iy,:)    

    end associate

  end subroutine

  subroutine DomainVarOutTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(in)    :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowpgrid%zsoil(ix,iy,:) = noahowp%domain%zsoil(:) 
    noahowpgrid%dzsnso(ix,iy,:) = noahowp%domain%dzsnso(:)  
    noahowpgrid%zsnso(ix,iy,:) = noahowp%domain%zsnso(:)   
    noahowpgrid%nowdate = noahowp%domain%nowdate !this needs to be kept here until nowdate is updated in NoahowpGriddedDriverModule

    end associate

  end subroutine DomainVarOutTransfer

end module DomainVarTransferModule