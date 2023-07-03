module DomainTypeTransfer

    use DomainType
    use DomainGridType

    implicit none
  
  contains
  
  subroutine DomainVarInTransfer(domain, domaingrid, ix, iy)

    implicit none

    type(domain_type),     intent(inout) :: domain
    type(domaingrid_type), intent(in)    :: domaingrid
    integer, intent(in)                  :: ix
    integer, intent(in)                  :: iy

    domain%DT = domaingrid%DT
    domain%startdate = domaingrid%startdate 
    domain%enddate = domaingrid%enddate    
    domain%nowdate = domaingrid%nowdate           
    domain%start_datetime = domaingrid%start_datetime  
    domain%end_datetime = domaingrid%end_datetime  
    domain%curr_datetime = domaingrid%sim_datetimes(domaingrid%itime)    
    domain%itime = domaingrid%itime          
    domain%ntime = domaingrid%ntime          
    domain%time_dbl = domaingrid%time_dbl
    domain%iloc = ix 
    domain%jloc = iy 
    domain%lat = domaingrid%lat(ix,iy)       
    domain%lon = domaingrid%lon(ix,iy)       
    domain%ZREF = domaingrid%ZREF(ix,iy)         
    domain%terrain_slope = domaingrid%terrain_slope(ix,iy) 
    domain%azimuth = domaingrid%azimuth(ix,iy)          
    domain%vegtyp = domaingrid%vegtyp(ix,iy)    
    domain%croptype = domaingrid%croptype(ix,iy)        
    domain%isltyp = domaingrid%isltyp(ix,iy)       
    domain%IST = domaingrid%IST(ix,iy)      
    domain%zsoil(:) = domaingrid%zsoil(ix,iy,:)    
    domain%dzsnso(:) = domaingrid%dzsnso(ix,iy,:)   
    domain%zsnso(:) = domaingrid%zsnso(ix,iy,:)
    domain%soilcolor = domaingrid%soilcolor(ix,iy)    

  end subroutine

  subroutine DomainVarOutTransfer(domain, domaingrid, ix, iy)

    implicit none

    type(domaingrid_type), intent(inout) :: domaingrid
    type(domain_type),        intent(in) :: domain
    integer, intent(in)                  :: ix
    integer, intent(in)                  :: iy

    domaingrid%zsoil(ix,iy,:) = domain%zsoil(:) 
    domaingrid%dzsnso(ix,iy,:) = domain%dzsnso(:)  
    domaingrid%zsnso(ix,iy,:) = domain%zsnso(:)   
    domaingrid%nowdate = domain%nowdate !this needs to be kept here until nowdate is updated in domaingriddedDriverModule

  end subroutine DomainVarOutTransfer

end module DomainTypeTransfer