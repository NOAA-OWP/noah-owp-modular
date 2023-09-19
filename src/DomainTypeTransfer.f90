module DomainTypeTransfer

    use DomainType
    use DomainGridType

    implicit none
  
  contains
  
  subroutine DomainVarInTransfer(domain, domaingrid, ix, iy)

    implicit none

    type(domain_type),     intent(inout) :: domain
    type(domaingrid_type), intent(in)    :: domaingrid
    integer,               intent(in)    :: ix
    integer,               intent(in)    :: iy

    domain%ix = ix 
    domain%iy = iy 
    domain%curr_datetime = domaingrid%sim_datetimes(domaingrid%itime)    
    domain%lat = domaingrid%lat(ix,iy)  
    domain%lon = domaingrid%lon(ix,iy)           
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
    domaingrid%nowdate = domain%nowdate   

  end subroutine DomainVarOutTransfer

end module DomainTypeTransfer