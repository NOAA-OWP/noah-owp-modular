module DomainVarTransferModule

    use NoahowpmpIOType
    use NoahowpmpType

    implicit none
  
  contains
  
  subroutine DomainVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    noahowpmp%domain%DT = NoahowpmpIO%DT
    noahowpmp%domain%startdate = NoahowpmpIO%startdate 
    noahowpmp%domain%enddate = NoahowpmpIO%enddate    
    noahowpmp%domain%nowdate = NoahowpmpIO%nowdate           
    noahowpmp%domain%start_datetime = NoahowpmpIO%start_datetime  
    noahowpmp%domain%end_datetime = NoahowpmpIO%end_datetime  
    noahowpmp%domain%curr_datetime = NoahowpmpIO%sim_datetimes(NoahowpmpIO%itime)    
    noahowpmp%domain%sim_datetimes = NoahowpmpIO%sim_datetimes
    noahowpmp%domain%itime = NoahowpmpIO%itime          
    noahowpmp%domain%ntime = NoahowpmpIO%ntime          
    noahowpmp%domain%time_dbl = NoahowpmpIO%time_dbl
    noahowpmp%domain%iloc = ix 
    noahowpmp%domain%jloc = iy 
    noahowpmp%domain%lat = NoahowpmpIO%lat(ix,iy)       
    noahowpmp%domain%lon = NoahowpmpIO%lon(ix,iy)       
    noahowpmp%domain%ZREF = NoahowpmpIO%ZREF(ix,iy)         
    noahowpmp%domain%terrain_slope = NoahowpmpIO%terrain_slope(ix,iy) 
    noahowpmp%domain%azimuth = NoahowpmpIO%azimuth(ix,iy)          
    noahowpmp%domain%vegtyp = NoahowpmpIO%vegtyp(ix,iy)    
    noahowpmp%domain%croptype = NoahowpmpIO%croptype(ix,iy)        
    noahowpmp%domain%isltyp = NoahowpmpIO%isltyp(ix,iy)       
    noahowpmp%domain%IST = NoahowpmpIO%IST(ix,iy)      
    noahowpmp%domain%zsoil(:) = NoahowpmpIO%zsoil(ix,iy,:)    
    noahowpmp%domain%dzsnso(:) = NoahowpmpIO%dzsnso(ix,iy,:)   
    noahowpmp%domain%zsnso(:) = NoahowpmpIO%zsnso(ix,iy,:)    

    end associate

  end subroutine

  subroutine DomainVarOutTransfer(Noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: Noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    
    NoahowpmpIO%zsoil(ix,iy,:) = Noahowpmp%domain%zsoil(:) 
    NoahowpmpIO%dzsnso(ix,iy,:) = Noahowpmp%domain%dzsnso(:)  
    NoahowpmpIO%zsnso(ix,iy,:) = Noahowpmp%domain%zsnso(:)   
    NoahowpmpIO%nowdate = Noahowpmp%domain%nowdate 
  
    end associate

  end subroutine DomainVarOutTransfer

end module DomainVarTransferModule