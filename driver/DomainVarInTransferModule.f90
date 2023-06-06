module DomainVarInTransferModule

    use NoahowpmpIOVarType
    use NoahowpType

    implicit none
  
  contains
  
  subroutine DomainVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahmp_type),   intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    noahowpmp%DT = NoahowpmpIO%DT
    noahowpmp%startdate = NoahowpmpIO%startdate 
    noahowpmp%enddate = NoahowpmpIO%enddate    
    noahowpmp%nowdate = NoahowpmpIO%nowdate           
    noahowpmp%start_datetime = NoahowpmpIO%start_datetime  
    noahowpmp%end_datetime = NoahowpmpIO%end_datetime  
    noahowpmp%curr_datetime = NoahowpmpIO%curr_datetime    
    noahowpmp%sim_datetimes = NoahowpmpIO%sim_datetimes
    noahowpmp%itime = NoahowpmpIO%itime          
    noahowpmp%ntime = NoahowpmpIO%ntime          
    noahowpmp%time_dbl = NoahowpmpIO%time_dbl
    noahowpmp%ix = ix 
    noahowpmp%iy = iy 
    noahowpmp%lat = NoahowpmpIO%lat(ix,iy)       
    noahowpmp%lon = NoahowpmpIO%lon(ix,iy)       
    noahowpmp%ZREF = NoahowpmpIO%ZREF(ix,iy)         
    noahowpmp%terrain_slope = NoahowpmpIO%terrain_slope(ix,iy) 
    noahowpmp%azimuth = NoahowpmpIO%azimuth(ix,iy)          
    noahowpmp%vegtyp = NoahowpmpIO%vegtyp(ix,iy)    
    noahowpmp%croptype = NoahowpmpIO%croptype(ix,iy)        
    noahowpmp%isltyp = NoahowpmpIO%isltyp(ix,iy)       
    noahowpmp%IST = NoahowpmpIO%IST(ix,iy)      
    noahowpmp%zsoil(:) = NoahowpmpIO%zsoil(ix,iy,:)    
    noahowpmp%dzsnso(:) = NoahowpmpIO%dzsnso(ix,iy,:)   
    noahowpmp%zsnso(:) = NoahowpmpIO%zsnso(ix,iy,:)    

    end associate

  end subroutine

end module DomainVarInTransferModule