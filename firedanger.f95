!  Author: Siri Chandana Kathroju
!  Date: 2/8/2021
!  Program: Fortran subroutine, danger() calculates the National Fire Danger Rating Index
!           Converted from Fortran 77 to Fortran 95

program fire
   
   !  Data needed for the calculations are:
   !  1. dry,        dry bulb temperature
   !  2. wet,        wet bulb temperature
   !  3. isnow,      some positive non zero number if there is snow on the ground
   !  4. wind,       the current wind speed in miles per hour
   !  5. buo,        the last value of the build up index
   !  6. iherb,      the current herb state of the district 1=cured, 2=transition, 3=green

   !  Data returned from the subroutine danger() are:
   !  1. df,         drying factor
   !  2. ffm,        fine fuel moisture
   !  3. adfm,       adjusted (10 day lag) fuel moisture
   !  4. grass,      grass spread index will be returned
   !  5. timber,     timber spread index will be returned
   !  6. fload,      fire load rating (man-hour base)
   !  7. buo,        build up index will be returned

   !  Variable declarations
   real :: dry, wet, precip, wind, buo, df, ffm, adfm, grass, timber, fload
   integer :: isnow, iherb

   !  Prompt the user to enter values and store the values in respective variables
   write (*,*) ' Enter the dry bulb temperature in degrees Fahrenheit:  '
   read (*,*) dry
   write (*,*) ' Enter the wet bulb temperature in degrees Fahrenheit: '
   read (*,*) wet
   write (*,*) ' Is there snow on the ground? (yes=1/no=0) '
   read (*,*) isnow
   write (*,*) ' Enter the current wind speed in miles per hour: '
   read (*,*) wind
   write (*,*) ' Enter in the last value of the build up index: '
   read (*,*) buo
   write (*,*) ' Enter in the current herb state of the district: (Cured=1, Transition=2, Green=3) '
   read (*,*) iherb
   write (*,*) ' Enter in the preceding 24-hour precipitation: '
   read (*,*) precip

   call danger(dry,wet,isnow,precip,wind,buo,iherb,df,ffm,adfm,grass,timber,fload)

   !  Display the calculated information from danger()
   write (*,*) '__________________ OUTPUT ______________________'
   write (*,*) '     Fine Fuel Moisture     = ', ffm
   write (*,*) '     Adjusted Fuel Moisture = ', adfm
   write (*,*) '     Fine Fuel Spread       = ', grass
   write (*,*) '     Timber Spread Index    = ', timber
   write (*,*) '     Fire Load Index        = ', fload
   write (*,*) '     Build Up Index         = ', buo
   write (*,*) '_________________________________________________'

end program fire

!  Subroutine for computing national fire danger ratings and fire load index
subroutine danger(dry,wet,isnow,precip,wind,buo,iherb,df,ffm,adfm,grass,timber,fload)

   !  These are the table values stored in an array that are used in computing the danger ratings
   real, dimension(4) :: a = (/-0.185900, -0.85900, -0.059660, -0.077373/)
   real, dimension(4) :: b = (/30.0, 19.2, 13.8, 22.5/)
   real, dimension(3) :: c = (/4.5, 12.5, 27.5/)
   real, dimension(6) :: d = (/16.0, 10.0, 7.0, 5.0, 4.0, 3.0/)


   !  Check to see if there is snow on the ground
   if (isnow <= 0) then

      ! If there is no snow on the ground and we will compute the spread indexes
      ! and fire load
      dif=dry-wet
      
      do i = 1, 4
         if (i == 4) then 
            exit
         else if (dif <= c(i)) then
            exit
         end if
      end do

      ffm=b(i)*exp(a(i)*dif)

      !  This will help figure out the drying factor for the day
      do j = 1, 7
         if (j == 7) then 
            df = 7
            exit
         else if (ffm - d(j) <= 0.) then 
            continue
         else
            df = j - 1
            exit
         end if
      end do

      !  Test to see if the fine fuel moisture is one or less
      !  if fine fuel moisture is one or less we set it to one
      if (ffm - 1. >= 0) then

         !  Add 5 percent fine fuel moisture for each herb stage greater than one
         ffm = ffm + (iherb - 1) * 5.

         !  We adjust the bui for precipitation before adding the dying factor
         if ((precip - .1) <= 0) then

            !  After the correction for rain, if any, we add today's drying factor
            !  to obtain the current the build up index
            buo=buo+df

            !  The grass spread index for heavy fuel lags
            !  the result will be the timber spread index 
            !  the adjusted fuel moisture, adfm, adjusted for heavy fuels 
            adfm = .9*ffm +.5 +9.5*exp(-(buo)/50.)

            !  Test to see if the fuel moistures are greater than 30 percent
            if (adfm-30. >= 0) then

               if (ffm-30. >= 0) then
                  grass = 1.
                  timber = 1.
                  return
               else

                  timber = 1.

                  !  Wind is greater than 14 MPH
                  if ( wind-14. >= 0) then

                     grass  = .00918*(wind+14.) * (33.-ffm)**1.65 - 3.

                     if ( grass-99. <= 0) then

                        if (timber <= 0) then
                           return 
                        else

                           if (buo <= 0) then
                              return 
                           else

                              !  Both timber spread and build up are greater than zero
                              fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640

                              !  Ensure that fload is greater than zero, otherwise set it zero
                              if (fload <= 0) then
                                 fload = 0.
                                 return 
                              else
                                 fload = 10. ** fload 
                                 return
                              end if

                           end if

                        end if

                     else 
                        grass = 99.
                     end if

                  else 

                     !  If windspeed is less than 14 MPH
                     grass = .01312*(wind+6.) * (33.-ffm)**1.65 - 3.

                     if (timber-1. <= 0) then

                        timber = 1.
                        if (grass-1. >= 0) then

                           if (timber <= 0) then
                              return 
                           else 

                              if (buo <= 0) then
                                 return 
                              else

                                 !  Both timber spread and build up are greater than zero
                                 fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640

                                 !  Ensure that fload is greater than zero, otherwise set it zero
                                 if (fload <= 0) then
                                    fload = 0.
                                    return 
                                 else
                                    fload = 10. ** fload
                                 end if  

                              end if

                           end if 

                        else
                           grass = 1.
                        end if

                     else 

                        if (timber <= 0.) then
                           return 
                        else 

                           if (buo <= 0.) then
                              return 
                           else 

                              !  Both timber spread and build up are greater than zero
                              fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640

                              !  Ensure that fload is greater than zero, otherwise set it zero
                              if (fload <= 0) then
                                 fload = 0.
                                 return 
                              else
                                 fload = 10. ** fload
                              end if

                           end if

                        end if 

                     end if 

                  end if 

               end if

            else 

               !  If wind speed is greater than 14 MPH
               if ( wind-14. >= 0) then
                  timber = .00918*(wind+14.) * (33.-adfm)**1.65 - 3.
               else 
                  timber = .01312*(wind+6.) * (33.-adfm)**1.65 - 3.
               end if

            end if 

         else 

            !  Precipitation exceeded 0.10 inches we must reduce the 
            !  build up index (buo) by an amount equal to the rain fall
            buo=-50.*alog(1.-(1.-exp(-buo/50.))*exp(-1.175*(precip-.1)))

            if (buo >= 0.) then

               !  After correction for rain, if any, we are ready to add today
               !  drying factor to obtain the current build up index
               buo=buo+df

               !  Adjust the grass spred index for heavy fuel lags
               !  the result will be the timber spread index
               !  the adjusted fuel moisture, adfm, adjusted for heavy fuels
               adfm = .9*ffm +.5 +9.5*exp(-(buo)/50.)

               !  Check to see if the fuel moistures are greater than 30 percent
               if (adfm-30. >= 0) then

                  if (ffm-30. >= 0) then
                     grass = 1.
                     timber = 1.
                     return
                  else

                     timber = 1.

                     !  Test to see if the wind speed is greater than 14 mph
                     if ( wind-14. >= 0) then

                        grass  = .00918*(wind+14.) * (33.-ffm)**1.65 - 3.

                        if ( grass-99. <= 0) then

                           if (timber <= 0) then
                              return 
                           else 

                              if (buo <= 0) then
                                 return 
                              else

                                 !  Both timber spread and build up are greater than zero
                                 fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640

                                 !  Ensure that fload is greater than zero, otherwise set it to zero
                                 if (fload <= 0) then
                                    fload = 0.
                                    return 
                                 else
                                    fload = 10. ** fload 
                                    return
                                 end if

                              end if

                           end if 

                        else 

                           grass = 99.
                           if (timber-99. <= 0) then

                              if (timber <= 0) then
                                 return 
                              else 

                                 if (buo <= 0) then
                                    return 
                                 else 

                                    !  Both timber spread and build up are greater than zero
                                    fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640

                                    !  Ensure that fload is greater than zero, otherwise set it to zero
                                    if (fload <= 0) then
                                       fload = 0.
                                       return 
                                    else
                                       fload = 10. ** fload
                                    end if 

                                 end if

                              end if 

                           else
                              timber = 99.
                           end if 

                        end if 

                     else 

                        grass = .01312*(wind+6.) * (33.-ffm)**1.65 - 3.

                        if (timber-1. <= 0) then
                           timber = 1.
                           if (grass-1. >= 0) then

                              if (timber <= 0) then
                                 return 
                              else 

                                 if (buo <= 0) then
                                    return 
                                 else 

                                    !  Both timber spread and build up are greater than zero
                                    fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640

                                    !  Ensure that fload is greater than zero, otherwise set it to zero
                                    if (fload <= 0) then
                                       fload = 0.
                                       return 
                                    else
                                       fload = 10. ** fload
                                    end if  

                                 end if

                              end if 

                           else
                              grass = 1.
                           end if 

                        else 

                           if (timber <= 0.) then
                              return 
                           else 

                              if (buo <= 0.) then
                                 return 
                              else 

                                 !  Both timber spread and build up are greater than zero
                                 fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640

                                 !  Ensure that fload is greater than zero, otherwise set it to zero
                                 if (fload <= 0) then
                                    fload = 0.
                                    return 
                                 else
                                    fload = 10. ** fload
                                 end if 

                              end if

                           end if 

                        end if 

                     end if 

                  end if

               else

                  !  If wind speed is greater than 14 MPH
                  if ( wind-14. >= 0) then
                     timber = .00918*(wind+14.) * (33.-adfm)**1.65 - 3.
                  else 
                     timber = .01312*(wind+6.) * (33.-adfm)**1.65 - 3.
                  end if

               end if

            else
               buo = 0.0
            end if

         end if

      else
         ffm = 1
      end if

   else

      !  There is snow on the ground and the timber and grass spread indexes
      !  must be set to zero. with a zero timber spread the fire load is
      !  also zero. build up will be adjusted for precipitation. 
      grass = 0.
      timber = 0.

      if ((precip - .1) <= 0) then
         return
      else

         !  Precipitation exceeded .1 inches and we reduce the build up index
         buo=-50.*alog(1.-(1.-exp(-buo/50.))*exp( -1.175*(precip-.1)))

         if (buo >= 0) then
            return
         else 
            buo = 0.
         endif

      endif

   end if

end subroutine danger
