subroutine get_fi_per_every_xi (alldata, currnt_x, size_alldata, fi)
    integer, intent(in)                          :: currnt_x, size_alldata
    integer, dimension(size_alldata), intent(in) :: alldata
    integer, intent(out)                         :: fi
    fi = 0

    do idx = 1, size_alldata
        if ( alldata(idx) == currnt_x ) then
            fi = fi + 1
        end if
    end do
end subroutine

subroutine get_median_value (alldata, size_alldata, median)
    integer, intent(in)                          :: size_alldata
    integer, dimension(size_alldata), intent(in) :: alldata
    real, intent(out)                         :: median
    integer                                      :: idx1, idx2

    if ( mod(size_alldata, 2) /= 0 ) then
        idx1 = (size_alldata + 1) / 2
        median = alldata(idx1)
    else
        idx1 = size_alldata / 2
        idx2 = idx1 + 1
        median = (real(idx1) + real(idx2)) / 2
    end if
end subroutine

subroutine init (filename, ks_var, ns_var)
    character (100), intent(in) :: filename
    integer,         intent(in) :: ks_var, ns_var
    integer                     :: loopvar1 = 1, auxnumber = 1, loopvar2 = 1
    integer, dimension(ns_var)  :: alldata
    integer, dimension(ks_var)  :: xi, fi, Fi_ac, mean, var
    real,    dimension(ks_var)  :: hi, DM
    real                        :: meanvalue, medianvalue, dmvalue, varvalue, dtvalue
    integer                     :: modevalue
    real                        :: auxnumberR

    open (unit = 1, file=filename)

    ! -------------------------------------------------------------------------------------------------------------------------------
    ! Saving all values into the file, alldata vector will save every number in the file                                            !
    ! xi vector will save all numbers but without repeating them                                                                    -
    do loopvar1 = 1, ns_var                                                                                                         !
        read(1, *) auxnumber                                                                                                        !
        if ( any(xi == auxnumber) .eqv. .false. ) then                                                                              !
            xi(loopvar2) = auxnumber                                                                                                !
            loopvar2 = loopvar2 + 1                                                                                                 !
        end if                                                                                                                      !
        alldata(loopvar1) = auxnumber                                                                                               !
    end do                                                                                                                          !
    close(1)                                                                                                                        !
    ! -------------------------------------------------------------------------------------------------------------------------------

    ! -------------------------------------------------------------------------------------------------------------------------------
    ! Setting the values of fi, Fi, and hi vectores, all those are columns in a simple table of descripive statistics               !
    ! fi   = how many times xi in alldata                                                                                           -
    ! Fi   = fi - f(i-1)                                                                                                            !
    ! hi   = fi / n                                                                                                                 !
    ! moda = xi with the fi bigger                                                                                                  !
    do loopvar1 = 1, ks_var                                                                                                         !
        call get_fi_per_every_xi (alldata, xi(loopvar1), ns_var, auxnumber);                                                       !
        fi(loopvar1) = auxnumber                                                                                                  !
                                                                                                                                  !
        if ( loopvar1 == 1 ) then                                                                                                  !
            Fi_ac(loopvar1) = auxnumber                                                                                            !
        else                                                                                                                       !
            Fi_ac(loopvar1) = auxnumber + Fi_ac(loopvar1 - 1)                                                                       !
        end if                                                                                                                      !
                                                                                                                                    !
        if ( xi(loopvar1) /= 0 ) then                                                                                               !
            hi(loopvar1) = real(auxnumber) / real(ns_var)                                                                           !
        else                                                                                                                       !
            hi(loopvar1) = 0.0                                                                                                      !
        end if                                                                                                                     !
    end do                                                                                                                          !
    do loopvar1 = 1, ks_var                                                                                                         !
        if ( auxnumber <= fi(loopvar1) ) then                                                                                       !
            modevalue = xi(loopvar1)                                                                                                !
            auxnumber = fi(loopvar1)                                                                                               !
        end if                                                                                                                      !
    end do                                                                                                                          !
    ! -------------------------------------------------------------------------------------------------------------------------------

    ! -------------------------------------------------------------------------------------------------------------------------------
    ! Getting values of mean and median:                                                                                            !
    ! mean:                                                                                                                        !
    !   Σ _xi_*_fi_                                                                                                               !
    !         n                                                                                                                  !
    ! median:                                                                                                                   !
    !   if n is odd:                                                                                                             !
    !      x((n + 1) / 2)                                                                                                         !
    !   if n is even:                                                                                                              !
    !      _x(n_/_2)_+_x((n_/_2)_+_1)_                                                                                              !
    !                   2                                                                                                           !
    auxnumber = 0                                                                                                                   !
    do loopvar1 = 1, ks_var                                                                                                        !
        mean(loopvar1) = (xi(loopvar1) * fi(loopvar1))                                                                            !
        auxnumber = auxnumber + mean(loopvar1)                                                                                   !
    end do                                                                                                                        !
    meanvalue = real(auxnumber) / real(ns_var)                                                                                     !
    call get_median_value(alldata, ns_var, medianvalue)                                                                             !
    ! -------------------------------------------------------------------------------------------------------------------------------

    ! -------------------------------------------------------------------------------------------------------------------------------
    ! Setting values of varianza, desviacion media and desviacion tipica (Spanish names)                                            !
    ! varianza:                                                                                                                     !
    !      Σ _(xi²_*_fi)_  - x²                                                                                                     !
    !           n                                                                                                                    !
    ! desviacion media:                                                                                                             !
    !      Σ _|xi_-_x|_*_fi_                                                                                                        !
    !            n                                                                                                                   !
    ! desviacion tipica                                                                                                              !
    !      √ var                                                                                                                    !
    auxnumberR = 0                                                                                                                  !
    auxnumber = 0                                                                                                                    !
    do loopvar1 = 1, ks_var                                                                                                          !
        DM(loopvar1) = abs(real(xi(loopvar1)) - meanvalue) * fi(loopvar1)                                                           !
        var(loopvar1) = (xi(loopvar1) ** 2) * fi(loopvar1)                                                                          !
        auxnumberR = auxnumberR + DM(loopvar1)                                                                                     !
        auxnumber = auxnumber + var(loopvar1)                                                                                     !
    end do                                                                                                                       !
    dmvalue = auxnumberR / real(ns_var)                                                                                         !
    varvalue = (real(auxnumber) / real(ns_var)) - meanvalue**2                                                                 !
    dtvalue = sqrt(varvalue)                                                                                                  !
    ! ------------------------------------------------------------------------------------------------------------------------

    open(unit=1, file="solved", status="new")
    do loopvar1 = 1, ks_var
        write(1, *) xi(loopvar1), fi(loopvar1), Fi_ac(loopvar1), hi(loopvar1), mean(loopvar1), DM(loopvar1), var(loopvar1)
    end do
    write(1, *) "Another values:"
    write(1, *) meanvalue
    write(1, *) modevalue
    write(1, *) medianvalue
    write(1, *) dmvalue
    write(1, *) varvalue
    write(1, *) dtvalue

    close(1)
end subroutine

program main
    implicit none
    character (100) :: filename
    character (10)  :: k, n
    integer         :: ks_var, ns_var

    call get_command_argument(1, filename)
    call get_command_argument(2, n)
    call get_command_argument(3, k)

    read(k, '(i3)') ks_var
    read(n, '(i3)') ns_var
    call init(filename, ks_var, ns_var)
end program
