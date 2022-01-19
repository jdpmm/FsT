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

subroutine init (filename, ks_var, ns_var)
    character (100), intent(in) :: filename
    integer,         intent(in) :: ks_var, ns_var
    integer                     :: loopvar1 = 1, auxnumber = 1, loopvar2 = 1
    integer, dimension(ns_var)  :: alldata
    integer, dimension(ks_var)  :: xi, fi, Fi_ac
    real,    dimension(ks_var)  :: hi

    open (unit = 1, file=filename)

    ! Saving all values into the file, alldata vector will save every number in the file
    ! xi vector will save all numbers but without repeating them
    do loopvar1 = 1, ns_var
        read(1, *) auxnumber
        if ( any(xi == auxnumber) .eqv. .false. ) then
            xi(loopvar2) = auxnumber
            loopvar2 = loopvar2 + 1
        end if
        alldata(loopvar1) = auxnumber
    end do

    ! Setting the values of fi, Fi, and hi vectores, all those are columns in a simple table of descripive statistics
    ! fi = how many times xi in alldata
    ! Fi = fi - f(i-1)
    ! hi = fi / n
    do loopvar1 = 1, ks_var
        call get_fi_per_every_xi (alldata, xi(loopvar1), ns_var, auxnumber);
        fi(loopvar1) = auxnumber
        if ( loopvar1 == 1 ) then
            Fi_ac(loopvar1) = auxnumber
        else
            Fi_ac(loopvar1) = auxnumber + Fi_ac(loopvar1 - 1)
        end if
        hi(loopvar1) = real(auxnumber) / real(ns_var)
    end do

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
