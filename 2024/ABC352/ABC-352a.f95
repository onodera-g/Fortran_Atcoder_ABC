program abc352a
    !
    implicit none
    integer N, X, Y, Z, i

    !入力
    read (*, *) N, X, Y, Z

    if (Y > X) then
        do i = X, Y
            if (i == Z) then
                write (*, '(a)') 'Yes'
                stop
            end if
        end do
    else
        do i = X, Y, -1
            if (i == Z) then
                write (*, '(a)') 'Yes'
                stop
            end if
        end do
    end if
    write (*, '(a)') 'No'

end program abc352a
