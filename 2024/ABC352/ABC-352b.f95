program abc352b
    !
    implicit none
    character(2*10**5) S, T
    integer i, j

    !入力
    read (*, *) S
    read (*, *) T

    !結果の出力
    j = 1
    do i = 1, len(T)
        if (S(i:i) == T(j:j)) then
            write (*, '(i0,1x)', advance='no') j
            j = j + 1
        else
            do
                if (S(i:i) /= T(j:j)) then
                    j = j + 1
                else
                    write (*, '(i0,1x)', advance='no') j
                    j = j + 1
                    exit
                end if
            end do
        end if
        if (S(i + 1:i + 1) == "") stop
    end do
end program abc352b
