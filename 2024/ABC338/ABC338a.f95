program abc338a
    !S：文字列
    implicit none
    character(100) S
    integer i

    !入力
    read (*, *) S

    !１文字目が大文字かどうか
    if (ichar(S(1:1)) > 90) then
        write (*, '(a)') 'No'
        stop
    end if

    !2〜100文字目が小文字かどうか
    do i = 2, len_trim(S)
        if (ichar(S(i:i)) < 91) then
            write (*, '(a)') 'No'
            stop
        end if
    end do
    write (*, '(a)') 'Yes'
end program abc338a
