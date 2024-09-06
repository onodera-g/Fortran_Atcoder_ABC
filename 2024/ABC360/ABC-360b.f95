program abc360b
    !S：文字列S
    !T：文字列T(文字列Sを元にこれを作りたい)
    !l：作成した部分文字列

    implicit none
    integer w, c, i
    character(100) S, T, l

    ! 入力
    read (*, *) S, T

    do w = 1, len_trim(S) - 1 !区切る文字列
        do c = 1, w !スタート位置
            l = ""
            do i = c, len_trim(S), w !連結した文字列の作成
                l = trim(l)//S(i:i)
            end do
            if (trim(l) == trim(T)) then !Tと一致するか
                print *, "Yes"
                stop
            end if
        end do
    end do
    print *, "No"

end program abc360b
