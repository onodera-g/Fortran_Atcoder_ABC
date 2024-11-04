program abc377a
    ! S     : 英大文字からなる文字列
    ! A,B,C : A,B,Cの出現回数
    character(3) S
    integer A, B, C

    ! 入力
    read (*, *) S

    ! ABC判定
    A = 0; B = 0; C = 0
    do i = 1, 3
        select case (S(i:i))
        case ('A')
            A = A + 1
        case ('B')
            B = B + 1
        case ('C')
            C = C + 1
        end select
    end do

    ! 結果の出力
    if (A == 1 .and. B == 1 .and. C == 1) then
        write (*, *) 'Yes'
    else
        write (*, *) 'No'
    end if
end program abc377a
