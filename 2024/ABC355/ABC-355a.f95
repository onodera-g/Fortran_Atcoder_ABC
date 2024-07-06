program abc355a
    implicit none
    integer A, B, C

    !入力
    read (*, *) A, B

    !人Cの番号
    if (A == 1 .and. B == 2) C = 3
    if (A == 1 .and. B == 3) C = 2
    if (A == 2 .and. B == 1) C = 3
    if (A == 2 .and. B == 3) C = 1
    if (A == 3 .and. B == 1) C = 2
    if (A == 3 .and. B == 2) C = 1

    !結果の出力
    if (A == B) then
        write (*, *) - 1
    else
        write (*, *) C
    end if
end program abc355a
