program ABC401a
    ! S : 判定対象の整数（100 ≤ S ≤ 999）

    implicit none
    integer :: S

    ! 入力
    read (*, *) S

    ! 判定と結果の出力
    if (S >= 200 .and. S <= 299) then
        write (*, '(A)') 'Success'
    else
        write (*, '(A)') 'Failure'
    end if

end program ABC401a
