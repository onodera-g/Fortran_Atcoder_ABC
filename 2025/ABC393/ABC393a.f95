program ABC393a
    ! S1, S2    : 高橋君・青木君の体調 ("sick" または "fine")
    ! bad       : お腹を壊す牡蠣の種類番号

    implicit none
    character(len=4) :: S1, S2
    integer :: bad

    ! 入力
    read (*, '(A4,1X,A4)') S1, S2
    S1 = adjustl(trim(S1))
    S2 = adjustl(trim(S2))

    ! 両方とも sick の場合 → 牡蠣1 が原因
    if (S1 == 'sick' .and. S2 == 'sick') then
        bad = 1
        ! S1 が sick, S2 が fine の場合 → 牡蠣2 が原因
    else if (S1 == 'sick' .and. S2 == 'fine') then
        bad = 2
        ! S1 が fine, S2 が sick の場合 → 牡蠣3 が原因
    else if (S1 == 'fine' .and. S2 == 'sick') then
        bad = 3
        ! 両方とも fine の場合 → 残った 牡蠣4 が原因
    else
        bad = 4
    end if

    ! 結果の出力
    print *, bad

end program ABC393a
