program ABC398b
    ! A(i)      : 7枚のカードに書かれた数 (1 ≤ A(i) ≤ 13)
    ! mask      : 0..127 のビットマスク（どのカードを選ぶか）
    ! cntBits   : mask の立っているビット数（選択枚数）
    ! freq(v)   : 選択されたカードにおける値 v の出現回数
    ! cnt2      : freq(v)==2 の種類数
    ! cnt3      : freq(v)==3 の種類数
    ! possible  : フルハウス成立フラグ
    ! i, j, v   : ループ用カウンタ

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: A(7)
    integer(int32) :: mask, i, j, v
    integer(int32) :: cntBits, cnt2, cnt3
    integer(int32) :: freq(14)
    logical :: possible

    ! 入力
    read (*, *) (A(i), i=1, 7)

    possible = .false.

    ! 7枚から5枚を選ぶ全通りをビットマスクで探索
    do mask = 0, 2**7 - 1
        ! 選択枚数を数える
        cntBits = 0
        do j = 0, 6
            if (btest(mask, j)) cntBits = cntBits + 1
        end do
        if (cntBits /= 5) cycle

        ! 選択されたカードの頻度を初期化・集計
        freq = 0
        do i = 1, 7
            if (btest(mask, i - 1)) then
                freq(A(i)) = freq(A(i)) + 1
            end if
        end do

        ! 出現回数 2,3 の種類数をカウント
        cnt2 = 0; cnt3 = 0
        do v = 1, 13
            if (freq(v) == 2) cnt2 = cnt2 + 1
            if (freq(v) == 3) cnt3 = cnt3 + 1
        end do

        ! フルハウスは cnt2==1 且つ cnt3==1
        if (cnt2 == 1 .and. cnt3 == 1) then
            possible = .true.
            exit
        end if
    end do

    ! 結果の出力
    if (possible) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program ABC398b
