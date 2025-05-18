program ABC390a
    ! A(5)      : 入力される整数列 A₁～A₅ を格納する配列。1～5 の順列。
    ! B(5)      : A の要素を一度コピーして隣接要素を入れ替えるための配列。
    ! i         : 隣接要素を入れ替える位置を示すループカウンタ（1～4）。
    ! j         : ソート判定のためのループカウンタ（1～5）。
    ! temp      : 隣接要素を交換するための一時変数。
    ! sorted    : B が昇順 (1,2,3,4,5) になっているかを示すフラグ。
    ! ok        : どの隣接交換で昇順になったかを示すフラグ。初期値は .false.。
    implicit none
    integer :: A(5), B(5)
    integer :: i, j, temp
    logical :: sorted, ok

    ! 入力
    read (*, *) A

    ok = .false.

    ! 隣接するペアをすべて試してみる（4 通り）
    do i = 1, 4
        ! 元の配列をコピー
        B = A

        ! 隣接する i 番目と (i+1) 番目を交換
        temp = B(i)
        B(i) = B(i + 1)
        B(i + 1) = temp

        ! 昇順かどうか判定 (1,2,3,4,5 になっているか)
        sorted = .true.
        do j = 1, 5
            if (B(j) /= j) then
                sorted = .false.
                exit
            end if
        end do

        if (sorted) then
            ok = .true.
            exit
        end if
    end do

    ! 結果の出力
    if (ok) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program ABC390a
