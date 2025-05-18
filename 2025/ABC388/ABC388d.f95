program abc388d
    ! N      : 宇宙人の人数 (1 ≤ N ≤ 5×10^5)
    ! A(i)   : i 人目の初期所持石数
    ! D(i)   : 差分配列。累積和が k_j（寄付可能な大人の数）を表す
    ! B(i)   : i 人目の N 年後の最終所持石数
    ! curr   : 現在の累積和 = k_j
    ! C_init : 成人直後の i 人目の石数 = A(i) + curr
    ! l, r   : 差分配列更新区間の左右端
    implicit none
    integer(kind=8) :: N
    integer(kind=8) :: i, curr, C_init, l, r
    integer(kind=8), allocatable :: A(:), D(:), B(:)

    !--- 入力読み込み ---
    read (*, *) N
    allocate (A(N), D(N + 2), B(N))
    read (*, *) (A(i), i=1, N)

    !--- 差分配列初期化 ---
    D = 0_8
    curr = 0_8

    !--- 各年 t = 1…N の成人イベント処理 ---
    do i = 1, N
        ! 差分配列の累積和を更新し、k_i を得る
        curr = curr + D(i)

        ! 成人直後の石数 = 初期 + 寄付できる大人の数
        C_init = A(i) + curr

        ! i+1 年目以降、C_init 年間だけ寄付可能 (差分配列で区間加算)
        l = i + 1
        r = i + C_init
        if (r > N) r = N
        if (l <= r) then
            D(l) = D(l) + 1_8
            D(r + 1) = D(r + 1) - 1_8
        end if

        ! 最終所持石数 = 成人直後の石数 − 実際に寄付した回数
        ! 餘裕がない場合は0
        if (C_init > (N - i)) then
            B(i) = C_init - (N - i)
        else
            B(i) = 0_8
        end if
    end do

    !--- 出力 ---
    do i = 1, N
        write (*, '(I0)', advance='no') B(i)
        if (i < N) then
            write (*, '(A)', advance='no') ' '
        end if
    end do
    write (*, *)

end program abc388d
