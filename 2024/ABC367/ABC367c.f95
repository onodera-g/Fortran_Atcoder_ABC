program abc367c
    !N    ：長さ
    !K    ：条件２の倍数
    !R    ：i 番目の要素の最大数
    !R_idx：配列Rの格納場所を管理
    !R_sum：条件２の管理用
    !total：作成できる組み合わせの総数(条件２は無視)
    !ans  ：長さNの整数列(辞書順、条件１を満たすが、条件２は無視)
    implicit none
    integer i, j, N, K, total, R_idx, R_sum
    integer, allocatable :: R(:)
    integer, allocatable :: ans(:, :)

    !入力
    read (*, *) N, K
    allocate (R(N))
    read (*, *) R(:)
    total = 1
    do i = 1, N
        total = total*R(i)
    end do
    allocate (ans(total, N))

    !組み合わせの作成
    R_idx = 1
    call cal_sequences(N, R, ans, R_idx)

    !結果の出力
    do i = 1, total
        R_sum = 0
        !条件２の判定
        do j = 1, N
            R_sum = R_sum + ans(i, j)
        end do
        if (mod(R_sum, K) == 0) then !総和がKの倍数か
            do j = 1, N
                write (*, '(I0)', advance="no") ans(i, j)
                if (j < N) then
                    write (*, '(A)', advance="no") " "
                end if
            end do
            write (*, *)
        end if
    end do
contains
    subroutine cal_sequences(N, R, ans, R_idx)
        integer, intent(in) :: N, R(N)
        integer, intent(out) :: ans(:, :)
        integer, intent(inout) :: R_idx
        integer :: indices(N)
        integer :: i, pos

        indices = 1
        do
            do i = 1, N
                ans(R_idx, i) = indices(i)
            end do
            R_idx = R_idx + 1
            pos = N
            do while (pos >= 1)
                if (indices(pos) < R(pos)) then
                    indices(pos) = indices(pos) + 1
                    exit
                else
                    indices(pos) = 1
                    pos = pos - 1
                end if
            end do
            if (pos == 0) exit
        end do
    end subroutine cal_sequences
end program abc367c
