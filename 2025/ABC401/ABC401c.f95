program ABC401c
    ! n          : 問題で与えられる指数 N
    ! k          : 問題で与えられる範囲幅 K
    ! range_summ : スライディングウィンドウの現在合計 (A_{i−K}…A_{i−1} の和)
    ! arr        : A_0…A_n を格納する配列
    ! i          : ループ用インデックス

    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    integer(int64), parameter :: modu = 10_int64**9
    integer(int32) :: n, k
    integer(int64) :: range_summ
    integer(int64), allocatable :: arr(:)
    integer(int32) :: i

    !　入力
    read (*, *) n, k

    ! n < k のときは定義上 A_n = 1 なので直接出力して終了
    if (n < k) then
        write (*, '(I0)') 1
        stop
    end if

    ! i = k…n まで順に A_i を計算
    allocate (arr(0:n))
    arr(0:k - 1) = 1_int64
    range_summ = int(k, kind=int64)

    do i = k, n
        arr(i) = range_summ
        range_summ = range_summ - arr(i - k) + arr(i)
        range_summ = mod(range_summ, modu)
        if (range_summ < 0_int64) then
            range_summ = range_summ + modu
        end if
    end do

    ! 結果の出力
    write (*, '(I0)') arr(n)
end program ABC401c
