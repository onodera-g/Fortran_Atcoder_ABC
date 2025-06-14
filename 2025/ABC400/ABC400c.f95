program ABC400c
    ! N                  : 入力される上限（1 ≤ N ≤ 10^18）
    ! count_for_factor(f): 関数。f * b^2 ≤ N となる最大の非負整数 b を返す。
    ! b, c               : T, U のそれぞれの集合に対応する b, c の最大値
    ! answer             : 良い整数の総数（|T| + |U|）

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer(int64) :: N
    integer(int64) :: b, c, answer

    ! 入力
    read (*, *) N

    b = count_for_factor(2_int64, N) ! T: X = 2 * b^2 ≤ N を満たす b の最大値
    c = count_for_factor(4_int64, N) ! U: X = 4 * c^2 ≤ N を満たす c の最大値
    answer = b + c ! 良い整数の個数は T ∪ U の和

    ! 結果の出力
    write (*, '(I0)') answer

contains

    pure function count_for_factor(factor, N) result(res)
        ! factor * x^2 ≤ N となる最大の x (x ≥ 0) を求める
        integer(int64), intent(in) :: factor, N
        integer(int64) :: res
        integer(int64) :: low, high, mid

        ! 探索範囲を指数的に拡大
        low = 0_int64
        high = 1_int64
        do while (factor*high*high <= N)
            high = high*2_int64
        end do

        ! 二分探索で low < x ≤ high の範囲を絞る
        do while (high - low > 1_int64)
            mid = (low + high)/2_int64
            if (factor*mid*mid <= N) then
                low = mid
            else
                high = mid
            end if
        end do

        res = low
    end function count_for_factor

end program ABC400c
