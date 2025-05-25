program ABC407a
    ! A       : 入力される正整数 A (1 ≤ A ≤ 407)
    ! B       : 入力される正の奇数 B (1 ≤ B ≤ 407)
    ! n       : 整数部 ⌊A/B⌋
    ! r       : 剰余 A − n*B
    ! result  : A/B に最も近い整数（出力値）
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: A, B, n, r, result

    ! 入力
    read (*, *) A, B

    ! 整数除算と剰余を利用して、誤差を比較
    n = A/B
    r = A - n*B
    ! B は奇数なので、2*r ≠ B は保証される
    if (2*r < B) then
        result = n
    else
        result = n + 1
    end if

    ! 出力
    print *, result

end program ABC407a
