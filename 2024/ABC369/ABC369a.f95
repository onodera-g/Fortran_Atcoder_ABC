program abc369a
    !A  ：整数A(入力)
    !B  ：整数B(入力)
    !x  ：整数x
    !p  ：等差数列の対象となる整数p
    !q  ：等差数列の対象となる整数q
    !r  ：等差数列の対象となる整数r
    !cnt：等差数列が成立する個数
    implicit none
    integer i
    integer A, B, x
    integer p, q, r, cnt

    !入力
    read (*, *) A, B

    !等差数列の判定
    cnt = 0
    do i = -200, 200
        x = i
        !x,A,B
        p = x; q = A; r = B
        if (q - p == r - q) then
            cnt = cnt + 1
            cycle
        end if
        !B,x,A
        p = B; q = x; r = A
        if (q - p == r - q) then
            cnt = cnt + 1
            cycle
        end if
        !A,B,x
        p = A; q = B; r = x
        if (q - p == r - q) then
            cnt = cnt + 1
            cycle
        end if
    end do

    !出力
    print *, cnt
end program abc369a
