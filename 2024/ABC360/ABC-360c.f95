program abc360c
    !N    ：箱の数
    !A    ：荷物A
    !W    ：各荷物の重さ
    !max_W：箱に入っている荷物の最重量
    !ans  ：コストの総和の最小値
    implicit none
    integer i
    integer N, ans
    integer, allocatable::A(:), W(:), max_W(:)

    !入力
    read (*, *) N
    allocate (A(N), W(N), max_W(N))
    read (*, *) A
    read (*, *) W

    !コストの計算
    max_W = 0
    do i = 1, n
        max_W(A(i)) = max(W(i), max_W(a(i)))
    end do
    ans = sum(W) - sum(max_W)

    !結果の出力
    write (*, '(i0)') ans
end program abc360c
