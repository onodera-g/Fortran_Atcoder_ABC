program abc353c
    !N：数列の長さ
    !M：10**8
    !A：数列
    !cnt：f(Ai,Aj)が10**8を超える回数
    !ans：ΣΣf(x,y)を10**8でわったあまり
    !【解説】
    !Σの中はAjと同じAiはN-1回登場するので、ΣΣを愚直に計算する必要はない
    !Aiは10**8以下になるので、10**8で割った商が1を超えることはない
    !x+yがMを超える場合のみ商１あまりxxになる。それ以外はx＋yが10**8を超えないので、あまり＝x+yになる。
    implicit none
    integer(16) N, M, cnt
    integer(16) i, ans, j
    integer(16), allocatable ::A(:)

    !入力
    M = 10**8
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)
    cnt = 0

    !ΣΣf(Ai,Aj)の計算
    ans = 0
    do i = 1, N
        ans = ans + a(i)*(N - 1)
    end do

    !ΣΣf(Ai,Aj)を10**8で割ったあまりを求める
    call margesort(A, N)
    j = n
    do i = 1, N
        j = max(j, i)
        do while (a(i) + a(j) >= M .and. j > i)
            j = j - 1
        end do
        cnt = cnt + (n - j)
    end do
    ans = ans - M*cnt

    !結果の出力
    write (*, '(i0)') ans
end program

!マージソート
subroutine margesort(x, n)
    integer(16) N
    integer(16) x(N), tmp(N)
    character(1) y(N), tmp2(N)
    integer(16) start, end
    start = 1; end = N
    call loop_margesort(x, tmp, N, start, end)
end subroutine
recursive subroutine loop_margesort(x, tmp, N, left, right)
    integer(16) left, right, mid
    integer(16) N
    integer(16) x(N), tmp(N)
    integer(16) i, j, k
    if (left >= right) return
    mid = (left + right)/2
    call loop_margesort(x, tmp, N, left, mid)
    call loop_margesort(x, tmp, N, mid + 1, right)
    j = 0
    tmp(left:mid) = x(left:mid)
    do i = mid + 1, right
        tmp(i) = x(right - j)
        j = j + 1
    end do
    i = left
    j = right
    do k = left, right
        if (tmp(i) < tmp(j)) then
            x(k) = tmp(i)
            i = i + 1
        else if (tmp(i) == tmp(j)) then
            x(k) = tmp(i)
            i = i + 1
        else
            x(k) = tmp(j)
            j = j - 1
        end if
    end do
end subroutine loop_margesort
