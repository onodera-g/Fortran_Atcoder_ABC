program abc342b
    !N：並んでいる人数
    !P：前からi 番目に並んでいる人数Pi
    !Q：クエリの個数
    !A：クエリの内容
    !B：クエリの内容
    implicit none
    integer i
    integer N, Q
    integer, allocatable::P(:, :), A(:), B(:)

    !入力
    read (*, *) N
    allocate (P(N, 2))
    read (*, *) P(:, 1)
    do i = 1, N
        P(i, 2) = i
    end do
    read (*, *) Q
    allocate (A(Q), B(Q))
    do i = 1, Q
        read (*, *) A(i), B(i)
    end do
    call margesort(P(:, 1), P(:, 2), N)

    !結果の出力
    do i = 1, Q
        if (P(A(i), 2) < P(P(B(i), 1), 2)) then !Aの方が前
            write (*, *) A(i)
        else !Bの方が前
            write (*, *) B(i)
        end if
    end do
contains
    subroutine margesort(x, y, n)
        integer N
        integer x(N), tmp(N)
        integer y(N), tmp2(N)
        integer start, end
        start = 1; end = N
        call loop_margesort(x, y, tmp, tmp2, N, start, end)
    end subroutine
    recursive subroutine loop_margesort(x, y, tmp, tmp2, N, left, right)
        integer left, right, mid
        integer N
        integer x(N), tmp(N)
        integer y(N), tmp2(N)
        integer i, j, k
        if (left >= right) return
        mid = (left + right)/2
        call loop_margesort(x, y, tmp, tmp2, N, left, mid)
        call loop_margesort(x, y, tmp, tmp2, N, mid + 1, right)
        j = 0
        tmp(left:mid) = x(left:mid)
        tmp2(left:mid) = y(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            tmp2(i) = y(right - j)
            j = j + 1
        end do
        i = left
        j = right
        do k = left, right
            if (tmp(i) < tmp(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else if (tmp(i) == tmp(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else
                x(k) = tmp(j)
                y(k) = tmp2(j)
                j = j - 1
            end if
        end do
    end subroutine loop_margesort
end program abc342b
