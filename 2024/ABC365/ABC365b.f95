program abc365b
    !N    ：数列の長さ
    !A    ：長さNの数列
    !A_num：並び替え前の数列の位置
    implicit none
    integer(16) i
    integer(16) N
    integer(16), allocatable::A(:), A_num(:)

    !入力
    read (*, *) N
    allocate (A(N), A_num(N))
    read (*, *) A(:)
    do i = 1, N
        A_num(i) = i
    end do

    !ソート
    call margesort(A, A_num, N)

    !結果の出力
    write (*, *) A_num(2)
end program abc365b
subroutine margesort(x, y, n)
    integer(16) N
    integer(16) x(N), tmp(N)
    integer(16) y(N), tmp2(N)
    integer(16) start, end
    start = 1; end = N
    call loop_margesort(x, y, tmp, tmp2, N, start, end)
end subroutine
recursive subroutine loop_margesort(x, y, tmp, tmp2, N, left, right)
    integer(16) left, right, mid
    integer(16) N
    integer(16) x(N), tmp(N)
    integer(16) y(N), tmp2(N)
    integer(16) i, j, k

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
        if (tmp(i) > tmp(j)) then
            x(k) = tmp(i)
            y(k) = tmp2(i)
            i = i + 1
        else if (tmp(i) == tmp(j) .and. tmp2(i) < tmp2(j)) then
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

