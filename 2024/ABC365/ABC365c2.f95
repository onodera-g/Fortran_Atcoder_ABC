module mod_variable
    !N：参加人数
    !M：予算の上限
    !A：交通費の希望金額
    implicit none
    integer(16), allocatable:: A(:)
    integer(16) N, M

end module mod_variable
program abc365c
    !ok ：予算内となる交通費の金額
    !ng ：予算オーバーとなる交通費の金額
    !mi ：２分探索用の予算オーバーするか判定する交通費の金額
    !tmp：予算オーバーかの判定用：0：予算内、1：予算オーバー
    use mod_variable
    implicit none
    integer(16) ok, ng, mi, tmp

    !入力
    read (*, *) N, M
    allocate (A(N))
    read (*, *) A(:)

    !交通費の上限の検索
    if (sum(A) <= M) then
        write (*, '(a)') 'infinite'
        stop
    end if
    call margesort(A, N)
    ok = 0; ng = A(N)
    do while (ok + 1 < ng)
        mi = (ok + ng)/2
        tmp = 0
        !write (*, '(a2,1x,i0,1x,a2,1x,i0,1x,a2,1x,i0)') 'OK', ok, 'NG', ng, 'mi', mi
        call cal(mi, tmp)
        if (tmp == 0) then
            ok = mi
        else
            ng = mi
        end if
    end do

    !結果の出力
    write (*, '(i0)') ok

end program abc365c
subroutine margesort(x, n)
    integer(16) N
    integer(16) x(N), tmp(N)
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
subroutine cal(mi, tmp)
    use mod_variable
    integer(16) mi, tmp
    integer(16) i, Ans
    Ans = 0
    do i = 1, N
        Ans = Ans + min(A(i), mi)
        if (Ans > M) then
            tmp = 1
            return
        end if
    end do
end subroutine cal
