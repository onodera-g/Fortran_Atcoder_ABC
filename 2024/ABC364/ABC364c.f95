program abc364c
    !N    ：料理の数
    !X    ：摂取できる甘さの限界
    !Y    ：摂取できるしょっぱさの限界
    !sum_X：現在摂取している甘さの累計
    !sum_Y：現在摂取しているしょっぱさの累計
    !cnt_A：食べた料理の数
    !cnt_B：食べた料理の数
    implicit none
    integer(16) N, X, Y, sum_X, sum_Y, cnt_A, cnt_B
    integer(16) i
    integer(16), allocatable::A(:), B(:)

    !入力
    read (*, *) N, X, Y
    allocate (A(N), B(N))
    read (*, *) A(:)
    read (*, *) B(:)

    !料理を食べるシミュレーション
    call margesort(A, N)
    call margesort(B, N)
    sum_X = 0; sum_Y = 0
    cnt_A = 0; cnt_B = 0
    !甘さXを基準に食べれる限界を探る
    do i = 1, N
        cnt_A = cnt_A + 1
        if (sum_X + A(i) <= X) then
            sum_X = sum_X + A(i)
        else
            exit
        end if
    end do
    !しょっぱさYを基準に食べれる限界を探る
    do i = 1, N
        cnt_B = cnt_B + 1
        if (sum_Y + B(i) <= Y) then
            sum_Y = sum_Y + B(i)
        else
            exit
        end if
    end do

    !結果の出力
    write (*, *) min(cnt_A, cnt_B)

end program abc364c
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
        if (tmp(i) > tmp(j)) then !
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

