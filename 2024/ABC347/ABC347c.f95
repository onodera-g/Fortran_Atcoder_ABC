program abc347c
    !A   ：休日の日数(1~A日目)
    !B   ：平日の日数(A+1~A+B日目)
    !N   ：予定の個数
    !D   ：i番目の予定日
    !E   ：mod(D,A+B)
    !flag：結果の判定用(Trueなら"Yes"、Falseなら"No"）
    implicit none
    integer(16) i, tmp
    integer(16) N, A, B
    integer(16), allocatable :: D(:), E(:)
    logical flag

    ! 入力
    read (*, *) N, A, B
    allocate (D(N), E(N))
    read (*, *) D
    do i = 1, N
        E(i) = mod(D(i), A + B)
    end do

    !予定が全て休日である可能性があるかを判定
    !N=1
    if (N == 1) then
        if (E(1) >= A .and. E(1) < A + B .or. E(1) < A) then
            print *, 'Yes'
        else
            print *, 'No'
        end if
        stop
    end if
    !N>1
    call remove_overlap(E, N)
    call margesort(E, N)
    flag = .false.
    if (N > 1) then
        do i = 1, N - 1
            tmp = E(i + 1) - E(i)
            if (mod(tmp, A + B) > B) then
                flag = .true.
                exit
            end if
        end do
        !端部の判定(E_k,E_k+1)
        tmp = E(1) - E(N) + A + B
        if (mod(tmp, A + B) > B) flag = .true.
    end if

    !結果の出力
    if (flag .eqv. .true.) then
        print *, 'Yes'
    else
        print *, 'No'
    end if
contains

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
    subroutine remove_overlap(arr, N)
        integer(16) N, cnt, i, tmp
        integer(16) arr(N), out_arr(N)
        if (N == 1) return
        out_arr = arr
        arr = 0
        arr(1) = out_arr(1)
        tmp = out_arr(1)
        cnt = 2
        do i = 2, N
            if (tmp /= out_arr(i)) then
                arr(cnt) = out_arr(i)
                cnt = cnt + 1
                tmp = out_arr(i)
            end if
        end do
        N = cnt - 1
    end subroutine remove_overlap
end program abc347c
