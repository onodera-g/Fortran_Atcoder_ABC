program abc355b
    implicit none
    integer(16) i, j
    integer(16) N, M, cnt
    integer(16), allocatable::A(:), B(:), C(:)
    character(1), allocatable:: Ac(:), Bc(:), Cc(:)
    character(2) check(2)

    !入力
    read (*, *) N, M
    allocate (A(N), B(M), C(N + M))
    allocate (Ac(N), Bc(M), Cc(N + M))
    read (*, *) (A(i), i=1, N)
    read (*, *) (B(i), i=1, M)
    Ac = "A"; Bc = "B"
    C(1:N) = A(1:N)
    C(N + 1:M) = B(1:M)
    Cc(1:N) = Ac(1:N)
    Cc(N + 1:M) = Bc(1:M)
    check(1) = "AA"
    check(2) = "BB"

    !
    call margesort(C, Cc, N + M)
    do i = 1, N + M - 1
        if (Cc(i) == "A" .and. Cc(i + 1) == "A") then
            write (*, '(a)') 'Yes'
            stop
        end if
    end do
    write (*, '(a)') 'No'

contains

    subroutine margesort(x, y, n)
        integer(16) N
        integer(16) x(N), tmp(N)
        character(1) y(N), tmp2(N)
        integer(16) start, end
        start = 1; end = N
        call loop_margesort(x, y, tmp, tmp2, N, start, end)
    end subroutine
    recursive subroutine loop_margesort(x, y, tmp, tmp2, N, left, right)
        integer(16) left, right, mid
        integer(16) N
        integer(16) x(N), tmp(N)
        character(1) y(N), tmp2(N)
        integer(16) i, j, k

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call loop_margesort(x, y, tmp, tmp2, N, left, mid)
        call loop_margesort(x, y, tmp, tmp2, N, mid + 1, right)

        !並び替えの下準備としてtmpに配列をコピー
        j = 0
        tmp(left:mid) = x(left:mid)
        tmp2(left:mid) = y(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            tmp2(i) = y(right - j)
            j = j + 1
        end do

        !大小比較して小さい順に入れていく
        i = left
        j = right
        !write (*, '(3x,*(f13.101x),a)', advance='no') x(left:right)
        !write (*, '(a)', advance='no') '>>'
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
        !write (*, '(3x,*(f13.10,1x))') x(left:right)
    end subroutine loop_margesort
end program abc355b
