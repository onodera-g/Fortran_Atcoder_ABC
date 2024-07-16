program abc352c
    !N   ：巨人の人数
    !Ans ：高さの最大値
    !A   ：巨人の肩の高さ
    !B   ：巨人の頭の高さ
    !diff：肩から頭までの高さ
    implicit none
    integer(16) N, Ans, i
    integer(16), allocatable::A(:), B(:), diff(:)

    !入力
    read (*, *) N
    allocate (A(N), B(N), diff(N))
    do i = 1, N
        read (*, *) A(i), B(i)
    end do
    Ans = 0

    !肩から頭までの高さ
    diff = B - A
    call margesort(diff, A, N)

    !順番に肩に乗せていく
    do i = 1, N
        Ans = Ans + A(i)
    end do
    Ans = Ans + diff(N) !一番上は頭がでかい巨人を乗せる

    !結果の出力
    write (*, *) Ans
contains
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
end program abc352c
