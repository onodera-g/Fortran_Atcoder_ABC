program abc379c
    ! N       : 総マス数（1からNまでの番号がついたマス）
    ! M       : 初期状態で石が入っているマスの数
    ! X       : 石が最初に配置されているマスの位置を格納する配列
    ! A       : 各マスに配置されている石の数を格納する配列
    ! current : 現在の石の配置位置を管理する変数
    ! cnt     : 石をすべてのマスに1つずつ配置するための最小操作回数
    implicit none
    integer(16) i, N, M, current, cnt
    integer(16), allocatable :: X(:), A(:)

    ! 入力
    read (*, *) N, M
    allocate (X(M))
    allocate (A(M))
    read (*, *) X
    read (*, *) A

    call margesort(X, A, M)

    ! 石の総数の確認
    if (sum(A) /= N) then
        print *, "-1"
        stop
    end if

    ! 各石の配置に基づいて操作回数を計算
    cnt = 0
    current = 1
    do i = 1, M
        if (current < X(i)) then
            current = X(i)
        end if
        ! 配置可能かの確認
        if (current + A(i) - 1 > N) then
            print *, "-1"
            stop
        end if
        ! 操作回数の計算
        ! 各石をX(i)からcurrent, current+1, ..., current + A(i) -1 に移動させる
        cnt = cnt + A(i)*(current - X(i)) + (A(i)*(A(i) - 1))/2
        ! 次の配置位置を更新
        current = current + A(i)
    end do

    ! 結果の出力
    print *, cnt
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

        ! これ以上分割できないならreturn
        if (left >= right) return

        ! 分割できるだけ分割する
        mid = (left + right)/2
        call loop_margesort(x, y, tmp, tmp2, N, left, mid)
        call loop_margesort(x, y, tmp, tmp2, N, mid + 1, right)

        ! 並び替えの下準備としてtmpに配列をコピー
        j = 0
        tmp(left:mid) = x(left:mid)
        tmp2(left:mid) = y(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            tmp2(i) = y(right - j)
            j = j + 1
        end do

        ! 大小比較して小さい順に入れていく
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
end program abc379c
