program ABC398c
    ! N       : 人数
    ! arr     : 各人が持つ数を格納する配列
    ! idx     : ソート時に元の人番号を保持する配列 (降順ソート後の対応)
    ! cnt     : 同値の連続出現回数をカウントする変数
    ! ans     : 条件を満たす人の番号（存在しない場合は -1）

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: N, i, cnt, ans
    integer(int32), allocatable :: arr(:), idx(:)

    ! 入力
    read (*, *) N
    allocate (arr(N), idx(N))
    read (*, *) (arr(i), i=1, N)
    do i = 1, N
        idx(i) = i
    end do

    call margesort_pairs(arr, idx, N)

    ! 連続する同じ値をカウントし、1度だけならユニーク
    cnt = 0
    ans = -1
    do i = 1, N
        cnt = cnt + 1
        if (i < N .and. arr(i) == arr(i + 1)) then
            cycle
        end if
        if (cnt == 1) then
            ans = idx(i)
            exit
        end if
        cnt = 0
    end do

    ! 結果の出力
    write (*, '(I0)') ans

contains

    ! マージソート
    recursive subroutine margesort_pairs(v, id, N)
        integer(int32), intent(inout) :: v(:), id(:)
        integer(int32), intent(in)    :: N
        integer(int32), allocatable    :: tmpv(:), tmpid(:)

        allocate (tmpv(N), tmpid(N))
        call loop_margesort(v, id, tmpv, tmpid, 1, N)
    end subroutine

    recursive subroutine loop_margesort(v, id, tmpv, tmpid, left, right)
        integer(int32), intent(inout) :: v(:), id(:)
        integer(int32), intent(inout) :: tmpv(:), tmpid(:)
        integer(int32), intent(in)    :: left, right
        integer(int32) :: mid, i, j, k

        if (left >= right) return
        mid = (left + right)/2
        call loop_margesort(v, id, tmpv, tmpid, left, mid)
        call loop_margesort(v, id, tmpv, tmpid, mid + 1, right)

        ! 左半分をそのままコピー
        do i = left, mid
            tmpv(i) = v(i)
            tmpid(i) = id(i)
        end do
        ! 右半分を逆順コピー
        do i = mid + 1, right
            tmpv(i) = v(right - (i - (mid + 1)))
            tmpid(i) = id(right - (i - (mid + 1)))
        end do

        i = left
        j = right
        do k = left, right
            ! 降順に並べるため >= を使用
            if (tmpv(i) >= tmpv(j)) then
                v(k) = tmpv(i)
                id(k) = tmpid(i)
                i = i + 1
            else
                v(k) = tmpv(j)
                id(k) = tmpid(j)
                j = j - 1
            end if
        end do
    end subroutine

end program ABC398c
