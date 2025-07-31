program abc416c
    ! N         : 文字列の個数
    ! K         : 選ぶ個数
    ! X         : 求める順位
    ! S         : 入力文字列配列
    ! results   : 全ての連結文字列を格納する配列
    ! tmp       : マージソート用の一時配列
    ! cnt       : 生成された文字列数（32bitで十分）
    implicit none
    integer, parameter :: maxn = 10, maxk = 5, max_s_len = 10, maxlen = maxk*max_s_len
    integer(8), parameter :: max_results = 10**maxk
    integer :: N, K, i
    integer(8) :: X
    integer :: cnt
    integer(8) :: path_indices(maxk)
    character(max_s_len) :: S(maxn)
    character(maxlen), allocatable :: results(:), tmp(:)

    ! 入力
    read (*, *) N, K, X
    do i = 1, N
        read (*, *) S(i)
    end do

    ! 配列割り当て
    allocate (character(maxlen) :: results(max_results))
    allocate (character(maxlen) :: tmp(max_results))
    cnt = 0

    ! DFSで全ての連結文字列を生成
    call dfs(0, path_indices)

    ! マージソートで results を辞書順ソート
    call merge_sort_str(results, tmp, 1, cnt)

    ! X番目を出力
    print '(A)', trim(results(X))

    deallocate (results, tmp)

contains
    !----------------------------------------------
    recursive subroutine dfs(depth, path_indices)
        integer, intent(in) :: depth
        integer(8), intent(inout) :: path_indices(maxk)
        integer(8) :: j
        character(maxlen) :: current

        if (depth == K) then
            cnt = cnt + 1
            current = ''
            do j = 1, K
                current = trim(current)//trim(S(path_indices(j)))
            end do
            results(cnt) = current
            return
        end if

        do j = 1, N
            path_indices(depth + 1) = j
            call dfs(depth + 1, path_indices)
        end do
    end subroutine dfs

    !----------------------------------------------
    recursive subroutine merge_sort_str(arr, tmp, left, right)
        character(len=maxlen), intent(inout) :: arr(:)
        character(len=maxlen), intent(inout) :: tmp(:)
        integer, intent(in) :: left, right
        integer :: mid

        if (left >= right) return
        mid = (left + right)/2
        call merge_sort_str(arr, tmp, left, mid)
        call merge_sort_str(arr, tmp, mid + 1, right)
        call merge_str(arr, tmp, left, mid, right)
    end subroutine merge_sort_str

    !----------------------------------------------
    subroutine merge_str(arr, tmp, left, mid, right)
        character(len=maxlen), intent(inout) :: arr(:)
        character(len=maxlen), intent(inout) :: tmp(:)
        integer, intent(in) :: left, mid, right
        integer :: i, j, k

        tmp(left:mid) = arr(left:mid)
        do i = mid + 1, right
            tmp(i) = arr(right - (i - (mid + 1)))
        end do

        i = left
        j = right
        do k = left, right
            if (tmp(i) <= tmp(j)) then
                arr(k) = tmp(i)
                i = i + 1
            else
                arr(k) = tmp(j)
                j = j - 1
            end if
        end do
    end subroutine merge_str

end program abc416c
