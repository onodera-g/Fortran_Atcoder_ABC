program abc381a
    ! N   : 入力文字列の長さ
    ! mid : 文字列の中央位置 (1-based index)
    ! S   : 入力文字列
    ! ans : 文字列が11/22文字列であるかを示すフラグ (初期値: .true.)
    implicit none
    integer N, mid, i
    character(:), allocatable :: S
    logical ans

    ! 入力
    read (*, *) N
    allocate (character(N) :: S)
    read (*, '(A)') S

    ! 条件1 : 文字列の長さが偶数の場合、11/22文字列ではない
    ans = .true.
    if (mod(N, 2) == 0) then
        ans = .false.
    else
        mid = (N + 1)/2
        ! 条件2 : 前半部分が全て '1' であるか確認
        do i = 1, mid - 1
            if (S(i:i) /= '1') then
                ans = .false.
                exit
            end if
        end do
        ! 条件3 : 中央の文字が '/' であるか確認
        if (S(mid:mid) /= '/') then
            ans = .false.
        end if
        ! 条件4 : 後半部分が全て '2' であるか確認
        do i = mid + 1, N
            if (S(i:i) /= '2') then
                ans = .false.
                exit
            end if
        end do
    end if

    ! 結果の出力
    if (ans) then
        print *, "Yes"
    else
        print *, "No"
    end if
end program abc381a
