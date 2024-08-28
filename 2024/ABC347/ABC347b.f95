program abc347b
    !found：同一部分文字列の重複を管理するフラグ
    !cnt：ユニークな部分文字列のカウント
    !t： ユニークな部分文字列を格納する配列
    !S：文字列S
    !lenS：! 入力された文字列の長さ
    implicit none
    integer i, j, k, found
    integer cnt, lenS
    character(100) S
    character(len=100), dimension(:), allocatable :: t

    !入力
    read (*, *) S
    lenS = len_trim(S)
    allocate (t(lenS*(lenS + 1)/2))
    cnt = 0

    ! 部分文字列を生成して一意か確認
    do i = 1, lenS
        do j = i, lenS
            found = 0
            do k = 1, cnt
                if (trim(t(k)) == trim(S(i:j))) then
                    found = 1
                    exit
                end if
            end do
            if (found == 0) then
                cnt = cnt + 1
                t(cnt) = S(i:j)
            end if
        end do
    end do

    ! 結果を出力
    print *, cnt
end program abc347b
