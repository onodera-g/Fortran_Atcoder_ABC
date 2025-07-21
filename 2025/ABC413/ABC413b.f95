program abc413b
    ! N                : 文字列の個数
    ! S(100)           : 入力される文字列の配列
    ! result(10000)    : 連結結果を格納する配列
    ! count            : 異なる連結文字列の個数
    ! i, j, k          : ループ変数
    ! concat           : 連結した文字列
    ! found            : 重複確認用のフラグ
    implicit none
    integer :: N, i, j, k, count
    character(len=10) :: S(100)
    character(len=20) :: result(10000), concat
    logical :: found

    ! 入力
    read (*, *) N
    do i = 1, N
        read (*, *) S(i)
    end do

    count = 0

    ! 全ての異なる(i,j)を試す
    do i = 1, N
        do j = 1, N
            if (i == j) cycle
            concat = trim(S(i))//trim(S(j))

            ! 既にあるか確認
            found = .false.
            do k = 1, count
                if (result(k) == concat) then
                    found = .true.
                    exit
                end if
            end do

            ! 無ければ追加
            if (.not. found) then
                count = count + 1
                result(count) = concat
            end if
        end do
    end do

    ! 結果の出力
    print *, count

end program abc413b
