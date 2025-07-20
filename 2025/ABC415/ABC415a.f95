program abc415a
    ! N           : 数列の長さ
    ! X           : 判定する値
    ! A(100)      : 数列
    ! i           : ループ変数
    ! found       : X が A に含まれるかどうかを示すフラグ

    implicit none
    integer :: N, X
    integer :: A(100)
    integer :: i
    logical :: found

    ! 入力
    read (*, *) N
    read (*, *) (A(i), i=1, N)
    read (*, *) X

    found = .false.

    ! 検索
    do i = 1, N
        if (A(i) == X) then
            found = .true.
            exit
        end if
    end do

    !結果の出力
    if (found) then
        print *, 'Yes'
    else
        print *, 'No'
    end if
end program abc415a
