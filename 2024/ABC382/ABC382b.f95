program abc382b
    ! N : 箱の総数
    ! D : 高橋君がクッキーを食べる日数
    ! S : 各箱の状態を表す文字列（'@' はクッキーが入っている、'.' は空き箱）
    implicit none
    integer i, j
    integer N, D
    character(100) S
    !入力
    read (*, *) N, D
    read (*, *) S(1:N)

    ! クッキーを食べる処理
    do i = 1, D
        ! 右端にあるクッキーを空き箱に置き換える
        do j = N, 1, -1
            if (S(j:j) == "@") then
                S(j:j) = "."
                exit
            end if
        end do
    end do

    !　結果の出力
    write (*, *) S(1:N)

end program abc382b
