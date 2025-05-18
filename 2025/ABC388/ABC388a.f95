program abc388a
    ! S: 入力文字列
    ! C: S の1文字目
    implicit none
    character(len=100) :: S
    character(len=1) :: C

    ! 入力
    read (*, *) S

    ! 先頭文字取得
    C = S(1:1)

    ! 出力
    write (*, '(A)') C//'UPC'
end program abc388a
