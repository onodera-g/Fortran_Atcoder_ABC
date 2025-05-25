program ABC391a
    ! D    : 入力される方角を表す文字列 (`N`,`E`,`W`,`S`,`NE`,`NW`,`SE`,`SW`)
    ! opp  : D の反対の方角を格納する文字列

    implicit none
    character(len=2) :: D, opp

    ! 入力読み込み：2文字分を読み、trim で余分な空白を除去
    read (*, '(A)') D
    D = adjustl(trim(D))

    ! 反対方向を判定
    select case (D)
    case ("N")
        opp = "S"
    case ("S")
        opp = "N"
    case ("E")
        opp = "W"
    case ("W")
        opp = "E"
    case ("NE")
        opp = "SW"
    case ("SW")
        opp = "NE"
    case ("NW")
        opp = "SE"
    case ("SE")
        opp = "NW"
    end select

    ! 結果の出力
    write (*, '(A)') trim(opp)

end program ABC391a
