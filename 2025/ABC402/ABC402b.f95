program ABC402b
    ! Q      : クエリの総数
    ! line   : 入力行をまるごと受け取るバッファ
    ! t      : クエリ種別 (1 or 2)
    ! X      : タイプ1 のとき並ぶ人が持つメニュー番号
    ! p      : トークン分割用インデックス
    ! queue  : 待ち行列を表す配列
    ! front  : 待ち行列先頭のインデックス
    ! back   : 待ち行列末尾のインデックス
    ! i      : ループカウンタ

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32), parameter :: MAXQ = 100
    integer(int32) :: Q, t, X, p
    character(len=32) :: line
    integer(int32) :: queue(MAXQ)
    integer(int32) :: front, back
    integer(int32) :: i

    ! 入力
    read (*, *) Q
    front = 1
    back = 0

    ! 各クエリ処理
    do i = 1, Q
        read (*, '(A)') line !入力を文字列として読み込む
        read (line, *) t ! lineから頭1文字を取り出す
        if (t == 1) then
            ! タイプ1: 「1 X」のうち、X を取り出す
            p = index(line, ' ') ! 空白の位置
            if (p > 0) then
                read (line(p + 1:), *) X ! 空白以降をパース
            end if
            ! enqueue
            back = back + 1
            queue(back) = X
        else
            ! タイプ2: dequeue して出力
            write (*, '(I0)') queue(front)
            front = front + 1
        end if
    end do

end program ABC402b
