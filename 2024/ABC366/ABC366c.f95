program abc366c
    !Q         ：クエリの総数
    !cnt       ：入っている各ボールの数
    !ans       ：入っているボールの種類
    !type      ：クエリの種類(1~3)
    !inputs_len：クエリの区別用(1,2ならスキップ)
    !inputs    ：追加、取出のボールのナンバー
    implicit none
    integer i
    integer Q, cnt(1000000), ans
    integer inputs(1), type
    integer, parameter :: inputs_len(1:3) = [1, 1, 0]

    !クエリの処理
    read (*, *) Q
    cnt = 0; ans = 0
    do i = 1, Q
        !クエリの入力
        read (*, *) type, inputs(1:inputs_len(type))
        !クエリに応じて場合分け
        select case (type)
        case (1) !追加
            cnt(inputs(1)) = cnt(inputs(1)) + 1
            if (cnt(inputs(1)) == 1) ans = ans + 1
        case (2) !取出
            cnt(inputs(1)) = cnt(inputs(1)) - 1
            if (cnt(inputs(1)) == 0) ans = ans - 1
        case (3) !出力
            print *, ans
        end select
    end do
end program abc366c

