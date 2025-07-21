program abc413c
    ! Q                : クエリの数
    ! query_type       : クエリのタイプ (1 または 2)
    ! c                : タイプ1の個数パラメータ
    ! x                : タイプ1の値パラメータ
    ! k                : タイプ2の削除個数パラメータ
    ! head             : キューの先頭位置
    ! tail             : キューの末尾位置
    ! value(2*Q)       : キューに入っている値を格納する配列
    ! count(2*Q)       : キューに入っている値の個数を格納する配列
    ! sum              : タイプ2のクエリで出力する合計値
    ! take             : 今回処理する個数
    ! i                : ループ変数
    ! line             : 入力クエリ行を文字列で一時的に格納する

    implicit none
    integer :: Q, query_type, i
    integer(kind=8) :: c, x, k, sum, take
    integer :: head, tail
    integer(kind=8), allocatable :: value(:), count(:)
    character(len=256) :: line

    ! クエリ数を読み込む
    read (*, *) Q
    allocate (value(Q*2))
    allocate (count(Q*2))
    head = 1
    tail = 0

    ! Q 回のクエリを処理する
    do i = 1, Q
        ! 1行を文字列として読み込む
        read (*, '(A)') line

        ! 先頭の数値を読み込んでクエリ種別を判定
        read (line, *) query_type

        select case (query_type)

        case (1)
            ! タイプ1: 末尾に c 個の x を追加
            read (line, *) query_type, c, x
            tail = tail + 1
            value(tail) = x
            count(tail) = c

        case (2)
            ! タイプ2: 先頭から k 個削除し、その合計を出力
            read (line, *) query_type, k
            sum = 0
            do while (k > 0)
                ! 先頭の要素から取り出せる個数を計算
                take = min(k, count(head))
                ! 合計に加算
                sum = sum + value(head)*take
                ! 残り個数を更新
                count(head) = count(head) - take
                k = k - take
                ! 先頭の要素が尽きたら次の要素へ移動
                if (count(head) == 0) then
                    head = head + 1
                end if
            end do
            ! 合計を出力
            print *, sum
        end select
    end do

end program abc413c
