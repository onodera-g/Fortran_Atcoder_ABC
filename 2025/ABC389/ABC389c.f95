program ABC389c
    ! q           : クエリの数
    ! inputs_len  : クエリ種別ごとの追加入力数（kind=1→1, 2→0, 3→1）
    ! inputs      : クエリ種別に応じた追加入力を格納する配列
    ! kind        : クエリの種類（1, 2, 3）
    ! i           : ループカウンタ
    ! head_idx    : キュー先頭の配列インデックス
    ! tail_idx    : キュー末尾の次の配列インデックス
    ! cap         : キューの容量（Q）
    ! prev_idx    : 末尾直前のインデックス
    ! idx         : type 3で参照するインデックス
    ! offset      : 全体の頭位置シフト量（タイプ2の累積シフト）
    ! m           : 一時的にヘビの長さを格納する変数
    ! stored_head : 各ヘビの「頭位置 − offset」を格納する配列
    ! len_arr     : 各ヘビの長さを格納する配列
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int32) :: q
    integer(int32), parameter :: inputs_len(*) = [1, 0, 1]
    integer(int32) :: inputs(1)
    integer(int32) :: kind, i
    integer(int32) :: head_idx, tail_idx, cap, prev_idx, idx
    integer(int64) :: offset, m
    integer(int64), allocatable :: stored_head(:), len_arr(:)

    ! 入力クエリ数の読み込み
    read (input_unit, *) q

    ! 配列確保と初期化
    cap = q
    allocate (stored_head(cap))
    allocate (len_arr(cap))
    offset = 0_int64
    head_idx = 1
    tail_idx = 1

    do i = 1, q
        read (input_unit, *) kind, inputs(1:inputs_len(kind))
        select case (kind)
        case (1)
            ! タイプ1：長さ l のヘビを末尾に追加
            m = int(inputs(1), kind=int64)
            prev_idx = tail_idx - 1
            if (prev_idx == 0) prev_idx = cap
            if (head_idx == tail_idx) then
                stored_head(tail_idx) = -offset
            else
                stored_head(tail_idx) = stored_head(prev_idx) + len_arr(prev_idx)
            end if
            len_arr(tail_idx) = m
            tail_idx = tail_idx + 1
            if (tail_idx > cap) tail_idx = 1

        case (2)
            ! タイプ2：先頭のヘビを取り出し、全体を左にシフト
            m = len_arr(head_idx)
            head_idx = head_idx + 1
            if (head_idx > cap) head_idx = 1
            offset = offset - m

        case (3)
            ! タイプ3：先頭から k 番目のヘビの頭位置を出力
            idx = inputs(1)
            idx = head_idx + idx - 1
            if (idx > cap) idx = idx - cap
            write (output_unit, *) offset + stored_head(idx)

        end select
    end do

end program ABC389c
