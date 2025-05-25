program ABC391C
    ! N            : 鳩の数（巣の数）、Q : クエリの数
    ! inputs_len   : クエリ種別ごとの追加入力数 (1→2, 2→0)
    ! inputs(2)    : タイプ1クエリの P, H を格納する配列
    ! t            : クエリの種類 (1=移動, 2=出力)
    ! pigeon(N)    : 各鳩 i が現在いる巣の番号
    ! cnt(N)       : 各巣 j にいる鳩の頭数
    ! multiCount   : 鳩が2羽以上いる巣の個数
    ! oldNest      : 鳩移動前の巣番号
    ! i            : ループ用変数

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: N, Q, t, i, oldNest
    integer(int32), parameter :: inputs_len(2) = [2, 0]
    integer(int32) :: inputs(2)
    integer(int32), allocatable :: pigeon(:), cnt(:)
    integer(int32) :: multiCount

    ! 入力
    read (*, *) N, Q
    allocate (pigeon(N), cnt(N))

    ! 初期状態：鳩 i は巣 i、各巣に1羽ずつ
    do i = 1, N
        pigeon(i) = i
        cnt(i) = 1
    end do
    multiCount = 0

    ! クエリ処理
    do i = 1, Q
        ! クエリ種別 t と、必要なら inputs を読み込む
        read (*, *) t, inputs(1:inputs_len(t))
        select case (t)
        case (1)
            ! タイプ1: 鳩 P=inputs(1) を巣 H=inputs(2) へ移動
            oldNest = pigeon(inputs(1))
            ! 移動元の巣から1羽減らす
            cnt(oldNest) = cnt(oldNest) - 1
            if (cnt(oldNest) == 1) then
                multiCount = multiCount - 1
            end if
            ! 鳩の現在地更新
            pigeon(inputs(1)) = inputs(2)
            ! 移動先の巣に1羽追加
            if (cnt(inputs(2)) == 1) then
                multiCount = multiCount + 1
            end if
            cnt(inputs(2)) = cnt(inputs(2)) + 1

        case (2)
            ! タイプ2: multiCount を出力
            print *, multiCount
        end select
    end do

end program ABC391C
