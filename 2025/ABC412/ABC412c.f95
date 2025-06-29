program ABC412c
    ! T            : テストケース数
    ! N            : 各テストケースでのドミノ数
    ! S(200000)    : ドミノの大きさを保持する配列
    ! used(200000) : ドミノが既に使われたかを示すフラグ
    ! result(100000) : 各テストケースの答えを保存する配列
    ! current      : 現在右端にあるドミノの大きさ
    ! count        : 現在までに並べたドミノの数（ドミノ1を含む）
    ! best         : 次に置くドミノのインデックス
    ! best_value   : 条件を満たす中で最大の大きさを持つドミノの大きさ

    implicit none
    integer, parameter :: MAXN = 200000, MAXT = 100000
    integer :: T, N, i, j
    integer :: S(MAXN)
    logical :: used(MAXN)
    integer :: result(MAXT)
    integer :: current, count, best, best_value

    ! 入力
    read (*, *) T

    do i = 1, T
        ! ドミノ数と大きさを読み込み
        read (*, *) N
        read (*, *) (S(j), j=1, N)

        ! used 配列を初期化（全て未使用に）
        used(1:N) = .false.

        ! ドミノ1を最初に使用
        used(1) = .true.
        current = S(1)
        count = 1

        ! ドミノを並べる処理（条件を満たす限り繰り返す）貪欲法
        do
            ! 今のドミノから 2倍以内でドミノNが倒せるなら終了
            if (2*current >= S(N)) then
                ! ドミノNを最後に置いて合計個数を出力
                result(i) = count + 1
                exit
            end if

            ! 次に置く候補のうち最大のドミノを探す
            best = -1
            best_value = -1

            do j = 2, N - 1
                if (.not. used(j)) then
                    if (S(j) <= 2*current) then
                        if (S(j) > best_value) then
                            best = j
                            best_value = S(j)
                        end if
                    end if
                end if
            end do

            ! 候補が見つからなければ -1 を記録して終了
            if (best == -1) then
                result(i) = -1
                exit
            end if

            ! 最適なドミノを右に追加し、状態を更新
            current = S(best)
            used(best) = .true.
            count = count + 1
        end do
    end do

    ! 結果の出力
    do i = 1, T
        print *, result(i)
    end do

end program ABC412c
