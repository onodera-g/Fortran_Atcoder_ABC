program abc377c
    ! N      : マス目の数
    ! M      : コマの数
    ! move_x : コマの移動量
    ! move_y : コマの移動量
    ! x      : コマが移動できるマス(x 座標)
    ! y      : コマが移動できるマス(y 座標)
    ! str_x  : x を文字型に変換
    ! str y  : y を文字型に変換
    ! cnt    : 置くことのできないコマの数
    use stdlib_kinds
    use stdlib_hashmaps
    use stdlib_hashmap_wrappers
    use stdlib_sorting
    implicit none
    ! hashmap
    type(key_type)              :: key
    type(other_type)            :: other
    type(chaining_hashmap_type) :: map
    type dummy_type
        integer(16)             :: value(1)
    end type dummy_type
    type(dummy_type)            :: dummy
    class(*), allocatable       :: data
    logical :: present

    ! 変数
    integer(16) i, j
    integer(16) N, M, x, y, cnt, tmp
    integer(16), allocatable :: a(:), b(:)
    integer(16) :: move_x(8) = [1, 2, 2, 1, -1, -2, -2, -1]
    integer(16) :: move_y(8) = [2, 1, -1, -2, -2, -1, 1, 2]
    character(100) x_str, y_str, arr

    ! hashmap の初期化
    call map%init(fnv_1_hasher)

    ! 入力
    read (*, *) N, M
    allocate (a(M), b(M))
    do i = 1, M
        read (*, *) a(i), b(i)
    end do

    ! 取られる可能性のあるマスを保存
    dummy%value = 1
    call set(other, dummy)
    cnt = 0; tmp = 0
    do i = 1, M
        ! コマが置かれてる場所は除外
        write (x_str, *) a(i)
        write (y_str, *) b(i)
        arr = trim(x_str)//","//trim(y_str)
        ! hash map に登録済か確認
        key%value = transfer(arr, key%value)
        call map%key_test(key, present)
        if (present .eqv. .false.) then ! 未登録
            cnt = cnt + 1
            call map%map_entry(key, other)
        end if

        ! コマの移動で取られる可能があるマスの検証
        do j = 1, 8
            if (a(i) + move_x(j) >= 1 .and. a(i) + move_x(j) <= N) then
                x = a(i) + move_x(j)
                write (x_str, *) x
            else
                cycle
            end if
            if (b(i) + move_y(j) >= 1 .and. b(i) + move_y(j) <= N) then
                y = b(i) + move_y(j)
                write (y_str, *) y
            else
                cycle
            end if
            arr = trim(x_str)//","//trim(y_str)
            ! hash map に登録済か確認
            key%value = transfer(arr, key%value)
            call map%key_test(key, present)
            if (present .eqv. .false.) then ! 未登録
                cnt = cnt + 1
                call map%map_entry(key, other)
            end if
        end do
    end do

    ! 結果の出力
    write (*, *) N**2 - cnt
end program abc377c
