program abc385b
    ! H,W                  : グリッドの行列
    ! X,Y                  : サンタクロースの初期位置
    ! T                    : 移動指示の文字列
    ! tmp                  : グリッドの各行を一時的に格納する文字列
    ! S                    : グリッドの情報（H x W）各セルの状態を表す
    ! houses               : 通過または到達した家を記録する配列（H x W）
    ! current_x, current_y : サンタクロースの現在の位置
    ! count                : 通過または到達した家の数
    ! new_x, new_y         : 移動後の新しい位置
    implicit none
    integer H, W, X, Y
    integer i, j
    character(10005) T
    character(100) tmp
    character, allocatable :: S(:, :)
    integer, allocatable ::houses(:, :)
    integer current_x, current_y, count
    integer new_x, new_y

    ! 入力
    read (*, *) H, W, X, Y
    allocate (S(H, W))
    allocate (houses(H, W)); houses = 0
    do i = 1, H
        read (*, '(A)') tmp
        do j = 1, W
            S(i, j) = tmp(j:j)
        end do
    end do
    read (*, '(A)') T

    ! サンタクロースの初期位置を設定
    current_x = X
    current_y = Y

    ! 各移動指示に従って移動をシミュレーション
    do i = 1, len_trim(T)
        ! 仮でサンタを移動させる
        new_x = current_x
        new_y = current_y
        select case (T(i:i))
        case ('U')
            new_x = current_x - 1
        case ('D')
            new_x = current_x + 1
        case ('L')
            new_y = current_y - 1
        case ('R')
            new_y = current_y + 1
        end select

        ! 移動先が通行可能なら移動先を確定
        if (new_x >= 1 .and. new_x <= H .and. new_y >= 1 .and. new_y <= W) then
            if (S(new_x, new_y) == '.' .or. S(new_x, new_y) == '@') then
                current_x = new_x
                current_y = new_y
            end if
        end if

        ! 現在のセルに家がある場合、記録する
        if (S(current_x, current_y) == '@') then
            if (houses(current_x, current_y) == 0) houses(current_x, current_y) = 1
        end if
    end do

    ! 通過または到達した家の数をカウント
    count = sum(houses)

    ! 結果の出力
    write (*, *) current_x, current_y, count
end program abc385b
