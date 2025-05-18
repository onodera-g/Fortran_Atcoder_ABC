program ABC390c
    ! H            : グリッドの行数。
    ! W            : グリッドの列数。
    ! S            : 各行の状態を示す文字列の配列。長さ W の文字列が H 個。
    ! i, j         : ループカウンタ（行、列のインデックス）。
    ! min_r        : 固定で黒 ('#') のマスの最小行番号。
    ! max_r        : 固定で黒 ('#') のマスの最大行番号。
    ! min_c        : 固定で黒 ('#') のマスの最小列番号。
    ! max_c        : 固定で黒 ('#') のマスの最大列番号。
    ! ch           : グリッド上のマスの文字を一時的に格納する変数。
    use, intrinsic :: iso_fortran_env
    implicit none

    integer(int32)         :: H, W, i, j
    integer(int32)         :: min_r, max_r, min_c, max_c
    character(len=:), allocatable :: S(:)
    character              :: ch

    ! 入力の読み込み
    read (*, *) H, W
    allocate (character(len=W) :: S(H))
    do i = 1, H
        read (*, '(A)') S(i)
    end do

    ! 固定黒マスの境界を初期化
    min_r = H + 1
    max_r = 0
    min_c = W + 1
    max_c = 0

    ! 固定黒マス ('#') の最小／最大行・列を求める
    do i = 1, H
        do j = 1, W
            ch = S(i) (j:j)
            if (ch == '#') then
                if (i < min_r) then
                    min_r = i
                end if
                if (i > max_r) then
                    max_r = i
                end if
                if (j < min_c) then
                    min_c = j
                end if
                if (j > max_c) then
                    max_c = j
                end if
            end if
        end do
    end do

    ! 境界内に固定白マス ('.') があれば不可能
    do i = min_r, max_r
        do j = min_c, max_c
            ch = S(i) (j:j)
            if (ch == '.') then
                print *, 'No'
                stop
            end if
        end do
    end do

    ! 上記をクリアすれば長方形化可能
    print *, 'Yes'
end program ABC390c
