program ABC396b
    ! Q      : クエリ数
    ! t      : クエリ種別 (1: push, 2: pop)
    ! x      : push 時に読み込む値
    ! stack  : カードの山を表すスタック (十分な容量)
    ! top    : スタックの現在サイズ
    ! i      : ループ用インデックス
    ! line   : 入力行を丸ごと読み込むバッファ

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: Q, t, x
    integer(int32), parameter :: MAXS = 200000
    integer(int32) :: stack(MAXS)
    integer(int32) :: top, i
    character(len=32) :: line
    integer :: ios

    ! 初期状態: 0 を 100 枚プッシュ
    top = 100
    do i = 1, top
        stack(i) = 0
    end do

    ! 入力
    read (*, *) Q

    ! 各クエリを一行ずつ読み込み
    do i = 1, Q
        read (*, '(A)') line ! 一行まるごと読込
        read (line, *, IOSTAT=ios) t
        if (ios /= 0) stop "Input read error"
        if (t == 1) then
            ! "1 x" の場合: x も読み込んでプッシュ
            read (line, *, IOSTAT=ios) t, x
            if (ios /= 0) stop "Input read error"
            top = top + 1
            stack(top) = x
        else
            ! タイプ2: ポップして出力
            print *, stack(top)
            top = top - 1
        end if
    end do

end program ABC396b
