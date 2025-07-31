program abc416b
    ! S           : 入力として与えられる文字列 ('.' と '#' のみ)
    ! T           : 出力用の文字列
    ! n           : 入力文字列 S の長さ
    ! i           : 走査用のループ変数
    ! start_pos   : '.' 区間の開始位置
    ! end_pos     : '.' 区間の終了位置
    ! mid         : '.' 区間において 'o' を配置する位置
    ! first_block : 最初の '.' 区間かどうかを判定する論理値
    ! has_sharp   : 入力 S に '#' が含まれているかを示す論理値

    implicit none
    character(100) :: S, T
    integer :: n, i, start_pos, end_pos, mid
    logical :: first_block, has_sharp

    ! 入力
    read (*, '(A)') S
    n = len_trim(S)
    T = S

    ! '#' が含まれているか確認
    has_sharp = index(S(1:n), "#") > 0
    first_block = .true.
    i = 1

    ! S を走査して '.' 区間を処理
    do while (i <= n)
        if (S(i:i) == "#") then
            ! '#' はそのまま
            i = i + 1
        else
            ! '.' の連続区間を検出
            start_pos = i
            do while (i <= n .and. S(i:i) == ".")
                i = i + 1
            end do
            end_pos = i - 1

            ! 区間を '.' に初期化
            T(start_pos:end_pos) = repeat('.', end_pos - start_pos + 1)

            ! 配置ルールに従って 'o' を置く
            if (.not. has_sharp) then
                ! '#' がない場合 → 1区間 → 中央配置
                mid = (start_pos + end_pos)/2
                T(mid:mid) = "o"
            else if (first_block) then
                ! '#' がある場合 → 最初の '.' 区間は左端
                T(start_pos:start_pos) = "o"
                first_block = .false.
            else
                ! 2つ目以降の '.' 区間 → 中央(右寄り)に配置
                mid = (start_pos + end_pos + 1)/2
                T(mid:mid) = "o"
            end if
        end if
    end do

    ! 結果の出力
    print '(A)', trim(T)
end program abc416b
