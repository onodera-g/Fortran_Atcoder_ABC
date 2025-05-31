program ABC394d
    ! n            : 入力される文字列 S の長さ (1 ≤ n ≤ 200000)
    ! Sbuf(200000) : 固定長バッファ。入力文字列を左詰めで格納するために使用
    ! S(i)         : 1 文字ずつ格納する可変長配列。Sbuf(1:n) の各文字をコピー
    ! stack(200000): スタック配列。開き括弧を格納して対応する閉じ括弧をチェックする
    ! top          : スタックの現在のトップ位置 (0: 空, 1～n: 要素数)
    ! c            : S(i) から読み込んだ現在処理中の文字
    ! t            : stack(top) から読み出した、対応をチェックするための文字
    ! i            : ループ変数 (1～n) または (n-1→1) で配列を走査するインデックス

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: n, i, top
    character(len=200000) :: Sbuf
    character(len=1), allocatable :: S(:)
    character(len=1), allocatable :: stack(:)
    character(len=1) :: c, t

    !　入力
    read (*, '(A)') Sbuf
    n = len_trim(Sbuf)
    allocate (S(n))
    do i = 1, n
        S(i) = Sbuf(i:i)
    end do
    allocate (stack(n))
    top = 0

    !　文字列を左から走査し、スタックでマッチング判定
    do i = 1, n
        c = S(i)
        ! 左括弧 '(', '[', '<' はスタックにプッシュ
        if (c == '(' .or. c == '[' .or. c == '<') then
            top = top + 1
            stack(top) = c
        else
            ! 右括弧の場合は、まずスタックが空でないかチェック
            if (top == 0) then
                print *, "No"
                stop
            end if
            ! スタックのトップ文字を t に取り出して対応を確認
            t = stack(top)
            select case (c)
            case (')')
                if (t /= '(') then
                    print *, "No"
                    stop
                end if
            case (']')
                if (t /= '[') then
                    print *, "No"
                    stop
                end if
            case ('>')
                if (t /= '<') then
                    print *, "No"
                    stop
                end if
            case default
                ! 本問題の文字セット外の文字が来た場合も不正扱い
                print *, "No"
                stop
            end select
            ! マッチしたのでスタックからポップ
            top = top - 1
        end if
    end do

    ! 結果の出力
    if (top == 0) then
        print *, "Yes"
    else
        print *, "No"
    end if
end program ABC394d
