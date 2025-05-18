program abc389a
    ! S    : 入力の3文字文字列（例 "3x8"）
    ! a    : 文字列 S の1文字目を数値に変換したもの (1～9)
    ! b    : 文字列 S の3文字目を数値に変換したもの (1～9)
    ! ans  : 積 a × b
    implicit none
    character(len=3) :: S
    integer :: a, b, ans

    ! 入力
    read (*, '(A)') S

    ! 文字から数値に変換
    a = ichar(S(1:1)) - ichar('0')
    b = ichar(S(3:3)) - ichar('0')

    ! 積を計算
    ans = a*b

    ! 出力
    print *, ans
end program abc389a
