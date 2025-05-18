program abc387a
    ! A: 入力として与えられる1つ目の正整数 (1～2025)
    ! B: 入力として与えられる2つ目の正整数 (1～2025)
    ! sum: A + B の和を格納する変数
    ! result: (A + B)**2 の計算結果を格納する変数
    implicit none
    integer :: A, B
    integer :: sum
    integer :: result

    ! 入力
    read (*, *) A, B

    ! 計算
    sum = A + B
    result = sum*sum

    ! 出力
    print *, result
end program abc387a
