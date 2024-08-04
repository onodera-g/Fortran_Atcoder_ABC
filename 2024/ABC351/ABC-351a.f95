program abc351a
    !A：9回までの各回のAチームの得点
    !B：8回までの各回のAチームの得点
    implicit none
    integer A(9), B(8)

    !入力
    A = 0; B = 0
    read (*, *) A(:)
    read (*, *) B(:)

    !結果の出力
    write (*, *) sum(A) - sum(B) + 1
end program abc351a
