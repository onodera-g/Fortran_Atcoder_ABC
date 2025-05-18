program abc389b
    ! X    : 与えられる整数 (2 ≤ X ≤ 3×10^18)
    ! f    : 現在の階乗値を保持する変数
    ! N    : 階乗の被演算数
    implicit none
    integer(kind=8) :: X, f
    integer :: N

    ! 入力
    read (*, *) X

    ! 階乗を順に計算し、X と一致する N を探す
    f = 1_8
    do N = 1, 20
        f = f*N
        if (f == X) then
            print *, N
            exit
        end if
    end do

end program abc389b
