program abc362b
    !x    ：A,B,Cのx座標
    !y    ：A,B,Cのy座標
    !check：直角の判定用
    implicit none
    integer x(3), y(3)
    integer i
    logical check

    !入力
    do i = 1, 3
        read (*, *) x(i), y(i)
        !マイナスがあるとややこしいので+2000でプラス領域
        x(i) = x(i) + 2000
        y(i) = y(i) + 2000
    end do

    !直角の判定(ピタゴラスの定理)
    call deg(x(1), x(2), x(3), y(1), y(2), y(3), check)

    !結果の出力
    if (check .eqv. .true.) then
        write (*, '(a)') 'Yes'
    else
        write (*, '(a)') 'No'
    end if
contains
    !ピタゴラスの定理により直角を判定
    subroutine deg(x1, x2, x3, y1, y2, y3, check)
        !x1~x3：A,B,Cのx座標
        !y1~y3：A,B,Cのy座標
        !check：直角の判定用
        integer x1, x2, x3, y1, y2, y3
        integer AB, BC, CA
        logical check

        !確辺の長さを2乗
        AB = (x2 - x1)**2 + (y2 - y1)**2
        BC = (x3 - x2)**2 + (y3 - y2)**2
        CA = (x1 - x3)**2 + (y1 - y3)**2

        !直角判定
        check = .false.
        if (AB + BC == CA) check = .true.
        if (AB + CA == BC) check = .true.
        if (BC + CA == AB) check = .true.
    end subroutine deg
end program abc362b
