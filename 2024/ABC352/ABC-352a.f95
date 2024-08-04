program abc352a
    !N：駅の総数
    !X：出発駅
    !Y：到着駅
    !Z：途中に停車したい駅
    implicit none
    integer N, X, Y, Z, i

    !入力
    read (*, *) N, X, Y, Z

    !停車駅を順番に判定
    !XからY方向に電車が移動
    if (Y > X) then
        do i = X, Y
            if (i == Z) then
                write (*, '(a)') 'Yes'
                stop
            end if
        end do
        !YからX方向に電車が移動
    else
        do i = X, Y, -1
            if (i == Z) then
                write (*, '(a)') 'Yes'
                stop
            end if
        end do
    end if
    write (*, '(a)') 'No'

end program abc352a
