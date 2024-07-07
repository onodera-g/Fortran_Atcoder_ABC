program abc361b
    !x1,y1,z1：1つ目の直方体の左下座標(a,b,c)
    !x2,y2,z2：1つ目の直方体の右上座標(d,e,f)
    !x3,y3,z3：2つ目の直方体の左下座標(g,h,i)
    !x4,y4,z4：2つ目の直方体の右上座標(j,k,l)
    implicit none
    integer x1, y1, z1, x2, y2, z2 !a,b,c,d,e,f
    integer x3, y3, z3, x4, y4, z4 !g,h,i,j,k,l

    !入力
    read (*, *) x1, y1, z1, x2, y2, z2
    read (*, *) x3, y3, z3, x4, y4, z4

    !当たり判定の判定(X,Y,Zでどれか1つでも当たっていなければNoになる)
    !X軸方向
    if (max(x1, x3) > min(x2, x4)) then
        write (*, '(a)') 'No'
        stop
    end if

    !Y軸方向
    if (max(y1, y3) > min(y2, y4)) then
        write (*, '(a)') 'No'
        stop
    end if

    !Z軸方向
    if (max(z1, z3) > min(z2, z4)) then
        write (*, '(a)') 'No'
        stop
    end if

    !結果の出力
    write (*, '(a)') 'Yes'
end program abc361b

