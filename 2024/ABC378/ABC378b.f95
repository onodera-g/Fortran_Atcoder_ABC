program abc378b
    ! N        : ゴミの種類数
    ! q        : ゴミの番号
    ! r        : ゴミの種類に応じた回収日
    ! QQ       : 出したゴミの総数
    ! t        : 出したゴミの種類
    ! d        : ゴミを出した日
    ! diff     : 次のゴミ回収日までの日数
    ! gomi_mod : d日がゴミ回収日であるかを判定用する時のあまり格納
    implicit none
    integer(16) i
    integer(16) N, gomi_mod, QQ, diff
    integer(16), allocatable::q(:), r(:), t(:), d(:)

    !入力
    read (*, *) N
    allocate (q(N), r(N))
    do i = 1, N
        read (*, *) q(i), r(i)
    end do
    read (*, *) QQ
    allocate (t(QQ), d(QQ))
    do i = 1, QQ
        read (*, *) t(i), d(i)
    end do

    ! クエリの処理
    do i = 1, QQ
        ! 不足日数の計算
        gomi_mod = mod(d(i), q(t(i)))
        if (gomi_mod == r(t(i))) then ! ゴミ出し日=回収日
            diff = 0
        elseif (gomi_mod < r(t(i))) then ! ゴミ出し日より後に回収日
            diff = r(t(i)) - gomi_mod
        else
            diff = q(t(i)) - gomi_mod + r(t(i)) ! ゴミ出し日より後に回収日(ゴミ出し日までにあまり0を挟む)
        end if
        ! 結果の出力
        write (*, *) d(i) + diff
    end do

end program abc378b
