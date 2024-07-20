program abc350b
    !N：歯の総数
    !Q：治療回数
    !T：治療する歯の位置
    !tooth：各歯が抜歯後(=0)か治療前(=1)かを管理
    implicit none
    integer N, Q, i
    integer, allocatable::T(:), tooth(:)

    !入力
    read (*, *) N, Q
    allocate (T(Q), tooth(N))
    read (*, *) T
    tooth = 1

    !歯の治療
    do i = 1, Q
        if (tooth(T(i)) == 1) then !歯がある(=1)なら抜く
            tooth(T(i)) = tooth(T(i)) - 1
        else !歯がある(=0)なら治療
            tooth(T(i)) = tooth(T(i)) + 1
        end if
    end do

    !結果の出力
    write (*, *) sum(tooth)
end program abc350b
