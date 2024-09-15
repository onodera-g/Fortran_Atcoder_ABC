program abc371b
    !N：家の数
    !M：赤ちゃんの数
    !A：家の番号
    !B：赤ちゃんの性別 ('M' or 'F')
    !house：i番目に生まれた赤ちゃんの家番号
    !gender：i番目に生まれた赤ちゃんの性別 ('M' or 'F')
    !first_boy：各家に最初の男の子が生まれたかどうかを記録する配列
    implicit none
    integer N, M, i, house
    character(1) :: gender
    logical first_boy(100)
    integer, allocatable ::A(:)
    character(1), allocatable:: B(:)

    !各家にまだ男の子が生まれていない状態で初期化
    first_boy = .false.

    !入力
    read (*, *) N, M
    allocate (A(N), B(N))
    do i = 1, M
        read (*, *) A(i), B(i)
    end do

    !M人の赤ちゃんに対する処理
    do i = 1, M
        house = A(i); gender = B(i)
        !男の子の場合
        if (gender == 'M') then
            !その家にまだ男の子が生まれていない場合は「太郎」
            if (.not. first_boy(house)) then
                print *, 'Yes'
                first_boy(house) = .true. !その家に最初の男の子が生まれたことを記録
            else
                print *, 'No'
            end if
        else
            !女の子の場合は必ず「No」
            print *, 'No'
        end if
    end do
end program abc371b
