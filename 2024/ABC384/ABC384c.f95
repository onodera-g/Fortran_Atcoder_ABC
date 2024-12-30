program abc384c
    ! a, b, c, d, e     : 各問題A～Eの配点を格納する整数変数
    ! mask              : ビットマスク（1～31）のループカウンタ
    ! points            : 各問題の配点を格納する配列（サイズ5）
    ! problems          : 各問題の名前（'A'～'E'）を格納する配列（サイズ5）
    ! Participant       : 参加者のデータ構造
    !     name          : 参加者の名前（最大5文字）
    !     score         : 参加者の得点
    ! participants      : 31人の参加者情報を格納する配列
    ! participant_count : 生成された参加者の数をカウントする変数

    implicit none
    integer a, b, c, d, e
    integer mask, i
    integer points(5)
    character(1) problems(5)
    type Participant
        character(5) name
        integer score
    end type Participant
    type(Participant) participants(31)
    integer participant_count

    ! 入力
    problems = ['A', 'B', 'C', 'D', 'E']
    read (*, *) a, b, c, d, e
    points = (/a, b, c, d, e/)
    participant_count = 0

    ! 1から31までのビットマスク(2進数)で問題の正誤を管理
    do mask = 1, 31
        participant_count = participant_count + 1
        participants(participant_count)%name = ''
        participants(participant_count)%score = 0
        ! maskを２ビットにして、i bit目が1なら得点を加算
        do i = 0, 4
            if (btest(mask, i)) then
                ! 名前に問題を追加
                participants(participant_count)%name = trim(adjustl(participants(participant_count)%name))//trim(problems(i + 1))
                ! 得点を累積
                participants(participant_count)%score = participants(participant_count)%score + points(i + 1)
            end if
        end do
    end do

    ! ソート: 得点が高い順、同点なら名前の辞書順
    call merge_sort(participants, 1, participant_count)

    ! 結果の出力
    do i = 1, participant_count
        print *, trim(participants(i)%name)
    end do

contains

    recursive subroutine merge_sort(x, left, right)
        type(Participant), intent(inout) :: x(:)
        integer, intent(in) :: left, right
        integer :: mid

        if (left >= right) return

        mid = (left + right)/2
        call merge_sort(x, left, mid)
        call merge_sort(x, mid + 1, right)
        call merge(x, left, mid, right)
    end subroutine merge_sort

    recursive subroutine merge(x, left, mid, right)
        type(Participant), intent(inout) :: x(:)
        integer, intent(in) :: left, mid, right
        type(Participant), allocatable :: temp(:)
        integer :: i, j, k, n1, n2

        n1 = mid - left + 1
        n2 = right - mid

        allocate (temp(n1 + n2))

        ! コピー
        do i = 1, n1
            temp(i) = x(left + i - 1)
        end do
        do j = 1, n2
            temp(n1 + j) = x(mid + j)
        end do

        i = 1
        j = n1 + 1
        k = left

        ! マージ
        do while (i <= n1 .and. j <= n1 + n2)
            if (temp(i)%score > temp(j)%score) then
                x(k) = temp(i)
                i = i + 1
            else if (temp(i)%score == temp(j)%score) then
                if (temp(i)%name < temp(j)%name) then
                    x(k) = temp(i)
                    i = i + 1
                else
                    x(k) = temp(j)
                    j = j + 1
                end if
            else
                x(k) = temp(j)
                j = j + 1
            end if
            k = k + 1
        end do

        ! 残りをコピー
        do while (i <= n1)
            x(k) = temp(i)
            i = i + 1
            k = k + 1
        end do
        do while (j <= n1 + n2)
            x(k) = temp(j)
            j = j + 1
            k = k + 1
        end do

        deallocate (temp)
    end subroutine merge

end program abc384c
