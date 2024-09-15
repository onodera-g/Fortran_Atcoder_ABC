program abc371c
    ! N：頂点の数
    ! MG：グラフGのエッジの数
    ! MH：グラフHのエッジの数
    ! u：グラフGのi番目のエッジにおける1つ目の頂点
    ! v：グラフGのi番目のエッジにおける2つ目の頂点
    ! au：グラフHのi番目のエッジにおける1つ目の頂点
    ! av：グラフHのi番目のエッジにおける2つ目の頂点
    ! cost：頂点i-jを繋ぐまたは消すのにかかるコストを格納する配列
    ! G：グラフGの頂点間にエッジがあるかどうかを示す配列
    ! H：グラフHの頂点間にエッジがあるかどうかを示す配列
    ! perm：頂点の順列を格納する配列
    ! used：頂点がすでに順列に使われたかを管理する配列
    ! min_cost：エッジ操作の最小コスト
    implicit none
    integer N, MG, MH
    integer u(8*(8 - 1)/2), v(8*(8 - 1)/2)
    integer au(8*(8 - 1)/2), av(8*(8 - 1)/2)
    integer cost(8, 8)
    logical G(8, 8), H(8, 8)
    integer i, j
    integer(8) min_cost
    integer perm(8)
    logical used(8)

    !最終的に作りたいグラフの情報を読み込む
    read (*, *) N
    read (*, *) MG
    do i = 1, MG
        read (*, *) u(i), v(i)
    end do

    !元のグラフの情報を読み込む
    read (*, *) MH
    if (MH > 0) then
        do i = 1, MH
            read (*, *) au(i), av(i)
        end do
    end if

    !コスト行列の読み込み
    !(i-jを繋ぐor消すのにかかるコストをcost(i,j)に入れてる)
    do i = 1, N - 1
        read (*, *) (cost(i, j), j=i + 1, N)
        do j = i + 1, N
            cost(j, i) = cost(i, j)
        end do
    end do

    !グラフの初期化
    !i-jがつながっているならG(i,j)が.true.
    G = .false.; H = .false.
    do i = 1, MG
        G(u(i), v(i)) = .true.
        G(v(i), u(i)) = .true.
    end do
    do i = 1, MH
        H(au(i), av(i)) = .true.
        H(av(i), au(i)) = .true.
    end do

    !頂点の順列を生成して最小コストを計算
    min_cost = 9223372036854775807_8 ! integer(8)の最大値で初期化
    used = .false.
    call permute(1, N, perm, used, min_cost)

    !結果の出力
    write (*, *) min_cost
contains
    recursive subroutine permute(pos, N, perm, used, min_cost)
        ! pos：現在の順列で割り当てられている位置
        ! N：頂点の数
        ! perm：現在生成されている順列
        ! used：頂点がすでに使用されているかどうか
        ! min_cost：エッジ操作の最小コスト
        integer, intent(in) :: pos, N
        integer, intent(inout) :: perm(N)
        logical, intent(inout) :: used(N)
        integer(8), intent(inout) :: min_cost
        integer :: i
        integer(8) :: cost_now
        if (pos > N) then
            call edge_operations(N, perm, min_cost)
        else
            do i = 1, N
                if (.not. used(i)) then
                    used(i) = .true.
                    perm(pos) = i
                    call permute(pos + 1, N, perm, used, min_cost)
                    used(i) = .false.
                end if
            end do
        end if
    end subroutine permute

    subroutine edge_operations(N, perm, min_cost)
        ! N：頂点の数
        ! perm：現在の順列に基づく頂点の対応
        ! min_cost：エッジ操作の最小コスト
        ! G_edge：グラフGのiとjの間にエッジがあるかどうか
        ! H_edge：グラフHのperm(i)とperm(j)の間にエッジがあるかどうか
        integer, intent(in) :: N
        integer, intent(in) :: perm(N)
        integer(8), intent(inout) :: min_cost
        integer :: i, j
        integer(8) :: cost_now
        logical :: G_edge, H_edge
        cost_now = 0_8
        do i = 1, N - 1
            do j = i + 1, N
                G_edge = G(i, j)
                H_edge = H(perm(i), perm(j))
                if (G_edge .NEQV. H_edge) then
                    cost_now = cost_now + cost(perm(i), perm(j))
                    if (cost_now >= min_cost) return ! 枝刈り
                end if
            end do
        end do
        if (cost_now < min_cost) then
            min_cost = cost_now
        end if
    end subroutine edge_operations
end program abc371c

