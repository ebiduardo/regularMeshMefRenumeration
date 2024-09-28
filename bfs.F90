module grafo

!implicit none

type, public :: vertice
    integer num, peso 
  contains
    procedure :: acessarNum 
    procedure :: atribuirNum
    procedure :: mostrarConteudoV
end type vertice

type, public :: verticeL
    type(vertice)           :: V!    => null()
    type(verticeL), pointer :: next => null()
    type(verticeL), pointer :: pai  => null()
  contains
    !procedure :: mostrarConteudoLV
    !procedure :: incluirVerticeL
   !procedure excluirVerticeTopo
end type verticeL

type, public :: pVerticeL
    type(verticeL), pointer :: p
end type pVerticeL

   type(verticeL), allocatable :: adjArray(:)
   type(pVerticeL), allocatable :: adjArrayP(:)
   type(verticeL), allocatable :: adjArrayC(:)
   logical, allocatable :: visited(:)
   type(vertice), allocatable, target :: listaVertices(:) 

contains

integer pure function acessarNum(this)
    class (vertice), intent(in) :: this
    acessarNum = this%num
end function acessarNum

subroutine atribuirNum(this,num_)
    class(vertice), intent(out) :: this
    integer, intent(in) :: num_
    this%num=num_
end subroutine atribuirNum

subroutine mostrarConteudoV(this)
    class(vertice), intent(in) :: this
    write(*,'(i0,", ")',advance='No') acessarNum(this)
end subroutine mostrarConteudoV

subroutine mostrarConteudoLV(this)
    class(VerticeL), pointer, intent(in) :: this

    if(.not. associated(this)) return
    call  mostrarConteudoV(this%V)
    write(*,'(L, ", ")',advance='No') associated(this%next)
end subroutine mostrarConteudoLV

subroutine mostrarConteudoL(lV_)
 type(verticeL), pointer :: lV_

 type(verticeL), pointer :: aux
 integer :: c 
 write (*,'(a)', advance='no') ", em mostrarConteudoL"
 c = 0
 if (.not.associated(lV_)) then
    write (*,*) ", lista vazia ..."
 else
 write(*,'(a)',advance='no') ': ';
 aux=>lV_
   do while(associated(aux))!%next))
     call mostrarConteudoLV(aux)
     aux=>aux%next
     c=c+1
   end do
 print*, '...';
endif
    write(*,'(a,i5,a)',advance='yes') ',', c, ' elementos'
    return
end subroutine mostrarConteudoL

subroutine mostrarConteudoGp(adjArray_, inicio_, fim_)
type(pVerticeL), intent(in) :: adjArray_(:)
 type(vertice), pointer :: pointerV
 type(verticeL), pointer :: auxpV
 integer ::  inicio_, fim_
 integer :: eq, eqBandaMax, col
 print*; print*
 print*, "em  mostrarConteudoGp, total de vertices: " , size(adjArray_)
 print*, "adjacencias nodais, entre os vertices  = ", inicio_ , " a ", fim_
 do eq =  inicio_, fim_
  write(*,'(a, i0, ", ")', advance='no')  'adjs a ', eq; call mostrarConteudoL(adjArray_(eq)%p )
  write(*,'(a, i0, ", ")', advance='yes') 'maior-menor+1= ', maiorValor(adjArray_(eq)%p)-menorValor(adjArray_(eq)%p)+1
 end do
  return
end subroutine mostrarConteudoGp

integer function maiorValor(lV_)
 type(verticeL), pointer :: lV_
 type(verticeL), pointer :: aux
 aux=>lV_
   maiorValor=acessarNum(aux%V) 
   do while(associated(aux))
     if(acessarNum(aux%V)>maiorValor) maiorValor=acessarNum(aux%V)
     aux=>aux%next
   end do
end function maiorValor

integer function menorValor(lV_)
 type(verticeL), pointer :: lV_
 type(verticeL), pointer :: aux
 aux=>lV_
   menorValor=acessarNum(aux%V) 
   do while(associated(aux))
     if(acessarNum(aux%V)<menorValor) menorValor=acessarNum(aux%V)
     aux=>aux%next
   end do
end function menorValor

integer function bandaMax(adjArray_)
 type(pVerticeL),  intent(in) :: adjArray_(:)
 integer :: eq, banda,  eqBandaMax
 eq=1; eqBandaMax=eq 
 bandaMax=maiorValor(adjArray_(eq)%p)-menorValor(adjArray_(eq)%p)+1
 do eq = 2, size(adjArray_)
   banda=maiorValor(adjArray_(eq)%p)-menorValor(adjArray_(eq)%p)+1
   if(banda>bandaMax) then
     eqBandaMax=eq; 
     bandaMax=banda;
   endif
 end do
 bandaMax=eqBandaMax
end function bandaMax

function bfs (adjArray_, neq_, origem_, destino_)
   type(verticeL),  pointer    :: bfs 
   type(pVerticeL), intent(in) :: adjArray_(:)
   integer,         intent(in) :: neq_, origem_, destino_ 

   type(verticeL), pointer :: current, adj, inicio
   type(verticeL), pointer :: toVisit=> null() , visitedL=>null()
   logical,    allocatable :: included(:)
   integer ::  i 

   print*," em bfs, origem_,", origem_, ", destino_ ", destino_

   allocate(visited(neq_));  visited =.false.
   allocate(included(neq_)); included=.false.

   allocate(inicio); inicio%V%num=origem_; inicio%next=>null()
   
    !DB_24 write(*,'(a,i5)',advance='yes')' incluir em toVisit +++ ,'; call mostrarConteudoLV(inicio); print*
   !BD24 write(*,'(a,i5)',advance='yes')'     incluir em toVisit +++ ,'
   call incluirVerticeL(toVisit, inicio%V); 
   write(*,'(a)', advance='no') "nohs toVisit, "; call mostrarConteudoL(toVisit); print*
   current=>excluirVerticeMaisAntigo(toVisit)
   !BD24 write(*,'(a)', advance='no') "noh excluido, "; call mostrarConteudoLV(current); 
   i=1
   do while(associated(current)) ! .and. i<25)
      adj => adjArray_(acessarNum(current%V))%p 
      write(*,'(/a,i3)',advance='no')' avaliar vizinhos de ', acessarNum(current%V); call mostrarConteudoL(adj)
      do while(associated(adj))
         write(*,'(a,i3)',advance='no')' avaliar noh: '; call mostrarConteudoLV(adj);
         if(.not.included(acessarNum(adj%V)) .and. acessarNum(adj%V)/=acessarNum(current%V)) then
            write(*,'(a,i5)',advance='yes')'     incluir em toVisit +++ ,'; call mostrarConteudoV(adj%V); 
            call incluirVerticeLTopo(toVisit, adj%V)
         else 
            !BD24 write(*,'(a,i5)',advance='yes')' NAO incluir em toVisit++ ,'; 
         endif
         adj=>adj%next
      end do 
      !BD24 print*, "included(",acessarNum(current%V),")=", included(acessarNum(current%V))
      if(.not.included(acessarNum(current%V))) then
            !BD24 write(*,'(a,i5)',advance='yes')'     incluir em visitedL +++ ,'
             call incluirVerticeLTopo(visitedL, current%V)
      endif
      included(acessarNum(current%V)) = .true.
      write(*,'(a)', advance='no') "nohs visited, "; call mostrarConteudoL(visitedL); 
      write(*,'(a)', advance='no') " toVisit +++ "; call mostrarConteudoL(toVisit); print*

      ! não incluir repetidos em toVisit  
      do while (included(acessarNum(current%V)))
      write(*,'(a,i5)',advance='yes')'     excluirMaisAnt from toVisit +++ ,'
         current=>excluirVerticeMaisAntigo(toVisit)
         if(.not.associated(current)) exit  
      enddo
      write(*,'(a)', advance='no') " toVisit +++ "; call mostrarConteudoL(toVisit); print*
      !!if(i==9) stop
      i=i+1
   end do
   print*, " fim do bfs "
   write(*,'(a)', advance='no') "nohs visited, "; call mostrarConteudoL(visitedL); 
   bfs => visitedL
   stop
end function bfs 

 subroutine montarAdjArray(  adjArray_ ,LMstencilEq_, listaVertices_, neq_, numMaxVizEq_ )
 type(verticeL), intent(in) :: adjArray_(:)
 integer :: LMstencilEq_(neq_,numMaxVizEq_)
 type(vertice) :: listaVertices_(0:) 
 integer, intent(in) :: neq_, numMaxVizEq_
 integer :: eq, i
  do eq = 1, neq_
    allocate(adjArray(eq)%next)
    adjArray(eq)%next=>null()
    i=1
    do while(i<=numMaxVizEq_)
      if(LMstencilEq_(eq,i)>0) then
         call incluirVerticeL(adjArray(eq)%next, listaVertices_(LMstencilEq_(eq,i)))
      endif
      i=i+1
    end do
  end do 
 end subroutine montarAdjArray

 subroutine montarAdjArrayLM( adjArray_ ,LM_, listaVertices_, numel_, nen_, ndof_, neq_, numMaxVizEq_ )
 type(pVerticeL), intent(inout) :: adjArray_(:)
 integer :: LM_(4,numel_)
 type(vertice) :: listaVertices_(0:) 
 integer, intent(in) :: numel_, nen_, ndof_, neq_, numMaxVizEq_
 integer :: eq, i, j,  nel, eqB
 logical :: R1, R2

 print*, "em montarAdjArrayLM( subroutine  "
  do eq = 1, neq_
  adjArray_(eq)%p=>null()
  end do 

  do nel = 1, numel_
  !DB_24  write(*,'(a,i5,a, 4i5)', advance='yes') "nel=",nel, "-- LM =",LM_(:,nel)
    i=1
    do while(i<=ndof_*nen_)
      eq=LM_(i,nel)
      if(eq>0) then
      !BD24 print*, "eq= ",eq
        j=i
       !DB_24  write(*,'(3(a,i5,a))',advance='no') "vizinhos de :",eq;call mostrarConteudoL(adjArray_(eq)%p)
       !DB_24  write(*,*) "elemento:", nel ,",  analise para inclusao de :", LM_(:,nel)
        do while(j<=ndof_*nen_)
          eqB=LM_(j,nel)
          if(eqB>0) then
            ! o grafo é nao direcional 1->2 e 2->1 são diferentes
            R1=procurarVertice(adjArray_(eq)%p, listaVertices_(eqB)) 
            if(.not.R1) then
               !BD24 !print*, "A, incluindo= ", listaVertices_(eqB)%num, " em vizinhos de ", eq
               call incluirVerticeLOrdenado(adjArray_(eq)%p, listaVertices_(eqB))
            endif 
            R2=procurarVertice(adjArray_(eqB)%p, listaVertices_(eq)) 
            if(.not.R2) then
               !BD24 print*, "B, incluindo= ", listaVertices_(eq)%num, " em vizinhos de ", eqB
               call incluirVerticeLOrdenado(adjArray_(eqB)%p, listaVertices_(eq))
            endif 
          endif
          if(.not.R1) then
        !DB_24  write(*,'(3(a,i5,a))', advance='no') "apos inclusão, vizinhos de :", eq !, ", i=", i, ", j=",j
        !DB_24  call mostrarConteudoL(adjArray_(eq)%p)
          endif 
          j=j+1
        end do
      endif
      i=i+1
    end do
         !2024 eq=LM_(1,nel); write(*,'(/(a,i0,a))', advance='no')  "+++ lista de eq=", eq, ";;;";
         !2024 call mostrarConteudoL(adjArray(eq)%next)
         !2024 eq=LM_(2,nel); write(*,'(/(a,i0,a))', advance='no')  "+++ lista de eq=", eq, ";;;"
         !2024 call mostrarConteudoL(adjArray(eq)%next)
         !2024 eq=LM_(3,nel); write(*,'(/(a,i0,a))', advance='no')  "+++ lista de eq=", eq, ";;;"
         !2024 call mostrarConteudoL(adjArray(eq)%next)
         !2024 eq=LM_(4,nel); write(*,'(/(a,i0,a))', advance='no')  "+++ lista de eq=", eq, ";;;"
         !2024 call mostrarConteudoL(adjArray(eq)%next)
  end do 
 end subroutine montarAdjArrayLM
! //R=procurarVertice(graph_->adjArray[acessarNum(src_)-1], *v);//==0;
! //if(!R){// return;
! //  printf("A, NAO  ACHEI. SIM INCLUI: %d - %d\n", acessarNum(src_), acessarNum(dest_));
! //  addEdgeDV(graph_, src_ , dest_, peso);
logical recursive function procurarVertice(h_, v_) result (resp)
    type(VerticeL), pointer :: h_
    type(Vertice)           :: v_
    if(.not.associated(h_)) then
             resp= .false.;
          else
             if(acessarNum(h_%V)==acessarNum(v_)) then
                resp=.true.
            else 
                resp=procurarVertice(h_%next, v_)
            endif
        endif
end function procurarVertice

function excluirVerticeMaisAntigo(this_)
       type(verticeL), pointer :: excluirVerticeMaisAntigo
       type(verticeL), intent(inout), pointer:: this_

       type(verticeL), pointer :: ant, aux


       if(.not.associated(this_)) then
          print*, " ... lista vazia  !!! ! "
          aux=>null();
       else  if(.not.associated(this_%next)) then
          aux=>this_
          this_=>this_%next
       else
          aux=>this_
          do while(associated(aux%next))  
            ant=>aux
            aux=>aux%next;
          end do 
          aux=>ant%next
          ant%next=>null()
       endif

       !BD write(*,'(a)', advance="no")"  this_="; call mostrarConteudoL(this_); print*
       write(*,'(a)', advance="no") "em function excluirVerticeMaisAntigo, "
       if(.not.associated(aux)) print*, " final da lista "
       !call mostrarConteudoLV(aux); print*
       excluirVerticeMaisAntigo=>aux;
       return
end function  excluirVerticeMaisAntigo

function excluirVerticeLTopo(this_)
       type(verticeL), pointer :: excluirVerticeLTopo
       type(verticeL), intent(inout), pointer:: this_

       type(verticeL), pointer :: aux
       if(.not.associated(this_)) then
            print*, " ... lista vazia! "
            aux=>null();
       else
            aux  => this_
            this_=>this_%next
       end if
       excluirVerticeLTopo=>aux;
       write(*,'(a)', advance="no") "em function excluirVerticeLTopo, "
       call mostrarConteudoLV(aux); print*
end function excluirVerticeLTopo


subroutine incluirVerticeLTopoSemRepeticao(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target,  intent(in)    :: v_

   type(verticeL), pointer  :: novo, aux 
   write(*,'(a)', advance="no") "em function incluirVerticeLTopoSemRepeticao, "
   call mostrarConteudoV(v_); print*


   aux => this_

   do while(associated(aux))
     if(aux%V%num==v_%num) return
     aux=> aux%next
   end do 

   allocate(novo)
   novo%V   =v_ 
   novo%next=>this_;
   this_     =>novo;

   return


end subroutine incluirVerticeLTopoSemRepeticao

subroutine incluirVerticeLTopo(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target,  intent(in)    :: v_

   type(verticeL), pointer  :: novo
   call incluirVerticeLTopoSemRepeticao(this_, v_)
   return
   write(*,'(a)', advance="no") "em function incluirVerticeLTopo, "
   call mostrarConteudoV(v_); print*

   allocate(novo)
   novo%V   =v_ 
   novo%next=>this_;
   this_     =>novo;
   return
end subroutine incluirVerticeLTopo

subroutine incluirVerticeLOrdenado(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target,  intent(in)    :: v_

   type(verticeL), pointer  :: novo, aux
   integer :: i

   write(*,'(a)', advance="no") "em function incluirVerticeLOrdenado, "
   call mostrarConteudoV(v_); print*
   !call mostrarConteudoL(this_); 
   allocate(novo)
   novo%V%num   =v_%num
   if (.not.associated(this_) ) then
      print*, "lista estava vazia ...."
      novo%next   =>null()
      this_   =>novo;
   elseif (novo%V%num < this_%V%num ) then ! .and. associated(this) ) then
      !print*, " novo menor que o topo"
      novo%next=>this_;
      this_=>novo;
   elseif (.not.associated(this_%next) .and. novo%V%num > this_%V%num) then
      !print*, " lista com 1 elemento e novo MAIOR que o topo"
      novo%next =>null()
      this_%next=> novo
   else
      !print*, " lista com 2ou+ elementos e novo MAIOR que o topo"
      aux=> this_
      !do while (associated(aux%next) .and. novo%V%num > aux%next%V%num) 
      ! short-circuit evaluations hasn´t working 
      do while (associated(aux%next))
          if (novo%V%num < aux%next%V%num) exit
          aux => aux%next
      end do
      novo%next=>aux%next
      aux%next => novo
    endif
   return
end subroutine incluirVerticeLOrdenado

function excluirVerticeL(this_)
       type(verticeL), pointer :: excluirVerticeL
       type(verticeL), intent(inout), pointer:: this_
       excluirVerticeL=>excluirVerticeLTopo(this_); return
  !     excluirVerticeL=>excluirVerticeMaisAntigo(this_); return
end function excluirVerticeL

subroutine incluirVerticeL(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target,  intent(in)    :: v_
   call  incluirVerticeLTopo(this_, v_); return
   !call  incluirVerticeLOrdenado(this_, v_); return
end subroutine incluirVerticeL

end module grafo

program renum
        call renumerarCM
end program renum
subroutine renumerarCM()
 use grafo
 implicit none

 integer ::numnp=21, numel=12, neq = 15, numMaxVizEq=9, ndof=1, nen=4

 integer, allocatable :: LMstencilEq(:,:)
 integer, allocatable :: LM(:,:)
 integer, allocatable :: id(:,:)
 integer, allocatable :: numNova(:)
 integer :: eq, i, n, origem, destino 
 integer :: NUMNPX, NUMELX, eqBandaMaxOriginal, eqBandaMax

 !https://stackoverflow.com/questions/8900336/arrays-of-pointers
 !does not define an array of pointers, as you might think, but a pointer to an array.
 !type(verticeL), pointer :: adjArray(:)

 type(verticeL), pointer :: caminho, aux, v1, v2, v3 , v4
 type(vertice) ::vA 
 type(verticeL), pointer :: toVisit , current, adj
 !type(verticeL), external :: excluirVerticeTopo

  integer :: inicio, final

  toVisit=>null()

  vA%num=10; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=21; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=42; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=10; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=52; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=15; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=05; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=65; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=7; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=13; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 
  vA%num=13; print*, "incluindo ......... V%num=", vA%num
  call incluirVerticeL(toVisit, vA); 
  call mostrarConteudoL(toVisit); 

  toVisit=>null()

 NUMNPX=(600+1)*(20+1); NUMELX=600*20
 NUMNPX=(60+1)*(20+1); NUMELX=60*20
 NUMNPX=(6+1)*(2+1); NUMELX=6*2
 numnp=NUMNPX; numel=NUMELX; neq = numnp; 

 print*, " estudos de renumeracao usando Cuthill–McKee algorithm "
 print*, "          em malha de elmentos finitos"
 print*, "numnp=", numnp,", numel=", numel, ", neq =",  neq; 

 allocate(LM(nen*ndof,numel))
 allocate(id(ndof,numnp))
 allocate(numNova(neq))
 allocate(listaVertices(0:neq)) ; 
 allocate(adjArray(1:neq)); !adjArray=>null()
 allocate(adjArrayP(1:neq)); !adjArray=>null()

 do i = 1, numnp
 adjArrayP(i)%p=>null()
 end do

 call setId()
 do i = 1, numnp
   call atribuirNum(listaVertices(id(1,i)),id(1,i))
 end do
 do i = 1, 5;       call mostrarConteudoV(listaVertices(i)); end do; print*
 do i = neq-5, neq; call mostrarConteudoV(listaVertices(i)); end do; print*
! allocate(LMstencilEq(neq,numMaxVizEq))
! call setLMstencil();! call montarAdjArray(adjArray, LMstencilEq, listaVertices, neq, numMaxVizEq)
 call setLM();
 print*, " conectividades dos elementos da malha original"
 inicio=1; final=5
 if(numel<=12) final=numel
 call mostrarLM(LM, nen, numel, inicio, final)
 inicio=numel-5; final=numel
 if(numel>12) call mostrarLM(LM, nen, numel, inicio, final)
 call montarAdjArrayLM(adjArrayP, LM, listaVertices, numel, nen, ndof,  neq, numMaxVizEq)

 print*, " adjacencias nodais original  "
 inicio=1; final=numnp
 if(numnp<21) final = numnp 
 call mostrarConteudoGp(adjArrayP, inicio, final)
 inicio=numnp-5; final=numnp
 if(numnp>21) call mostrarConteudoGp(adjArrayP, inicio, final)

 eqBandaMaxOriginal=bandaMax(adjArrayP)
 print*, "banda_ original = ", maiorValor(adjArrayP(eqBandaMaxOriginal)%p)-menorValor(adjArrayP(eqBandaMaxOriginal)%p)+1
 print*, "  ------------- BFS  --------------------------------------BFS "
 origem=1; destino=4; !(??? qual o significado de destino para o algoritmo bfs)
 caminho => bfs (adjArrayP, neq, origem, destino)

 eqBandaMax=bandaMax(adjArrayP)
 print*, "banda_ original = ", maiorValor(adjArrayP(eqBandaMax)%p)-menorValor(adjArrayP(eqBandaMax)%p)+1

 do i =1,1 ! não funciona com repeticoes, não melhora a banda
 call renumerar()
 cycle
 print*, " conectividades dos elementos da numeração modificada"
 inicio=1; final=5
 if(numel<=12) final=numel
 call mostrarLM(LM, nen, numel, inicio, final)
 inicio=numel-5; final=numel
 if(numel>12) call mostrarLM(LM, nen, numel, inicio, final)
 print*, " adjacencias nodais modificada  "
 inicio=1; final=numnp
 if(numnp<21) final = numnp 
 call mostrarConteudoGp(adjArrayP, inicio, final)
 inicio=numnp-5; final=numnp
 if(numnp>21) call mostrarConteudoGp(adjArrayP, inicio, final)
 eqBandaMax=bandaMax(adjArrayP)
 print*, "banda_ modificada = ", maiorValor(adjArrayP(eqBandaMax)%p)-menorValor(adjArrayP(eqBandaMax)%p)+1
 end do

 return
contains 

subroutine renumerar()
 type(verticeL), pointer :: caminhoC
 integer :: i, dof, eq, nel
 type(verticeL) :: adjV
 type(verticeL), pointer ::  novoVL, aux

 print*, "         caminho minimo"
 call mostrarConteudoL(caminho); 
 eq = neq
 nullify(caminhoC) !=>caminho
 aux=>caminho
 do while (associated(aux)) 
    allocate(novoVL); 
 !   print'(i0,a,i0)', eq,' vai para posição ', acessarNum(aux%V);
    novoVL%V%num=aux%V%num
    nullify(novoVL%next)
    call incluirVerticeL(caminhoC, novoVL%V)
    eq = eq-1
    aux=>aux%next
 end do; 

 print*, "copia do caminho minimo"
 call mostrarConteudoL(caminhoC); 

 aux=>caminhoC
 do i=1,neq;
     listaVertices(acessarNum(aux%V))%num=i
     aux=>aux%next; 
 end do;

 if(neq<=21) print*, "nova numeracao"
 if(neq<=21) then; do i=1,neq; write(*,'(i3)', advance='no')  acessarNum(listaVertices(i)); end do; endif
         print*
 if(neq<=21) then; do i=1,neq; print*,i, "->", acessarNum(listaVertices(i)); end do; endif

 do nel = 1, numel
    do dof=1, ndof*nen
    if(lm(dof,nel) > 0)  lm(dof,nel) = acessarNum(listaVertices(lm(dof,nel)))
    end do
 end do
 print*, "novo LM "; call mostrarLM(LM, nen, numel, 1, 12);
 do i = 1, numnp; listaVertices(i)%num=i; end do; 
 do i = 1, numnp; adjArrayP(i)%p=>null(); end do; 
 call montarAdjArrayLM(adjArrayP, LM, listaVertices, numel, nen, ndof,  neq, numMaxVizEq)

 eqBandaMax=bandaMax(adjArrayP)
 print*, "banda_ modificada    = ", maiorValor(adjArrayP(eqBandaMax)%p)-menorValor(adjArrayP(eqBandaMax)%p)+1
 
 call mostrarConteudoL(caminho); 

 return 

end subroutine renumerar

 subroutine mostrarLM(LM_, nen_, numel_, inicio_, fim_)
 integer :: nen_, numel_, inicio_, fim_
 integer :: LM_(nen_,numel_)
 integer :: nel, nohA, nohB, lin, col
 print*, "em  mostrarLM, total de elementos: " , numel
 print*, "conectividades nodais, entre os elementos = ", inicio_ , " a ", fim_

  do nel = inicio_, fim_
    print '(a,i0,a,4(i0,2x))', "elemento ", nel, ", LM=", LM_(1:4,nel)
  end do

  return
 do nel =  inicio_, fim_ 
   do nohA = 1, nen_
      lin=LM_(nohA,nel) 
     do nohB = 1, nen_
      col=LM_(nohB,nel) 
     print'(a,i0,a,i0,a)', "BR( ",lin," , ",col," )=1;"
  end do
  end do
   print*
  end do
 end subroutine mostrarLM

 subroutine setLM()
 integer :: eq, i, nel

#include "conectividades.F90"

return 
nel= 1; LM(1:ndof*nen,nel) = (/ 1 , 2 , 9 , 8 /)
nel= 2; LM(1:ndof*nen,nel) = (/ 2 , 3 , 10 , 9 /)
nel= 3; LM(1:ndof*nen,nel) = (/ 3 , 4 , 11 , 10 /)
nel= 4; LM(1:ndof*nen,nel) = (/ 4 , 5 , 12 , 11 /)
nel= 5; LM(1:ndof*nen,nel) = (/ 5 , 6 , 13 , 12 /)
nel= 6; LM(1:ndof*nen,nel) = (/ 6 , 7 , 14 , 13 /)
nel= 7; LM(1:ndof*nen,nel) = (/ 8 , 9 , 16 , 15 /)
nel= 8; LM(1:ndof*nen,nel) = (/ 9 , 10 , 17 , 16 /)
nel= 9; LM(1:ndof*nen,nel) = (/ 10 , 11 , 18 , 17 /)
nel= 10; LM(1:ndof*nen,nel) = (/ 11 , 12 , 19 , 18 /)
nel= 11; LM(1:ndof*nen,nel) = (/ 12 , 13 , 20 , 19 /)
nel= 12; LM(1:ndof*nen,nel) = (/ 13 , 14 , 21 , 20 /)
return
nel= 1; LM(1:ndof*nen,nel) = (/ 1 , 2 , 5 , 4 /)
nel= 2; LM(1:ndof*nen,nel) = (/ 2 , 3 , 6 , 5 /)
nel= 3; LM(1:ndof*nen,nel) = (/ 4 , 5 , 8 , 7 /)
nel= 4; LM(1:ndof*nen,nel) = (/ 5 , 6 , 9 , 8 /)
nel= 5; LM(1:ndof*nen,nel) = (/ 7 , 8 , 11 , 10 /)
nel= 6; LM(1:ndof*nen,nel) = (/ 8 , 9 , 12 , 11 /)
nel= 7; LM(1:ndof*nen,nel) = (/ 10 , 11 , 14 , 13 /)
nel= 8; LM(1:ndof*nen,nel) = (/ 11 , 12 , 15 , 14 /)
nel= 9; LM(1:ndof*nen,nel) = (/ 13 , 14 , 17 , 16 /)
nel= 10; LM(1:ndof*nen,nel) = (/ 14 , 15 , 18 , 17 /)
nel= 11; LM(1:ndof*nen,nel) = (/ 16 , 17 , 20 , 19 /)
nel= 12; LM(1:ndof*nen,nel) = (/ 17 , 18 , 21 , 20 /)
do nel = 1, numel
  print*,"nel=",nel, ', LM=',LM(:,nel)
end do 
return
nel=1; LM(1:ndof*nen,nel) = (/0,1,2,0/)
nel=2; LM(1:ndof*nen,nel) = (/1,4,5,2/)
nel=3; LM(1:ndof*nen,nel) = (/4,7,8,5/)
nel=4; LM(1:ndof*nen,nel) = (/7,10,11,8/)
nel=5; LM(1:ndof*nen,nel) = (/10,13,14,11/)
nel=6; LM(1:ndof*nen,nel) = (/13,0,0,14/)
nel=7; LM(1:ndof*nen,nel) = (/0,2,3,0/)
nel=8; LM(1:ndof*nen,nel) = (/2,5,6,3/)
nel=9; LM(1:ndof*nen,nel) = (/5,8,9,6/)
nel=10; LM(1:ndof*nen,nel) = (/8,11,12,9/)
nel=11; LM(1:ndof*nen,nel) = (/11,14,15,12/)
nel=12; LM(1:ndof*nen,nel) = (/14,0,0,15/)
do nel = 1, numel
  print*,"nel=",nel, ', LM=',LM(:,nel)
end do 
return
 nel=1; LM(:,nel) = (/0,1,6,0/)
 nel=2; LM(:,nel) = (/1,2,7,6/)
 nel=3; LM(:,nel) = (/2,3,8,7/)
 nel=4; LM(:,nel) = (/3,4,9,8/)
 nel=5; LM(:,nel) = (/4,5,10,9/)
 nel=6; LM(:,nel) = (/5,0,0,10/)
 nel=7; LM(:,nel) = (/0,6,11,0/)
 nel=8; LM(:,nel) = (/6,7,12,11/)
 nel=9; LM(:,nel) = (/7,8,13,12/)
nel=10; LM(:,nel) = (/8,9,14,13/)
nel=11; LM(:,nel) = (/9,10,15,14/)
nel=12; LM(:,nel) = (/10,0,0,15/)
do nel = 1, numel
  print*,"nel=",nel, ', LM=',LM(:,nel)
end do 
 end subroutine setLM

 subroutine setLMstencil()
 integer :: eq, i, n
LMstencilEq =0
!#include "nosVizinhos"
  do eq = 1, 10
    print *, "LMstencilEq=", LMstencilEq(eq, :)
  end do
  do eq =numnp-10, numnp 
    print *, "LMstencilEq=", LMstencilEq(eq, :)
  end do
return

n= 9 ;eq= 1 ; LMstencilEq(eq, 1:n)=(/ 1 , 2 , 8 , 9 , 0 ,  0 , 0 , 0 , 0  /)
n= 9 ;eq= 2 ; LMstencilEq(eq, 1:n)=(/ 1 , 2 , 3 , 8 , 9 ,  10 , 0 , 0 , 0  /)
n= 9 ;eq= 3 ; LMstencilEq(eq, 1:n)=(/ 2 , 3 , 4 , 9 , 10 ,  11 , 0 , 0 , 0  /)
n= 9 ;eq= 4 ; LMstencilEq(eq, 1:n)=(/ 3 , 4 , 5 , 10 , 11 ,  12 , 0 , 0 , 0  /)
n= 9 ;eq= 5 ; LMstencilEq(eq, 1:n)=(/ 4 , 5 , 6 , 11 , 12 ,  13 , 0 , 0 , 0  /)
n= 9 ;eq= 6 ; LMstencilEq(eq, 1:n)=(/ 5 , 6 , 7 , 12 , 13 ,  14 , 0 , 0 , 0  /)
n= 9 ;eq= 7 ; LMstencilEq(eq, 1:n)=(/ 6 , 7 , 13 , 14 , 0 ,  0 , 0 , 0 , 0  /)
n= 9 ;eq= 8 ; LMstencilEq(eq, 1:n)=(/ 1 , 2 , 8 , 9 , 15 ,  16 , 0 , 0 , 0  /)
n= 9 ;eq= 9 ; LMstencilEq(eq, 1:n)=(/ 1 , 2 , 3 , 8 , 9 ,  10 , 15 , 16 , 17  /)
n= 9 ;eq= 10 ; LMstencilEq(eq, 1:n)=(/ 2 , 3 , 4 , 9 , 10 ,  11 , 16 , 17 , 18  /)
n= 9 ;eq= 11 ; LMstencilEq(eq, 1:n)=(/ 3 , 4 , 5 , 10 , 11 ,  12 , 17 , 18 , 19  /)
n= 9 ;eq= 12 ; LMstencilEq(eq, 1:n)=(/ 4 , 5 , 6 , 11 , 12 ,  13 , 18 , 19 , 20  /)
n= 9 ;eq= 13 ; LMstencilEq(eq, 1:n)=(/ 5 , 6 , 7 , 12 , 13 ,  14 , 19 , 20 , 21  /)
n= 9 ;eq= 14 ; LMstencilEq(eq, 1:n)=(/ 6 , 7 , 13 , 14 , 20 ,  21 , 0 , 0 , 0  /)
n= 9 ;eq= 15 ; LMstencilEq(eq, 1:n)=(/ 8 , 9 , 15 , 16 , 0 ,  0 , 0 , 0 , 0  /)
n= 9 ;eq= 16 ; LMstencilEq(eq, 1:n)=(/ 8 , 9 , 10 , 15 , 16 ,  17 , 0 , 0 , 0  /)
n= 9 ;eq= 17 ; LMstencilEq(eq, 1:n)=(/ 9 , 10 , 11 , 16 , 17 ,  18 , 0 , 0 , 0  /)
n= 9 ;eq= 18 ; LMstencilEq(eq, 1:n)=(/ 10 , 11 , 12 , 17 , 18 ,  19 , 0 , 0 , 0  /)
n= 9 ;eq= 19 ; LMstencilEq(eq, 1:n)=(/ 11 , 12 , 13 , 18 , 19 ,  20 , 0 , 0 , 0  /)
n= 9 ;eq= 20 ; LMstencilEq(eq, 1:n)=(/ 12 , 13 , 14 , 19 , 20 ,  21 , 0 , 0 , 0  /)
n= 9 ;eq= 21 ; LMstencilEq(eq, 1:n)=(/ 13 , 14 , 20 , 21 , 0 ,  0 , 0 , 0 , 0  /)
return

n= 9 ;eq= 1 ; LMstencilEq(eq, 1:n)=(/ 1 , 2 , 4 , 5 , 0 ,  0 , 0 , 0 , 0  /)
n= 9 ;eq= 2 ; LMstencilEq(eq, 1:n)=(/ 1 , 2 , 3 , 4 , 5 ,  6 , 0 , 0 , 0  /)
n= 9 ;eq= 3 ; LMstencilEq(eq, 1:n)=(/ 2 , 3 , 5 , 6 , 0 ,  0 , 0 , 0 , 0  /)
n= 9 ;eq= 4 ; LMstencilEq(eq, 1:n)=(/ 1 , 2 , 4 , 5 , 7 ,  8 , 0 , 0 , 0  /)
n= 9 ;eq= 5 ; LMstencilEq(eq, 1:n)=(/ 1 , 2 , 3 , 4 , 5 ,  6 , 7 , 8 , 9  /)
n= 9 ;eq= 6 ; LMstencilEq(eq, 1:n)=(/ 2 , 3 , 5 , 6 , 8 ,  9 , 0 , 0 , 0  /)
n= 9 ;eq= 7 ; LMstencilEq(eq, 1:n)=(/ 4 , 5 , 7 , 8 , 10 ,  11 , 0 , 0 , 0  /)
n= 9 ;eq= 8 ; LMstencilEq(eq, 1:n)=(/ 4 , 5 , 6 , 7 , 8 ,  9 , 10 , 11 , 12  /)
n= 9 ;eq= 9 ; LMstencilEq(eq, 1:n)=(/ 5 , 6 , 8 , 9 , 11 ,  12 , 0 , 0 , 0  /)
n= 9 ;eq= 10 ; LMstencilEq(eq, 1:n)=(/ 7 , 8 , 10 , 11 , 13 ,  14 , 0 , 0 , 0  /)
n= 9 ;eq= 11 ; LMstencilEq(eq, 1:n)=(/ 7 , 8 , 9 , 10 , 11 ,  12 , 13 , 14 , 15  /)
n= 9 ;eq= 12 ; LMstencilEq(eq, 1:n)=(/ 8 , 9 , 11 , 12 , 14 ,  15 , 0 , 0 , 0  /)
n= 9 ;eq= 13 ; LMstencilEq(eq, 1:n)=(/ 10 , 11 , 13 , 14 , 16 ,  17 , 0 , 0 , 0  /)
n= 9 ;eq= 14 ; LMstencilEq(eq, 1:n)=(/ 10 , 11 , 12 , 13 , 14 ,  15 , 16 , 17 , 18  /)
n= 9 ;eq= 15 ; LMstencilEq(eq, 1:n)=(/ 11 , 12 , 14 , 15 , 17 ,  18 , 0 , 0 , 0  /)
n= 9 ;eq= 16 ; LMstencilEq(eq, 1:n)=(/ 13 , 14 , 16 , 17 , 19 ,  20 , 0 , 0 , 0  /)
n= 9 ;eq= 17 ; LMstencilEq(eq, 1:n)=(/ 13 , 14 , 15 , 16 , 17 ,  18 , 19 , 20 , 21  /)
n= 9 ;eq= 18 ; LMstencilEq(eq, 1:n)=(/ 14 , 15 , 17 , 18 , 20 ,  21 , 0 , 0 , 0  /)
n= 9 ;eq= 19 ; LMstencilEq(eq, 1:n)=(/ 16 , 17 , 19 , 20 , 0 ,  0 , 0 , 0 , 0  /)
n= 9 ;eq= 20 ; LMstencilEq(eq, 1:n)=(/ 16 , 17 , 18 , 19 , 20 ,  21 , 0 , 0 , 0  /)
n= 9 ;eq= 21 ; LMstencilEq(eq, 1:n)=(/ 17 , 18 , 20 , 21 , 0 ,  0 , 0 , 0 , 0  /)
return
n=4; eq=1; LMstencilEq(eq, 1:n) = (/1,2,6,7 /)
print'(a, 30i3)', "LMstencilEq(1, 1:4) =", LMstencilEq(eq, 1:n)
n=6; eq=2; LMstencilEq(eq, 1:n) = (/1,2,3,6,7,8 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=3; LMstencilEq(eq, 1:n) = (/2,3,4,7,8,9 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=4; LMstencilEq(eq, 1:n) = (/3,4,5,8,9,10 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=4; eq=5; LMstencilEq(eq, 1:n) = (/4,5,9,10 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=6; LMstencilEq(eq, 1:n) = (/1,2,6,7,11,12 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=9; eq=7; LMstencilEq(eq, 1:n) = (/1,2,3,6,7,8,11,12,13 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=9; eq=8; LMstencilEq(eq, 1:n) = (/2,3,4,7,8,9,12,13,14 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=9; eq=9; LMstencilEq(eq, 1:n) = (/3,4,5,8,9,10,13,14,15 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=10; LMstencilEq(eq, 1:n) = (/4,5,9,10,14,15 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=4; eq=11; LMstencilEq(eq, 1:n) = (/6,7,11,12 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=12; LMstencilEq(eq, 1:n) = (/6,7,8,11,12,13 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=13; LMstencilEq(eq, 1:n) = (/7,8,9,12,13,14 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=14; LMstencilEq(eq, 1:n) = (/8,9,10,13,14,15 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=4; eq=15; LMstencilEq(eq, 1:n) = (/9,10,14,15 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
end subroutine setLMstencil

 subroutine setLMstencil00()
 integer :: eq, i, n
LMstencilEq =0
n=4; eq=1; LMstencilEq(eq, 1:n) = (/1,2,6,7 /)
print'(a, 30i3)', "LMstencilEq(1, 1:4) =", LMstencilEq(eq, 1:n)
n=6; eq=2; LMstencilEq(eq, 1:n) = (/1,2,3,6,7,8 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=3; LMstencilEq(eq, 1:n) = (/2,3,4,7,8,9 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
!return
n=6; eq=4; LMstencilEq(eq, 1:n) = (/3,4,5,8,9,10 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=4; eq=5; LMstencilEq(eq, 1:n) = (/4,5,9,10 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=6; LMstencilEq(eq, 1:n) = (/1,2,6,7,11,12 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=9; eq=7; LMstencilEq(eq, 1:n) = (/1,2,3,6,7,8,11,12,13 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=9; eq=8; LMstencilEq(eq, 1:n) = (/2,3,4,7,8,9,12,13,14 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=9; eq=9; LMstencilEq(eq, 1:n) = (/3,4,5,8,9,10,13,14,15 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=10; LMstencilEq(eq, 1:n) = (/4,5,9,10,14,15 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=4; eq=11; LMstencilEq(eq, 1:n) = (/6,7,11,12 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=12; LMstencilEq(eq, 1:n) = (/6,7,8,11,12,13 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=13; LMstencilEq(eq, 1:n) = (/7,8,9,12,13,14 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=6; eq=14; LMstencilEq(eq, 1:n) = (/8,9,10,13,14,15 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
n=4; eq=15; LMstencilEq(eq, 1:n) = (/9,10,14,15 /)
print'(i3,a,30i3)',eq,',', LMstencilEq(eq, 1:n)
end subroutine setLMstencil00

subroutine setId()
 integer :: eq, i, n
  do n = 1, numnp
    eq = n
    id(1:ndof, n)=(/eq/)
!    print *, id(:,n)
  end do
 return 
n=1; id(1:ndof, n)=(/0/);
n=2; id(1:ndof, n)=(/1/);
n=3; id(1:ndof, n)=(/2/);
n=4; id(1:ndof, n)=(/3/);
n=5; id(1:ndof, n)=(/4/); 
n=6; id(1:ndof, n)=(/5/);
n=7; id(1:ndof, n)=(/0/); n=8; id(1:ndof, n)=(/0/);
n=9; id(1:ndof, n)=(/6/); n=10; id(1:ndof, n)=(/7/);
n=11; id(1:ndof, n)=(/8/); n=12; id(1:ndof, n)=(/9/);
n=13; id(1:ndof, n)=(/10/); n=14; id(1:ndof, n)=(/0/);
n=15; id(1:ndof, n)=(/0/); n=16; id(1:ndof, n)=(/11/);
n=17; id(1:ndof, n)=(/12/); n=18; id(1:ndof, n)=(/13/);
n=19; id(1:ndof, n)=(/14/); n=20; id(1:ndof, n)=(/15/);
n=21; id(1:ndof, n)=(/0/);
write(*, '(a)', advance='no') "id = " 
write(*, '(100(i0,", "))') id(:,1:numnp)
 end subroutine setId
end subroutine renumerarCM
