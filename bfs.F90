module grafo

!implicit none

type, public :: vertice
    integer, private ::  num =-5 
    !integer ::  num =-5 
    integer ::  numViz=-1 
  contains
    procedure :: acessarNum, atribuirNum
    procedure :: acessarPeso, atribuirPeso
    !procedure :: mostrarConteudoV
end type vertice

type, public :: verticeL
    type(vertice)           :: V
    type(vertice),  pointer :: pV   => null()
    type(verticeL), pointer :: next => null()
    type(verticeL), pointer :: pai  => null()
  contains
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

integer pure function acessarNum(this_)
    class (vertice), intent(in) :: this_
    acessarNum = this_%num
end function acessarNum

integer pure function acessarPeso(this_)
    class (vertice), intent(in) :: this_
    acessarPeso = this_%numViz
end function acessarPeso

subroutine atribuirNum(this_, num_)
    class(vertice), intent(inout) :: this_
    integer, intent(in) :: num_
    this_%num=num_
 !print*, "em subroutine atribuirNum, this_%num =", this_%num
end subroutine atribuirNum

subroutine atribuirPeso(this_, numViz_)
    class(vertice), intent(inout) :: this_
    integer, intent(in) :: numViz_
    this_%numViz=numViz_
end subroutine atribuirPeso

integer function numVizinhos(lV_)
 type(verticeL), pointer :: lV_

 type(verticeL), pointer :: aux
 integer :: c 
 c = 0
 if (.not.associated(lV_)) then
    write (*,*) ", lista vazia ..."
 else
   aux=>lV_
   do while(associated(aux))!%next))
     aux=>aux%next
     c=c+1
   end do
endif
   numVizinhos=c
end function numVizinhos

subroutine mostrarConteudoV(this_)
        type(vertice), intent(in) :: this_
    write(*,'(" ::", i0,":", i0,", ")',advance='No') acessarNum(this_)!,  acessarPeso(this_)
    !write(*,'(a, i0,", ")',advance='No')"num=", acessarNum(this_)
    !write(*,'(a, i0, ",  ")',advance='No')"numViz=", acessarPeso(this_)
end subroutine mostrarConteudoV

subroutine mostrarConteudoL(lV_)
 type(verticeL), pointer :: lV_
 type(verticeL), pointer :: aux
 integer :: c 

 if (.not.associated(lV_)) then
   write (*,*) ", lista vaziaAAAAAAA ..."
   return
 end if
 c = mostrarConteudoLD(lV_)
 write(*,'(a,i5,a)',advance='yes') ',', c, ' elementos'
 return
end subroutine mostrarConteudoL

integer recursive function mostrarConteudoLR(this_) result(c)
 !integer :: mostrarConteudoLR
 type(verticeL), pointer :: this_
 if (.not.associated(this_)) then
    c = 0
   ! write (*,*) ", lista vazia ..."
 else
 c=mostrarConteudoLR(this_%next)+1
 call mostrarConteudoV(this_%pV)
endif
    return
end function mostrarConteudoLR

integer recursive function mostrarConteudoLD(this_) result(c)
 !integer :: mostrarConteudoLR
 type(verticeL), pointer :: this_
 if (.not.associated(this_)) then
   c=0
   print*
   !write (*,*) ", lista vazia ..."
 else
   call mostrarConteudoV(this_%pV)
   c=mostrarConteudoLD(this_%next)+1
 endif
 return
end function mostrarConteudoLD

subroutine mostrarConteudoGp(adjArray_, inicio_, fim_)
type(pVerticeL), intent(in) :: adjArray_(:)
 integer, intent(in) ::  inicio_, fim_

 integer :: eq, eqBandaMax
 print*; print*
 print*, "em  mostrarConteudoGp, total de vertices: " , size(adjArray_)
 print*, "adjacencias nodais, entre os vertices  = ", inicio_ , " a ", fim_
 do eq =  inicio_, fim_
 write(*,'(a, i0, ", ")', advance='no')  'adjs a ', eq; call mostrarConteudoL(adjArray_(eq)%p )
  write(*,'(a, i0)', advance='yes') 'maior-menor+1= ', maiorValor(adjArray_(eq)%p)-menorValor(adjArray_(eq)%p)+1
 end do
  return
end subroutine mostrarConteudoGp

integer function maiorValor(lV_)
 type(verticeL), intent(in),  pointer :: lV_
 type(verticeL), pointer :: aux
 aux=>lV_
   maiorValor=acessarNum(aux%pV) 
   do while(associated(aux))
     if(acessarNum(aux%pV)>maiorValor) maiorValor=acessarNum(aux%pV)
     aux=>aux%next
   end do
end function maiorValor

integer function menorValor(lV_)
 type(verticeL), intent(in),  pointer :: lV_
 type(verticeL), pointer :: aux
 aux=>lV_
   menorValor=acessarNum(aux%pV) 
   do while(associated(aux))
     if(acessarNum(aux%pV)<menorValor) menorValor=acessarNum(aux%pV)
     aux=>aux%next
   end do
end function menorValor

integer function bandaMax(adjArray_)
 type(pVerticeL), intent(in) :: adjArray_(:)
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
   integer ::  i, n 

   print*," em bfs, origem_,", origem_, ", destino_ ", destino_

   allocate(included(neq_)); included=.false.

   allocate(inicio); 
   inicio%pV=>listaVertices(origem_);
   inicio%next=>null()
   
   !DB_24 write(*,'(a,i5)',advance='yes')' incluir em toVisit +++ ,'; call mostrarConteudoV(inicio%pV); print*
   !BD24 write(*,'(a,i5)',advance='yes')'     incluir em toVisit +++ ,'
   call incluirVerticeL(toVisit, inicio%pV); 
   write(*,'(a)', advance='no') "nohs toVisit, "; call mostrarConteudoL(toVisit); print*
   current=>excluirVerticeMaisAntigo(toVisit)
   write(*,'(a)', advance='no') "noh excluido, "; call mostrarConteudoV(current%pV); 
   i=1
   do while(associated(current)) ! .and. i<25)
      adj => adjArray_(acessarNum(current%pV))%p 
      write(*,'(/a,i3)',advance='no')' avaliar vizinhos de ', acessarNum(current%pV); call mostrarConteudoL(adj)
      do while(associated(adj))
         write(*,'(a,i3)',advance='no')' avaliar noh: '; call mostrarConteudoV(adj%pV);
                 print*, "numViz= ",  acessarNum(listaVertices(acessarNum(adj%pV))) 
            call atribuirPeso(adj%pV,listaVertices(acessarNum(adj%pV))%numViz)
         if(.not.included(acessarNum(adj%pV)) .and. acessarNum(adj%pV)/=acessarNum(current%pV)) then
            write(*,'(a,i5)',advance='yes')'     incluir em toVisit +++ ,'; call mostrarConteudoV(adj%pV); 
            print*, "..."
            call mostrarConteudoV(adj%pV)
            call incluirVerticeLTopo(toVisit, adj%pV)
            !included(acessarNum(adj%pV)) = .true.
         else 
            write(*,'(a,i5)',advance='yes') '  NAO incluir em toVisit ' , acessarNum(adj%pV)
         endif
         adj=>adj%next
            print*, "...  ..."
      end do 
      !BD24 print*, "included(",acessarNum(current%pV),")=", included(acessarNum(current%pV))
      if(.not.included(acessarNum(current%pV))) then
             write(*,'(a,i5,a)',advance='yes')"  incluir ", acessarNum(current%pV), " em visitedL +++ "
             call incluirVerticeLTopo(visitedL, current%pV)
      endif
      included(acessarNum(current%pV)) = .true.
      write(*,'(a)', advance='no') "nohs visited, "; call mostrarConteudoL(visitedL); 
      !write(*,'(a)', advance='no') " toVisit +++ "; call mostrarConteudoL(toVisit); print*

      ! não incluir repetidos em toVisit  
      write(*,'(a)', advance='no') " toVisit +++ "; call mostrarConteudoL(toVisit); print*
       
      current=>excluirVerticeMaisAntigo(toVisit)
      do while (associated(current))  
         write(*,'(a,i5)',advance='yes')'     excluirMaisAnt from toVisit +++ ,'
         if(.not.included(acessarNum(current%pV))) exit
         current=>excluirVerticeMaisAntigo(toVisit)
      enddo
      if(associated(current))  call mostrarConteudoV(current%pV); 
      write(*,'(a)', advance='no') " excluido from  toVisit +++ ";
      !!if(i==9) stop
      i=i+1
   end do
   print*, " fim do bfs "
   write(*,'(a)', advance='no') " reversed visited nodes, "; n=mostrarConteudoLD(visitedL); 
   print*, "num elementos = ", n
   write(*,'(a)', advance='no') " direct visited nodes  , "; n=mostrarConteudoLR(visitedL); print*
   print*, "num elementos = ", n
   call mostrarConteudoL(visitedL); 
   bfs => visitedL
end function bfs 

 subroutine montarAdjArrayLM( adjArray_ ,LM_, listaVertices_, numel_, nen_, ndof_, neq_, numMaxVizEq_ )
 type(pVerticeL), intent(inout) :: adjArray_(:)
 integer :: LM_(4,numel_)
 type(vertice), target :: listaVertices_(0:) 
 type(vertice), pointer :: pU, pV 
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
            ! o grafo é direcional 1->2 e 2->1 são diferentes
            pU => listaVertices_(eqB)
            R1=procurarVertice(adjArray_(eq)%p, pU) !listaVertices_(eqB)) 
            if(.not.R1) then
               !BD24 !print*, "A, incluindo= ", listaVertices_(eqB)%num, " em vizinhos de ", eq
               call incluirVerticeLOrdenado(adjArray_(eq)%p, pU) ! listaVertices_(eqB))
            endif 
            pV => listaVertices_(eq)
            R2=procurarVertice(adjArray_(eqB)%p, pV) !listaVertices_(eq)) 
            if(.not.R2) then
               !BD24 print*, "B, incluindo= ", listaVertices_(eq)%num, " em vizinhos de ", eqB
               call incluirVerticeLOrdenado(adjArray_(eqB)%p, pV) ! listaVertices_(eq))
            endif 
          endif
          j=j+1
        end do
      endif
      i=i+1
    end do
  end do 
  do eq = 1,  neq_ 
   call atribuirPeso(listaVertices_(eq), numVizinhos(adjArray_(eq)%p))
   !call mostrarConteudoV(listaVertices_(eq)); print*
 end do
 !call mostrarConteudoGp(adjArray_, 1, neq_)
  !end do
  !DB_24  write(*,'(a,i5,a, 4i5)', advance='yes') "nel=",nel, "-- LM =",LM_(:,nel)
 end subroutine montarAdjArrayLM
! //R=procurarVertice(graph_->adjArray[acessarNum(src_)-1], *v);//==0;
! //if(!R){// return;
! //  printf("A, NAO  ACHEI. SIM INCLUI: %d - %d\n", acessarNum(src_), acessarNum(dest_));
! //  addEdgeDV(graph_, src_ , dest_, numViz);
logical recursive function procurarVertice(h_, v_) result (resp)
    type(VerticeL), pointer :: h_
    type(Vertice)           :: v_
    if(.not.associated(h_)) then
             resp= .false.;
          else
             !print*, acessarNum(h_%pV),"==", acessarNum(v_)
             if(acessarNum(h_%pV)==acessarNum(v_)) then
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
       !call mostrarConteudoV(aux%pV); print*
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
       call mostrarConteudoV(aux%pV); print*
end function excluirVerticeLTopo

function excluirVerticeL(this_)
       type(verticeL), pointer :: excluirVerticeL
       type(verticeL), intent(inout), pointer:: this_
       excluirVerticeL=>excluirVerticeLTopo(this_); return
       excluirVerticeL=>excluirVerticeMaisAntigo(this_); return
end function excluirVerticeL

subroutine incluirVerticeLTopoSemRepeticao(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target,  intent(in)    :: v_

   type(verticeL), pointer  :: novo, aux 
   write(*,'(a)', advance="no") "em function incluirVerticeLTopoSemRepeticao, "
   call mostrarConteudoV(v_); print*
   aux => this_
   do while(associated(aux))
     if(aux%pV%num==v_%num) return
     aux=> aux%next
   end do 

   allocate(novo)
   novo%pV  =>v_ 
   novo%next=>this_;
   this_     =>novo;
   return
end subroutine incluirVerticeLTopoSemRepeticao

subroutine incluirVerticeLTopo(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target,  intent(in)    :: v_

   type(verticeL), pointer  :: novo

   write(*,'(a)', advance="no") "em function incluirVerticeLTopo, "
   call mostrarConteudoV(v_); print*
   allocate(novo)
   novo%pV  =>v_ 
   novo%next=>this_;
   this_    =>novo;
   return
end subroutine incluirVerticeLTopo

subroutine incluirVerticeLOrdenado(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target,  intent(in)    :: v_

   type(verticeL), pointer  :: novo
   type(verticeL), pointer  :: aux!, novo
   integer :: i

   write(*,'(a)', advance="no") "em function incluirVerticeLOrdenado, "
   call mostrarConteudoV(v_); print*
   allocate(novo)
   novo%pV  =>v_
   novo%next=>null()
   if (.not.associated(this_) ) then
      !print*, "lista estava vazia ...."
      novo%next   =>null()
      this_   =>novo;
   elseif (novo%pV%num <= this_%pV%num ) then ! .and. associated(this) ) then
      !print*, " novo menor que o topo"
      novo%next=>this_;
      this_=>novo;
   elseif (.not.associated(this_%next)) then
         ! else( novo%pV%num >= this_%pV%num) then
      !           print*, " lista com 1 elemento e novo MAIOR que o topo"
                  novo%next =>null()
                  this_%next=> novo
   else
      !print*, " lista com 2 ou+ elementos e novo MAIOR que o topo"
      aux=> this_
      !do while (associated(aux%next) .and. novo%pV%num > aux%next%pV%num) 
      ! short-circuit evaluations hasn´t working 
      do while (associated(aux%next))
          if (novo%pV%num <= aux%next%pV%num) exit
          aux => aux%next
      end do
      novo%next=>aux%next
      aux%next => novo
    endif
   return
end subroutine incluirVerticeLOrdenado

subroutine incluirVerticeL0(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target,  intent(in)    :: v_
   call  incluirVerticeLTopo(this_, v_); return
   call  incluirVerticeLTopoSemRepeticao(this_, v_); return
   call  incluirVerticeLOrdenado(this_, v_); return
end subroutine incluirVerticeL0

subroutine incluirVerticeL(this_, v_)
   type(verticeL), pointer, intent(inout) :: this_
   type(vertice),  target, intent(in)    :: v_
   print*, "em subroutine incluirVerticeL(this_, v_)"
   call  incluirVerticeLOrdenado(this_, v_); return
   call  incluirVerticeLTopoSemRepeticao(this_, v_); return
   call  incluirVerticeLTopo(this_, v_); return
   return
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

 !call testesIniciais()
 !return

 NUMNPX=(600+1)*(20+1); NUMELX=600*20
 NUMNPX=(60+1)*(20+1); NUMELX=60*20
 NUMNPX=(6+1)*(6+1); NUMELX=6*6
 numnp=21; numel=12; neq = numnp; 

 print*, " estudos de renumeracao usando Cuthill–McKee algorithm "
 print*, "          em malha de elmentos finitos"
 print*, "numnp=", numnp,", numel=", numel, ", neq =",  neq; 


 allocate(LM(nen*ndof,numel))
 allocate(id(ndof,numnp))
 allocate(listaVertices(0:neq)) ; 

 allocate(adjArrayP(1:neq)); do i = 1, numnp; adjArrayP(i)%p=>null(); end do

 call setId()
 do i = 1, numnp
   call atribuirNum (listaVertices(i), i)
   call atribuirPeso(listaVertices(i), -i*10)
   !call mostrarConteudoV(listaVertices(i)); print*
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
 inicio=1; final=5; if(numnp<=21) final = numnp 
 call mostrarConteudoGp(adjArrayP, inicio, final)

 inicio=numnp-5; final=numnp
 if(numnp>21) call mostrarConteudoGp(adjArrayP, inicio, final)

 eqBandaMaxOriginal=bandaMax(adjArrayP)
 print*, "banda_ original = ", maiorValor(adjArrayP(eqBandaMaxOriginal)%p)-menorValor(adjArrayP(eqBandaMaxOriginal)%p)+1
 print*, "  ------------- BFS  --------------------------------------BFS "
 origem=1; destino=4; !(??? qual o significado de destino para o algoritmo bfs)
 caminho => bfs (adjArrayP, neq, origem, destino)
 stop

 eqBandaMax=bandaMax(adjArrayP)
 print*, "banda_ original = ", maiorValor(adjArrayP(eqBandaMax)%p)-menorValor(adjArrayP(eqBandaMax)%p)+1

 do i =1,1 ! não funciona com repeticoes, não melhora a banda
 call renumerar()

 print*
 eqBandaMax=bandaMax(adjArrayP)
 print*, "banda_ modificada = ", maiorValor(adjArrayP(eqBandaMax)%p)-menorValor(adjArrayP(eqBandaMax)%p)+1
 print*, " conectividades dos elementos da numeração modificada"
 inicio=1; final=5
 if(numel<=12) final=numel
 call mostrarLM(LM, nen, numel, inicio, final)
 inicio=numel-5; final=numel
 if(numel>12) call mostrarLM(LM, nen, numel, inicio, final)
 print*, " adjacencias nodais modificada  "
 inicio=1; final=5
 if(numnp<21) final = numnp 
 call mostrarConteudoGp(adjArrayP, inicio, final)
 inicio=numnp-5; final=numnp
 if(numnp>21) call mostrarConteudoGp(adjArrayP, inicio, final)
 eqBandaMax=bandaMax(adjArrayP)
 print*, "banda_ modificada = ", maiorValor(adjArrayP(eqBandaMax)%p)-menorValor(adjArrayP(eqBandaMax)%p)+1
 call mostrarConteudoL(caminho)
 call mostrarConteudoL_reverso(caminho)
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
 !   print'(i0,a,i0)', eq,' vai para posição ', acessarNum(aux%pV);
    call atribuirNum(novoVL%pV,acessarNum(aux%pV))! =aux%pV%num
    novoVL%pV%numViz=aux%pV%numViz
    nullify(novoVL%next)
    call incluirVerticeL(caminhoC, novoVL%pV)
    eq = eq-1
    aux=>aux%next
 end do; 

 print*, "copia do caminho minimo"
 call mostrarConteudoL(caminhoC); 

 aux=>caminhoC
 do i=1,neq;
     call atribuirNum(listaVertices(acessarNum(aux%pV)),i)
     aux=>aux%next; 
 end do;

 if(neq<=210) print*, "nova numeracao"
 if(neq<=210) then; do i=1,neq; write(*,'(i3)', advance='no')  acessarNum(listaVertices(i)); end do; endif
         print*
 if(neq<=210) then; do i=1,neq; print*,i, "->", acessarNum(listaVertices(i)); end do; endif

 do nel = 1, numel
    do dof=1, ndof*nen
    if(lm(dof,nel) > 0)  lm(dof,nel) = acessarNum(listaVertices(lm(dof,nel)))
    end do
 end do
 print*, "novo LM "; call mostrarLM(LM, nen, numel, 1, numel);
 do i = 1, numnp; call atribuirNum(listaVertices(i),i); end do; 
 do i = 1, numnp; adjArrayP(i)%p=>null(); end do; 
 call montarAdjArrayLM(adjArrayP, LM, listaVertices, numel, nen, ndof,  neq, numMaxVizEq)

 eqBandaMax=bandaMax(adjArrayP)
 print*, "banda_ modificada    = ", maiorValor(adjArrayP(eqBandaMax)%p)-menorValor(adjArrayP(eqBandaMax)%p)+1
 
 call mostrarConteudoL(caminho); 
 call mostrarConteudoL_reverso(caminho); 

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

subroutine testesIniciais
        use grafo
 type(vertice), pointer ::vA 
 type(verticeL), pointer :: toVisit , aux
 type(verticeL), pointer :: toVisitC
 type(verticeL), pointer :: toVisitD 
 type(vertice), pointer :: listaV (:)
 integer :: i, nA=10, incA

  allocate(listaV(20))

  toVisit =>null()
  toVisitC=>null()
  toVisitD=>null()

  allocate(vA)

  nA=10; incA=-4;i =1
  call inserirVarios()
  print*, "                            ......................"
  call mostrarConteudoL(toVisitC); 

  nA=22;incA=+3;i = 5
  call inserirVarios()
  print*, "                            ......................"
  call mostrarConteudoL(toVisitD); 

  nA=5; incA=3; i = 10
  call mostrarConteudoL(toVisitC); 
  call inserirVarios()

  i = 5
  do while(associated(toVisit%next))
  aux = excluirVerticeL(toVisit)
  call mostrarConteudoL(toVisit); 
  toVisit=> toVisit%next
  i=i-1
  end do

contains 

subroutine inserirVarios

  integer :: k 
  k = 1 

  call atribuirNum(vA,nA); 
  listaV(i)=vA;
  do while(k<5) 
!  print*, "incluindo ......... V%num=", acessarNum(listaV(i)); print*
  call incluirVerticeL(toVisit, listaV(i)); 
  call mostrarConteudoL(toVisit); 
  i=i+1; k=k+1
  nA = nA + incA
  call atribuirNum(vA,nA); 
  listaV(i)=vA;
  end do

end subroutine inserirVarios

end subroutine testesIniciais
