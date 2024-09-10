module grafo

implicit none

type, public :: vertice
    integer num, peso 
  contains
    procedure :: acessarNum 
    procedure :: atribuirNum
    procedure :: mostrarConteudoV
end type vertice

type, public :: verticeL
    type(vertice) , pointer :: V    => null()
    type(verticeL), pointer :: next => null()
    type(verticeL), pointer :: pai  => null()
  contains
    procedure :: mostrarConteudoLV
    !procedure :: incluirVerticeL
end type verticeL

   type(verticeL), allocatable :: adjArray(:)
   type(verticeL), allocatable :: adjArrayC(:)
   type(verticeL), pointer :: toVisit=> null() , visitedL=>null()
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
    class(verticeL), intent(in) :: this
    call  mostrarConteudoV(this%V)
end subroutine mostrarConteudoLV

subroutine mostrarConteudoL(lV_)
 type(verticeL), pointer :: lV_

 type(verticeL), pointer :: aux
 write(*,'(a)',advance='no') ': ';
 aux=>lV_
   do while(associated(aux))
     call mostrarConteudoLV(aux)
     aux=>aux%next
   end do
 print*, '...';
end subroutine mostrarConteudoL

subroutine mostrarConteudoG(adjArray_, inicio_, fim_)
 type(verticeL), intent(in) :: adjArray_(:)
 type(vertice), pointer :: pointerV
 integer ::  inicio_, fim_
 integer :: eq, eqBandaMax
 print*; print*
 print*, "em  mostrarConteudoG, total de vertices: " , size(adjArray_)
 print*, "adjacencias nodais, entre os vertices  = ", inicio_ , " a ", fim_
 do eq =  inicio_, fim_ 
  write(*,'(a, i0, ", ")', advance='no') 'adjs a ', eq; call mostrarConteudoL(adjArray_(eq)%next )
  write(*,'(a, i0, ", ")', advance='yes') 'bandaLocal= ', maiorValor(adjArray_(eq)%next)-menorValor(adjArray_(eq)%next)+1
 end do
  eqBandaMax=bandaMax(adjArray_)
 print'(/a,i5,a,i5)', ':::..., banda maxima=', &
       maiorValor(adjArray_(eqBandaMax)%next)-menorValor(adjArray_(eqBandaMax)%next)+1, " em eq ", eqBandaMax
 !if ( eqBandaMax < inicio_ .or. eqBandaMax > fim_)  return
 print*, "vizinhanca da banda maxima"
 do eq = eqBandaMax-0 ,  eqBandaMax+0
  if (eq == eqBandaMax) print*, "equacao da banda maxima"
  write(*,'(a, i0, ", ")', advance='no') 'adjs a ', eq; call mostrarConteudoL(adjArray_(eq)%next )
  write(*,'(a, i0, ", ")', advance='yes') 'bandaLocal= ', maiorValor(adjArray_(eq)%next)-menorValor(adjArray_(eq)%next)+1
  if (eq == eqBandaMax) print*
 end do
end subroutine mostrarConteudoG

integer function maiorValor(lV_)
 type(verticeL), pointer :: lV_
 type(verticeL), pointer :: aux
 integer::maiorN
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
 integer::maiorN
 aux=>lV_
   menorValor=acessarNum(aux%V) 
   do while(associated(aux))
     if(acessarNum(aux%V)<menorValor) menorValor=acessarNum(aux%V)
     aux=>aux%next
   end do
end function menorValor

integer function bandaMax(adjArray_)
 type(verticeL), intent(in) :: adjArray_(:)
 integer :: eq, banda,  eqBandaMax
 eq=1; eqBandaMax=eq 
 bandaMax=maiorValor(adjArray_(eq)%next)-menorValor(adjArray_(eq)%next)+1

! banda=maiorValor(adjArray_(eq)%next)-menorValor(adjArray_(eq)%next)+1
 do eq = 2, size(adjArray_)
  banda=maiorValor(adjArray_(eq)%next)-menorValor(adjArray_(eq)%next)+1
  if(banda>bandaMax) then
     eqBandaMax=eq; !2024 bandaMax=banda
     bandaMax=banda;! maiorValor(adjArray_(eqBandaMax)%next)-menorValor(adjArray_(eqBandaMax)%next)+1
  endif
 end do
     bandaMax=eqBandaMax
end function bandaMax

subroutine incluirVerticeL(this, v_)
   type(verticeL), pointer, intent(inout) :: this
   type(vertice),  target,  intent(in)    :: v_

   type(verticeL), pointer  :: novo
   allocate(novo)
   novo%V   =>v_ 
   novo%next=>this;
   this     =>novo;
   return
end subroutine incluirVerticeL

function excluirVerticeMaisAntigo(c_)
       type(verticeL), pointer :: excluirVerticeMaisAntigo
       type(verticeL), intent(inout), pointer:: c_

       type(verticeL), pointer :: p
       type(verticeL), pointer :: ant

       ! 2024 print*, " em excluirVerticeMaisAntigo(VertList** c_) "

       if(.not.associated(c_)) then
            print*, " ... lista vazia! "
            excluirVerticeMaisAntigo=>null()
            return
       end if
       ant=>c_;
       if(.not.associated(c_%next)) then
           print*, " QUASE vazia, "
           write(*,'(a)',advance='no') "A, excluindo:"; call mostrarConteudoV(c_%V); print*;
           c_=>null()
           excluirVerticeMaisAntigo=>ant
           return
       end if

       p=>c_
       do 
          if(.not.associated(p%next)) exit
          ant=>p
          p=>p%next;
       end do 
       ant%next=>NULL(); 
       excluirVerticeMaisAntigo=>p;
       return
end function  excluirVerticeMaisAntigo

function bfs (adjArray_, neq_, origem_, destino_)
   type(verticeL), pointer :: bfs 
   type(verticeL), intent(in) :: adjArray_(:)
   type(verticeL), pointer :: current, adj, inicio
   integer, intent(in) :: neq_, origem_, destino_ 
   logical, allocatable :: included(:)

   integer ::  i 
   print*," em bfs, origem_,", origem_, ", destino_ ", destino_

   allocate(visited(neq_));  visited =.false.
   allocate(included(neq_)); included=.false.

   toVisit=>null()
   inicio=>adjArray_(origem_)%next
   do i = 1, 3
     inicio=>inicio%next
   end do
   write(*,'(a)'   , advance='no' ) "incluindo :" 
   write(*,'(a,i5)', advance='yes') ',', acessarNum(inicio%V);
   call incluirVerticeL(toVisit, inicio%V) 
   included(acessarNum(inicio%V) )=.true.

   write(*,'(a)',advance='no') '0, toVisit, '; call mostrarConteudoL(toVisit ); print*
   allocate(adj)  
   do while(associated(toVisit))
      current=>excluirVerticeMaisAntigo(toVisit)
      adj => adjArray_(acessarNum(current%V))%next 
      !2024 write(*,'(a)',   advance='no') "adjacents from ="; call mostrarConteudoV(current%V);
      !2024 call mostrarConteudoL(adjArray_(acessarNum(current%V))%next);
      !2024 write(*,'(a)',   advance='no') "incluindo :" 
      do while(associated(adj))
         if(.not.included(acessarNum(adj%V)) .and. acessarNum(adj%V)/=acessarNum(current%V)) then
            !2024 write(*,'(a,i5)',advance='no')',', acessarNum(adj%V); 
            call incluirVerticeL(toVisit, adj%V)
            included(acessarNum(adj%V)) = .true.
         endif
         adj=>adj%next
      end do 
      !2024 print*
      call incluirVerticeL(visitedL, current%V)
      visited(acessarNum(current%V))=.true.
      !2024 print*, "included = ", included
      !2024 print*, "visited  = ", visited
      !2024 write(*,'(a)',advance='no') '2, toVisit : '; call mostrarConteudoL(toVisit );
      !2024 write(*,'(a)',advance='no') '   visitedL: '; call mostrarConteudoL(visitedL );
      deallocate(current)
   end do
      bfs => visitedL
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
 type(verticeL), intent(in) :: adjArray_(:)
 integer :: LM_(4,numel_)
 type(vertice) :: listaVertices_(0:) 
 integer, intent(in) :: numel_, nen_, ndof_, neq_, numMaxVizEq_
 integer :: eq, i, j,  nel, eqB
 logical :: R
   
  do eq = 1, neq_
    allocate(adjArray(eq)%next)
    adjArray(eq)%next=>null()
  end do

  do nel = 1, numel_
    !2024 write(*,'(a,i5,a)', advance='no') "nel=",nel, "--"
    i=1
    do while(i<=ndof_*nen_)
      eq=LM_(i,nel)
      if(eq>0) then
        j=i
         !write(*,'(3(a,i5))', advance='no')  ", i=", i, ", j=",j,  ", eq=", eq
        do while(j<=ndof_*nen_)
          eqB=LM_(j,nel)
          if(eqB>0) then
            !write(*,'(/a,i5)', advance='no') ", eqB=",eqB
            ! o grafo é nao direcional 1->2 e 2->1 são diferentes
            R=procurarVertice(adjArray(eq)%next, listaVertices_(eqB)) 
            if(.not.R) then
               call incluirVerticeL(adjArray(eq)%next, listaVertices_(eqB))
            !   print*,"incluindo ",  acessarNum(adjArray(eq)%next%V)!, acessarNum(listaVertices_(eqB))
            !   print*, '--'
            endif 
            R=procurarVertice(adjArray(eqB)%next, listaVertices_(eq)) 
            if(.not.R) then
               call incluirVerticeL(adjArray(eqB)%next, listaVertices_(eq))
            !   print*,"incluindo ",  acessarNum(adjArray(eqB)%next%V)!, acessarNum(listaVertices_(eq))
            !   print*, '--'
            endif 
          endif
          j=j+1
        end do
      endif
      i=i+1
         !2024 write(*,'(3(a))', advance='no')  "+++"
         !2024    call mostrarConteudoL(adjArray(eqB)%next)
    end do
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

 type(verticeL), pointer :: caminho, aux
  integer :: inicio, final

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
 allocate(listaVertices(0:neq))
 allocate(adjArray(1:neq)); !adjArray=>null()
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

 call montarAdjArrayLM(adjArray, LM, listaVertices, numel, nen, ndof,  neq, numMaxVizEq)

 print*, " adjacencias nodais original  "
 inicio=1; final=numnp
 if(numnp<21) final = numnp 
 call mostrarConteudoG(adjArray, inicio, final)
 inicio=numnp-5; final=numnp
 if(numnp>21) call mostrarConteudoG(adjArray, inicio, final)


 eqBandaMaxOriginal=bandaMax(adjArray)
 print*, "banda_ original = ", maiorValor(adjArray(eqBandaMaxOriginal)%next)-menorValor(adjArray(eqBandaMaxOriginal)%next)+1

 origem=1; destino=1; !(??? qual o significado de destino para o algoritmo bfs)
 caminho => bfs (adjArray, neq, origem, destino)
 !call mostrarConteudoL(caminho)
 !call mostrarConteudoG(adjArray, 1, 10)
 !call mostrarConteudoG(adjArray,numnp-10, numnp)
 call renumerar()

 eqBandaMax=bandaMax(adjArray)
 print*, "banda_ modificada = ", maiorValor(adjArray(eqBandaMax)%next)-menorValor(adjArray(eqBandaMax)%next)+1

 print*, " conectividades dos elementos da numeração modificada"
 inicio=1; final=5
 if(numel<=12) final=numel
 call mostrarLM(LM, nen, numel, inicio, final)
 inicio=numel-5; final=numel
 if(numel>12) call mostrarLM(LM, nen, numel, inicio, final)

 print*, " adjacencias nodais modificada  "
 inicio=1; final=numnp
 if(numnp<21) final = numnp 
 call mostrarConteudoG(adjArray, inicio, final)
 inicio=numnp-5; final=numnp
 if(numnp>21) call mostrarConteudoG(adjArray, inicio, final)
 return
contains 

subroutine renumerar()
 type(verticeL), pointer :: caminhoC
 integer :: i, dof, eq, nel
 !2024 write(*, '(a)', advance='no') "numeracao 1= " 
 !2024 do eq = 1, neq; write(*, '(i0,", ")', advance="no") acessarNum(listaVertices(eq)); end do; print*
 eq = neq
 caminhoC=>caminho
 do while (associated(caminhoC)) 
!    print'(i0,a,i0)', eq,' vai para posição ', acessarNum(caminhoC%V);
    call atribuirNum(caminhoC%V,eq) ! altera os num dos vertices da listaVertices tambem 
    caminhoC=>caminhoC%next
    eq = eq-1
 end do; 
!2024 write(*, '(a)', advance='no') "renumerado =" 
!2024 do eq = 1, neq; write(*, '(i0,", ")', advance="no") acessarNum(listaVertices(eq)); end do; print*
!2024 write(*, '(a)', advance='no') "id = " 
!2024 write(*, '(100(i0,", "))') id(:,1:numnp)
 do i = 1, numnp
!2024  if(id(1,i) > 0) print*,i, id(1,i),  acessarNum(listaVertices(id(1,i)))
   if(id(1,i) > 0)  id(1,i) = acessarNum(listaVertices(id(1,i)))
 end do; 
!2024 write(*, '(a)', advance='no') "id = " 
!2024 write(*, '(100(i0,", "))') id(:,1:numnp)

!2024 do nel = 1, numel; print '(a,i0,a,4i5)',"nel=",nel, ', LM=',LM(:,nel); end do 
 do nel = 1, numel
    do dof=1, ndof*nen
    if(lm(dof,nel) > 0)  lm(dof,nel) = acessarNum(listaVertices(lm(dof,nel)))
    end do
 end do
!2024 do nel = 1, numel; print '(a,i0,a,4i5)',"nel=",nel, ', LM=',LM(:,nel); end do 

 !return 
 allocate(adjArrayC(1:neq)); !adjArray=>null()
 caminhoC=>caminho
 i = neq
 do while (associated(caminhoC)) 
    adjArrayC(i)=adjArray(acessarNum(caminhoC%V))
    caminhoC=>caminhoC%next
    i = i-1
 end do; 

 deallocate(adjArray); !adjArray=>null()

 adjArray=adjArrayC
 write(*, '(a)', advance='yes') " numeracoes " 
 write(*, '(a)', advance='yes') " original -> modificada" 
 do i = 1, 10
    write(*, '(8x, i0," -> ",i0)', advance="yes") i,  acessarNum(listaVertices(i))
 end do; print*
 do i = neq-10, neq
    write(*, '( i0," -> ",i0)', advance="yes") i,  acessarNum(listaVertices(i))
 end do; print*

end subroutine renumerar

 subroutine mostrarLM(LM_, nen_, numel_, inicio_, fim_)
 integer :: nen_, numel_, inicio_, fim_
 integer :: LM_(nen_,numel_)
 integer :: nel
 print*, "em  mostrarLM, total de elementos: " , numel
 print*, "conectividades nodais, entre os elementos = ", inicio_ , " a ", fim_

  do nel = inicio_, fim_
    print '(a,i0,a,4(i0,2x))', "elemento ", nel, ", LM=", LM_(1:4,nel)
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
