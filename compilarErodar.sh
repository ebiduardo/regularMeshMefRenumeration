cp bfs.F90 bfsTEMPLATE.F90

function process(){

  numel=$(echo "($nelx+0) * ($nely+0)" |bc);
  numnp=$(echo "($nelx+1) * ($nely+1)" |bc);
  echo numnp=$numnp, numel=$nelx X $nely; 

  cp conectiv$malha.txt conectividades.txt
  ./criarFontesDados.sh  > /dev/null 
  echo inicio de conectividades; head -5 conectividades.F90
  echo final  de conectividades; tail -5 conectividades.F90
  read -p "arguardando um ok para malha: $malha ."

  sed  -e "s/=NUMNPX/=$numnp/" -e "s/=NUMELX/=$numel/" bfsTEMPLATE.F90 > bfsM.F90
  rm a.out; time gfortran -O3 bfsM.F90
  time ./a.out |grep -v incluir > tela$malha.txt
  grep banda_ tela$malha.txt
  echo -e '.. \n\n  ..'
} # end function process(){

nelx=0006; nely=0002;
malha="${nelx}X${nely}A";
     process
grep "vizinhos\|^ toVisit\|nohs visited" tela$malha.txt
  echo -e '\n\n'
nelx=0006; nely=0002;
malha="${nelx}X${nely}C";
     process
nelx=0006; nely=0006;
malha="${nelx}X${nely}A";
     process
nelx=0060; nely=0020;
malha="${nelx}X${nely}";
     process
nelx=0600; nely=0020;
malha="${nelx}X${nely}";
     process


