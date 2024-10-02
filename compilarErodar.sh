cp bfs.F90 bfsTEMPLATE.F90
read -p "arguardando um ok para malha 0006x0006A"
cp conectiv0006x0006A.txt conectividades.txt
./criarFontesDados.sh  > /dev/null 
head conectividades.F90

sed  -e 's/=NUMNPX/=49/' -e 's/=NUMELX/=36/' bfsTEMPLATE.F90 > bfsM.F90
rm a.out; time gfortran bfsM.F90
time ./a.out |tee tela0006x0006A.txt
grep banda_ tela0006x0006A.txt
exit

cp conectiv0060x0020.txt conectividades.txt
./criarFontesDados.sh  > /dev/null 
head conectividades.F90

read -p "arguardando um ok para malha 0060x0020"
sed  -e 's/=NUMNPX/=61*21/' -e 's/=NUMELX/=60*20/' bfsTEMPLATE.F90 > bfsM.F90
rm a.out; time gfortran bfsM.F90
time ./a.out |tee tela0060x0020.txt
grep banda_ tela0060x0020.txt

read -p "arguardando um ok para malha 0600x0020"
cp conectiv0600x0020.txt conectividades.txt
./criarFontesDados.sh  > /dev/null 
head conectividades.F90

sed  -e 's/=NUMNPX/=601*21/' -e 's/=NUMELX/=600*20/' bfsTEMPLATE.F90 > bfsM.F90
rm a.out; time gfortran bfsM.F90
time ./a.out |tee tela0600x0020.txt
grep banda_ tela0600x0020.txt

read -p "arguardando um ok para malha 0006x0002A"
cp conectiv0006x0002A.txt conectividades.txt
./criarFontesDados.sh  > /dev/null 
head conectividades.F90

sed  -e 's/=NUMNPX/=21/' -e 's/=NUMELX/=12/' bfsTEMPLATE.F90 > bfsM.F90
rm a.out; time gfortran bfsM.F90
time ./a.out |tee tela0006x0002A.txt
grep banda_ tela0006x0002A.txt

read -p "arguardando um ok para malha 0006x0002C"
cp conectiv0006x0002C.txt conectividades.txt
./criarFontesDados.sh  > /dev/null 
head conectividades.F90

sed  -e 's/=NUMNPX/=21/' -e 's/=NUMELX/=12/' bfsTEMPLATE.F90 > bfsM.F90
rm a.out; time gfortran bfsM.F90
time ./a.out |tee tela0006x0002C.txt
