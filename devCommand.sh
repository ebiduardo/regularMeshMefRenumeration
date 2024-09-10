vi bfs.F90 ; rm a.out; gfortran bfs.F90 ; cp conectiv0006x0002A.txt conectividades.txt; ./criarFontesDados.sh  > /dev/null ; ./a.out |tee tela.txt ; grep "banda_\|numnp=" tela.txt
