import scipy.sparse as ss
#from scipy.sparse.csgraph import reverse_cuthill_mckee
import numpy as np

B = np.matrix('1 0 0 0 1 0 0 0;\
        0 1 1 0 0 1 0 1;\
        0 1 1 0 1 0 0 0;\
        0 0 0 1 0 0 1 0;\
        1 0 1 0 1 0 0 0;\
        0 1 0 0 0 1 0 1;\
        0 0 0 1 0 0 1 0;\
        0 1 0 0 0 1 0 1')

#Finite elemnt mesh numeration example
B = np.matrix('1 1 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 ;\
        1 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 ;\
        0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 ;\
        0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 ;\
        0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 ;\
        0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 ;\
        0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0 0 0 ;\
        1 1 0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0 ;\
        1 1 1 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 ;\
        0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 ;\
        0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 ;\
        0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 ;\
        0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 1 1 1 ;\
        0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0 1 1 ;\
        0 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0 ;\
        0 0 0 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 0 ;\
        0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 0 ;\
        0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 0 ;\
        0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 1 1 1 0 ;\
        0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 1 1 1 ;\
        0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 1 1 ')
print(B.A)

i = 0; j = 0;
print( "adjacents(") 
while i < 21:
    print( i+1 ,  end=": " )
    while j < 21:
      #  print( "i=", i,", j=", j,  B[i,j],  B[i,j] > 0) 
        if B[i,j] > 0:
            print( j+1 ,  end=", " )
        j+=1
    j=0
    i+=1
    print()

B = ss.csr_matrix(B)

np.random.seed(5)
N=21
ran=ss.random (N,N,.3, format='csr')#,  random_state=seed )
ran=ran+np.transpose(ran)+ss.identity(N)
#ran.data=np.ones(ran.data.shape).astype(np.uint8)
C = ss.csr_matrix((np.ones(ran.data.shape).astype(np.uint8),ran.indices, ran.indptr))

A=B[0:N,0:N]
print(" matriz A =\n", A.A)
#mstA = ss.csgraph.minimum_spanning_tree(A); print("minimum_spanning_tree : \n", mstA);
permA = ss.csgraph.reverse_cuthill_mckee(A)
print("vetor de permutação =\n", permA)
identity = np.identity(N,int)
P = ss.csr_matrix(identity[permA, :])
print("matrix de permutação =\n", P.A)
#Pt=P.transpose()
ApL=A[permA,:]
ApLC=ApL[:,permA]
print(" matriz A permutada : linhas + colunas =\n", ApLC.A)
print("vetor de permutação =\n", permA)

i = 0; j = 0;
print( "adjacents(") 
while i < 21:
    print( i+1 ,  end=": " )
    while j < 21:
      #  print( "i=", i,", j=", j,  B[i,j],  B[i,j] > 0) 
        if ApLC[i,j] > 0:
            print( j+1 ,  end=", " )
        j+=1
    j=0
    i+=1
    print()


