
#sed 's/:/;/' conectividades.txt | sed 's/nel =//' | sed 's/\.\.\.//'  | sed 's/,//g' | awk '{print "nel=",$1, "; LM(1:ndof*nen,nel) = (/",$3,",", $4,",", $5, ",", $6, "/)"}'  |tee conectividades.F90
sed 's/:/;/' conectividades.txt | sed 's/nel =//' | sed 's/\.\.\.//'  | sed 's/,//g' | awk '{print "nel=",$1, "LM(1:ndof*nen,nel) = (/", $2,",", $3,",", $4, ",", $5, "/)"}' |tee conectividades.F90
#sed 's/:...//' nosVizinhosDeNos.txt  |sed 's/,/ /g' | sed 's/vNode =//' |awk '{print "n=",9,";eq=", $1, "; LMstencilEq(eq, 1:n)=(/", $2,",", $3,",", $4,",", $5,",", $6,", ", $7,",", $8,",", $9, ",", $10, " /)" }' | tee nosVizinhos

