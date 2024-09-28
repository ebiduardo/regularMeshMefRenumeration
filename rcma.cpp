//https://www.geeksforgeeks.org/reverse-cuthill-mckee-algorithm/ 
// C++ program for Implementation of
// Reverse Cuthill Mckee Algorithm

#include <bits/stdc++.h>
using namespace std;

vector<double> globalDegree;

int findIndex(vector<pair<int, double> > a, int x) {
	for (int i = 0; i < a.size(); i++)
		if (a[i].first == x)
			return i;
	return -1;
}

bool compareDegree(int i, int j) {
	return ::globalDegree[i] < ::globalDegree[j];
}

template <typename T>
ostream& operator<<(ostream& out, vector<T> const& v) {
	for (int i = 0; i < v.size(); i++)
		out << v[i] << ' ';
	return out;
}

class ReorderingSSM {
private:
	vector<vector<double> > _matrix;

public:
	// Constructor and Destructor
	ReorderingSSM(vector<vector<double> > m) { _matrix = m; }

	ReorderingSSM() {}
	~ReorderingSSM() {}

	// class methods

	// Function to generate degree of all the nodes
	vector<double> degreeGenerator() {
		vector<double> degrees;
		cout << " degrees ...  " << endl;
		for (int i = 0; i < _matrix.size(); i++) {
			double count = 0;
			for (int j=0; j< _matrix[0].size(); j++){count+= _matrix[i][j]; }
			count = 1;
			cout <<  count << ", ";
			degrees.push_back(count);
		}
			cout << endl;
		return degrees;
	}

	void printPair(pair<int, double> p_){
           cout <<  p_.first << " ;  " ;
	   return;
           cout <<  p_.first << ": " << p_.second <<" ;  " ;
	}
	void printQueue(std::queue<int> q_) {
	  while (!q_.empty()) {
  		  std::cout << q_.front() << " ";
   		 q_.pop();
 	 }
  	std::cout << std::endl;
}

	// Implementation of Cuthill-Mckee algorithm
	vector<int> CuthillMckee() {
		vector<double> degrees = degreeGenerator();
                ::globalDegree = degrees;
                 queue<int> Q;
		vector<int> R;
		vector<pair<int, double> > notVisited;

		for (int i = 0; i < degrees.size(); i++) notVisited.push_back(make_pair(i, degrees[i]));
		for (int i = 0; i < notVisited.size(); i++) printPair(notVisited[i]); cout << endl;

		// Vector notVisited helps in running BFS
		// even when there are dijoind graphs
		while (notVisited.size()) {
	  	   int minNodeIndex = 0;
		   for (int i = 0; i < notVisited.size(); i++) {
			   if (notVisited[i].second < notVisited[minNodeIndex].second)
				   minNodeIndex = i;
		   }
			cout << " visiting = " <<  notVisited[minNodeIndex].first << endl;
			Q.push(notVisited[minNodeIndex].first);

			cout << " Q.push(notVisited[minNodeIndex].first); Q ="; printQueue(Q); cout  << endl;

			notVisited.erase(notVisited.begin()+findIndex(notVisited, notVisited[Q.front()].first));
			// Simple BFS
			while (!Q.empty()) {
			   vector<int> toSort;
			   for (int i = 0; i < _matrix[0].size(); i++) {
			       if (i != Q.front() && _matrix[Q.front()][i]==1 && findIndex(notVisited, i) != -1) {
				      toSort.push_back(i);
				      cout << "including "<< i << " adj from ... "<<Q.front()<<" ... in toSort = " << toSort << endl;
				      notVisited.erase(notVisited.begin()+findIndex(notVisited, i));
				     } //  if (i != Q.front() && _matrix[Q...
			    } // for (int i = 0; i < _matrix[0].size(); i++) {
			        sort(toSort.begin(), toSort.end(), compareDegree);
			cout << " toSort = " << toSort << endl ;
				for (int i = 0; i < toSort.size(); i++) Q.push(toSort[i]);
			cout << " toSort -> Q = " ; printQueue(Q);
				R.push_back(Q.front());
				Q.pop();
			cout << " R.push_back(Q.front())(), R = " << R << endl; //printQueue(R);
			cout << " Q.pop(),  Q = " ; printQueue(Q);
			cout << " notVisited = " ;
		        for (int i = 0; i < notVisited.size(); i++) printPair(notVisited[i]); cout << endl;
			}
		}
		cout << "final de .. vector<int> CuthillMckee() { " << endl;
		cout << " R = " << R << endl;
		return R;
	}

	// Implementation of reverse Cuthill-Mckee algorithm
	vector<int> ReverseCuthillMckee() {
		vector<int> cuthill = CuthillMckee();
		int n = cuthill.size();
		//if (n % 2 == 0) n -= 1; n = n / 2; 
		for (int i = 0; i <= n/2; i++) {
			int j;
		               j = cuthill[n-1-i];
                  cuthill[n-1-i] = cuthill[i];
   		      cuthill[i] = j;
		} 
		return cuthill;
	}
};

// Driver Code
int num_rows;

vector<vector<double> > matrix;

 void entrada00(){ 
        num_rows = 10; 
	for (int i = 0; i < num_rows; i++) {
		vector<double> datai;
		for (int j = 0; j < num_rows; j++)
			datai.push_back(0.0);
		matrix.push_back(datai);
	} 
	// This is the test graph,
	// check out the above graph photo
	matrix[0] = { 0, 1, 0, 0, 0, 0, 1, 0, 1, 0 };
	matrix[1] = { 1, 0, 0, 0, 1, 0, 1, 0, 0, 1 };
	matrix[2] = { 0, 0, 0, 0, 1, 0, 1, 0, 0, 0 };
	matrix[3] = { 0, 0, 0, 0, 1, 1, 0, 0, 1, 0 };
	matrix[4] = { 0, 1, 1, 1, 0, 1, 0, 0, 0, 1 };
	matrix[5] = { 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 };
	matrix[6] = { 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 };
	matrix[7] = { 0, 0, 0, 0, 0, 0, 0, 0, 1, 1 };
	matrix[8] = { 1, 0, 0, 1, 0, 0, 0, 1, 0, 0 };
	matrix[9] = { 0, 1, 0, 0, 1, 0, 0, 1, 0, 0 };
 } // void entrada00{

 void entrada21A(){
  
	cout << "\n Finite elemnt mesh numeration example " << endl;
	num_rows = 21;

	for (int i = 0; i < num_rows; i++) {
		vector<double> datai;
		for (int j = 0; j < num_rows; j++)
			datai.push_back(0.0);
		matrix.push_back(datai);
	}
	
	// This is the test graph,
	// check out the above graph photo
        int i = 0;
                 //     1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
	matrix[i++] = { 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	matrix[i++] = { 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	matrix[i++] = { 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	matrix[i++] = { 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	matrix[i++] = { 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 };
	matrix[i++] = { 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 };
	matrix[i++] = { 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 };

	matrix[i++] = { 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 };
	matrix[i++] = { 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 };
	matrix[i++] = { 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 };
	matrix[i++] = { 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0 };
	matrix[i++] = { 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0 };
	matrix[i++] = { 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1 };
	matrix[i++] = { 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1 };

	matrix[i++] = { 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 };
	matrix[i++] = { 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0 };
	matrix[i++] = { 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 };
	matrix[i++] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0 };
	matrix[i++] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0 };
	matrix[i++] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1 };
	matrix[i] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1 };
	matrix[i][0] =  0;

        i = 0;
	cout << "B = np.matrix('";
	do{ 
	    cout << matrix[i++] ;
	    if(i<20) cout << ";" ;//<< endl;
	  } while (i< 21); 
	cout << "')" << endl;

        i = 0;
	int j = 0;
	cout << "adjacents(" << endl;
	do{ 
	do{ 
	    if (matrix[i][j++] != 0) cout << j <<  ", " ;
	  } while (j< 21); 
	  i++; j=0;
	     cout<< endl;
	  } while (i< 21); 
	cout << "')" << endl;
	//exit(0);



	cout << "\n Rudimental order of objects: " << endl;
        for (int i=0; i<num_rows; i++ ){ 
		cout << "\t" << i+1 << ","  ;
	       	if ((i)%7==6) cout  << endl;
	       	//if ((i)%3==2) cout  << endl;
      	}
        //for (i=0; i<num_rows; i++ ){ matrix[i][i]=0;}

 } // void entrada21A{

int main() {

	entrada21A();

	ReorderingSSM m(matrix);

	vector<int> r;


	r = m.CuthillMckee();
	cout << "\n Permutation order of objects: " << r  << endl;
        for (int i=0; i<num_rows; i++ ){ 
		cout << "\t" << r[i]+1 << ","  ;
	       	//if ((i)%7==6) cout  << endl;
	       	//if ((i)%3==2) cout  << endl;
      	}
	cout << endl;

	r = m.ReverseCuthillMckee();
	cout << "\n Permutation order of objects: " << r  << endl;
        for (int i=0; i<num_rows; i++ ){ 
		cout << "\t" << r[i]+1 << ","  ;
	       	//if ((i)%7==6) cout  << endl;
	       	//if ((i)%3==2) cout  << endl;
      	}
	cout << endl;

       // for (int i=0; i<num_rows; i++ ){ cout << "\t" << r[i]+1 << ","  ; if ((i)%7==6) cout  << endl;  }

	return 0;
}

