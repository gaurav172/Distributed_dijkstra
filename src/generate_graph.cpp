#include<bits/stdc++.h> 
using namespace std; 
  
// Define the number of runs for the test data 
// generated 
#define RUN 1 
// Define the maximum number of vertices of the graph 
const int MAX_VERTICES = 5000;
// Define the maximum number of edges 
const int MAX_EDGES =  100; 
// Define the maximum weight of edges 
#define MAXWEIGHT 10
const int MAX_PROC = min(MAX_VERTICES,25); 
#define Inf -1

int graph[MAX_VERTICES+2][MAX_VERTICES+2];

int main() 
{ 

    for(int i=1;i<=MAX_VERTICES;i++)
    {
        for(int j=1;j<=MAX_VERTICES;j++)
        {
            if(i==j) graph[i][j] = 0;
            graph[i][j] = Inf;
        }
    } 
    set<pair<int, int>> container; 
    set<pair<int, int>>::iterator it; 
  
    // Uncomment the below line to store 
    // the test data in a file 
    // freopen("Test_Cases_Directed_Weighted_Graph.in", 
    //          "w", stdout); 
  
    // For random values every time 
    srand(time(NULL)); 
  
    int NUM;    // Number of Vertices 
    int NUMEDGE; // Number of Edges 
  
    for (int i=1; i<=RUN; i++) 
    { 
        NUM = 500; 
  
        // Define the maximum number of edges of the graph 
        // Since the most dense graph can have N*(N-1)/2 edges 
        // where N =  n number of vertices in the graph 
        NUMEDGE = 1 + rand() % MAX_EDGES; 
  
        while (NUMEDGE > NUM*(NUM-1)/2) 
            NUMEDGE = 1 + rand() % MAX_EDGES; 
  
        // First print the number of vertices and edges 
        // printf("%d %d\n", NUM, NUMEDGE); 
  
        // Then print the edges of the form (a b) 
        // where 'a' is connected to 'b' 
        for (int j=1; j<=NUMEDGE; j++) 
        { 
            // int a = 1 + rand() % NUM; 
            // int b = 1 + rand() % NUM; 
            // pair<int, int> p = make_pair(a, b); 
  
            // // Search for a random "new" edge every time 
            // // Note - In a tree the edge (a, b) is same 
            // // as the edge (b, a) 
            // while (container.find(p) != container.end()) 
            // { 
            //     a = 1 + rand() % NUM; 
            //     b = 1 + rand() % NUM; 
            //     p = make_pair(a, b); 
            // } 
            // container.insert(p); 

            int a = 1 + rand() % NUM; 
            int b = 1 + rand() % NUM; 
            pair<int, int> p = make_pair(a, b); 
            pair<int, int> reverse_p = make_pair(b, a); 
  
            // Search for a random "new" edge everytime 
            // Note - In a tree the edge (a, b) is same 
            // as the edge (b, a) 
            while (container.find(p) != container.end() || 
                    container.find(reverse_p) != container.end()) 
            { 
                a = 1 + rand() % NUM; 
                b = 1 + rand() % NUM; 
                p = make_pair(a, b); 
                reverse_p = make_pair(b, a); 
            } 
            container.insert(p); 
        } 
  
        for (it=container.begin(); it!=container.end(); ++it) 
        { 
            int wt = 1 + rand() % MAXWEIGHT; 
            // printf("%d %d %d\n", it->first, it->second, wt);
            graph[it->first][it->second] = wt;
            graph[it->second][it->first] = wt; 
        } 

        int source = 1;
        int num_proc = 2;
        num_proc = min(num_proc, NUM);
        cout<<endl;
        cout<<"Num_Proc: "<<num_proc<<" Source: "<<source<<endl;
        cout<<"Num Vertices:"<<NUM<<" Num Edges:"<<NUMEDGE<<endl;
        ofstream myfile;
        myfile.open ("test3.txt");
        myfile<<NUM<<" "<<num_proc<<" "<<source<<endl;
        for(int i=1;i<=NUM;i++)
        {
            for(int j=1;j<=NUM;j++)
            {
                myfile<<graph[i][j]<<" ";         
            }
            myfile<<endl;
        } 
        myfile.close();
        container.clear(); 
        // printf("\n"); 
  
    } 
  
    // Uncomment the below line to store 
    // the test data in a file 
    // fclose(stdout); 
    return(0); 
} 