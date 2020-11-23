#include<bits/stdc++.h> 
using namespace std; 
  
// Define the number of runs for the test data 
// generated 
#define RUN 1 
// Define the maximum number of vertices of the graph 
#define MAX_VERTICES 20 
// Define the maximum number of edges 
#define MAX_EDGES 200 
// Define the maximum weight of edges 
#define MAXWEIGHT 200 
#define MAX_PROC 10
#define Inf 10000000

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
        scanf("%d", &NUM); 
  
        // Define the maximum number of edges of the graph 
        // Since the most dense graph can have N*(N-1)/2 edges 
        // where N =  n number of vertices in the graph 
        NUMEDGE = min( 6 * NUM, NUM * (NUM - 1) / 2); 
  
        // First print the number of vertices and edges 
        printf("%d %d\n", NUM, NUMEDGE); 
  
        // Then print the edges of the form (a b) 
        // where 'a' is connected to 'b' 
        for (int j=1; j<=NUMEDGE; j++) 
        { 
            int a = 1 + rand() % NUM; 
            int b = 1 + rand() % NUM; 
            pair<int, int> p = make_pair(a, b); 
  
            // Search for a random "new" edge every time 
            // Note - In a tree the edge (a, b) is same 
            // as the edge (b, a) 
            while (container.find(p) != container.end()) 
            { 
                a = 1 + rand() % NUM; 
                b = 1 + rand() % NUM; 
                p = make_pair(a, b); 
            } 
            container.insert(p); 
        } 

        for (it=container.begin(); it!=container.end(); ++it) 
        { 
            int wt = 1 + rand() % MAXWEIGHT; 
            // printf("%d %d %d\n", it->first, it->second, wt);
            graph[it->first][it->second] = wt; 
        } 

        int source = 1;
        int num_proc;
        scanf("%d", &num_proc);
        cout<<endl;
        cout<<"Num_Proc: "<<num_proc<<" Source: "<<source<<endl;

        ofstream myfile;
        char ifname[1000];
        sprintf(ifname, "test_%d_%d.txt", NUM, num_proc);
        printf("filename:%s\n", ifname);
        myfile.open ( ifname ); 
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
