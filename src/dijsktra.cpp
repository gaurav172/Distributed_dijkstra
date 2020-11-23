#include<bits/stdc++.h>
using namespace std;
typedef long long int ll;	
#define Inf 10000000

int main(){
    // ios::sync_with_stdio(false);
    int t = 1;
    while(t--)
    {
        vector<pair<int,int> > adj[10005];
        priority_queue< pair<int,int>,vector<pair<int,int> >,greater<pair<int,int> > > min_heap;
        int dist[10004];
        bool visited[10005];

        for(int i=0;i<10004;i++)
            visited[i]=false;
        
        int v,proc,source;
        cin>>v>>proc>>source;

        for(int i=1;i<=v;i++)
        {
            for(int j=1;j<=v;j++)
            {
                int wt;
                cin>>wt;
                if(wt == -1)
                    wt = Inf;
                if(wt!=0 && wt != Inf)
                {
                    adj[i].push_back(make_pair(j,wt));
                    adj[j].push_back(make_pair(i,wt));
                    // cout<<i<<" "<<j<<" "<<wt<<endl;
                }    
            }
            
        }
        cout<<"source: "<<source<<endl;
        // int source,desti;
        // cin>>source>>desti;

        for(int i=1;i<=v;i++)
            dist[i]=Inf;

        dist[source]=0;
        min_heap.push(make_pair(0,source));

        while(min_heap.size())
        {	
            int x = min_heap.top().second;
            min_heap.pop();	

            if(visited[x]==true)
                continue;

            visited[x]=true;

            for(int i=0;i<adj[x].size();++i)
            {
                if(dist[x]+adj[x][i].second < dist[adj[x][i].first])
                    dist[adj[x][i].first] = dist[x]+adj[x][i].second;	
                min_heap.push(make_pair(dist[adj[x][i].first], adj[x][i].first));
            }		
        }

        // if(dist[desti]==INT_MAX)
        //     cout<<"NO"<<endl;
        // else
        //     cout<<dist[desti]<<endl;
        // for(int i=1;i<=v;i++)
        //     cout<<"{"<<i<<","<<dist[i]<<"}"<<endl;
        cout<<dist[source]<<endl;
        cout<<endl;
    }
return 0;
}	