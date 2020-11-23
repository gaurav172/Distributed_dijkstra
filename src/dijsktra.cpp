#include <bits/stdc++.h>
using namespace std;
#define ll long long int
#include <ext/pb_ds/assoc_container.hpp> 
#include <ext/pb_ds/tree_policy.hpp> 
using namespace __gnu_pbds;   
#define ordered_set tree<ll, null_type,less<ll>, rb_tree_tag,tree_order_statistics_node_update> 
#define ld long double
#define ff first
#define ss second
#define pb push_back
#define mp make_pair
#define all(a) a.begin(),a.end()
#define sz(a) (ll)(a.size())
const int M = 10004;
int wt[M][M],dist[M];
int main()
{
    ios_base::sync_with_stdio(0);cin.tie(0);cout.tie(0);
    int n,p,s;
    cin>>n>>p>>s;
    for(int i=1;i<=n;i++)
    {
        for(int j=1;j<=n;j++)
        {
            cin>>wt[i][j];
        }
    }
   
    for(int i=1;i<=n;i++)
        dist[i] = 1e9;

    dist[s] = 0;
    priority_queue<pair<int,int>,vector<pair<int,int>>,greater<pair<int,int>>> pq;
    pq.push({dist[s], s});



    while(!pq.empty())
    {
        auto P = pq.top();
        pq.pop();

        int id = P.ss;
        if(P.ff != dist[id])
            continue;

        for(int i=1;i<=n;i++)
        {
            if(dist[id] + wt[id][i] < dist[i])
            {
                dist[i] = dist[id] + wt[id][i];
                pq.push({dist[i], i});
            }
        }
    }


    ld res = duration.count();
    res = res/(1000000);
    // for(int i=1;i<=n;i++)
    //     cout<<i<<" "<<dist[i]<<endl;
    return 0;
}