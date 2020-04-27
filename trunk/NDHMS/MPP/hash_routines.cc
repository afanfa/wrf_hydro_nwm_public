#include <map>

using namespace std;

void getLocalIndx_C(int * gLinkId, int *llinkid, int * llinkidindx, int glinksl, int llinklen)
{
  map<int,int> hash;
  map<int,int>::iterator it;

  for(int i=0; i < llinklen; i++)
    hash.insert(std::pair<int,int>(llinkid[i],i));

  for(int i=0; i < glinksl; i++)
    {
      it = hash.find(gLinkId[i]);
      if(it != hash.end())
	{
	  llinkidindx[it->second] = i;
	  hash.erase(it);
	}
    }

}
