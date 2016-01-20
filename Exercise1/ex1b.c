#include <stdio.h>
#include <stdlib.h>

#define max_length 1000000

typedef struct trie
{
	int num_children;
	int num_times;
	char data;
	struct trie **children;
}trie;

long long  int max;

//
// Create a new trie
//
trie* trie_new()
{
	trie *t = (trie*)malloc(sizeof(trie));
	t->num_children = 0;
	t->num_times = 0;
	t->data = '\0';
	t->children = NULL;

   return t;
}

//
// Function that creates a new child
//
trie ** trie_new_child(trie **t, int size, char data)
{
	int i=0;

	trie **children = (trie**)malloc(sizeof(trie *)*(size + 1));

	for(;i<size;i++)
	{
		children[i] = t[i];
	}

	children[size] = (trie *)malloc(sizeof(trie));
	children[size]->data = data;
	children[size]->num_children = 0;
	children[size]->num_times = 1;
	children[size]->children = NULL;

	free(t);

	return children;
}

void trie_insert(trie *t,char* key,int j, int depth)
{
	const char END = '\0';
	int i;
	
        //skip unwanted characters (does not add them to the trie)
	if(key[j] == '\r' || key[j] == '\t' || key[j] == '\n'){
		trie_insert(t, key, ++j, depth);
		return;
	}

	//printf("\n\t{key[j]=%c,j=%d,depth=%d|", key[j], j, depth);
	
	if(key[j] == END){
		return;
	}

	/*
	Check the children for the current character your on (key[j]) 
	and if it is found then recursive call and if it isn't found then 
	create a new child with that letter and then recursive call on 
	that newly created child
	*/
	for(i=0;i<t->num_children;i++)
	{
		if(t->children[i]->data == key[j])
		{
			t->children[i]->num_times++;
			max = (depth*(t->children[i]->num_times) < max)? max : depth*(t->children[i]->num_times);

			trie_insert(t->children[i], key, ++j, ++depth);
			return;
		}
	}

	t->children = trie_new_child(t->children, t->num_children, key[j]);
	t->num_children++;
	max = (depth < max)? max : depth; //depth*1; since this is the first time adding the prefix
	trie_insert(t->children[t->num_children-1], key, ++j, ++depth);
}

//
// program entrypoint
//
int main(int argc,char *argv[])
{
	trie* t;
	char ch[max_length];
	
	t = trie_new();
	
	while(fgets(ch, max_length, stdin) != NULL){
		trie_insert(t,ch,0,1);
	}

	printf("%llu",max);
    getchar();
	return 0;
}
