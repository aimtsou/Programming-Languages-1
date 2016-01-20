#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>

#define CUTOFF 10
#define length(x) ( sizeof x / sizeof *x )


struct pivots {
  int left, right;
};

void swap ( int *a, int *b );
void insertion_sort ( int list[], int left, int right );
int median ( int list[], int left, int right );
struct pivots partition ( int list[], int left, int right );
void quicksort_r ( int list[], int left, int right );
void quicksort ( int list[], int left, int right );

void convert (char *ard,unsigned int *operand,unsigned int n)
{
	int i;
	for (i=0;i<n;i++){
		if(*(operand+i)<10)
            *(ard+i)=(*(operand+i))+48;
		else
			*(ard+i)=(*(operand+i)+55);
	}
	*(ard+n)='\0';
	return;
}

//
// Print the matrix
//
void print(int*ard,int n)
{
	int i;

	for (i = 0;i<n;i++)
        printf("%d",ard[i]);
}

//
// Find the next sorted number
//
void next_sorted(int* ard,int l ,int m,int b)
{
  int i =l;
  int j;
if (m%2 == 0){
for (i=0; i<m ; i++){
	if (ard[i] < ard[i+1])
		break;
	}
	for (j=0; j<=i; j++)
		 ard[j] = ard[i+1];
	for (i=m;i<2*m;i++)
		ard[i] = 0;
}
else{
for (i=0; i<m+1 ; i++){
	if (ard[i] < ard[i+1])
		break;
	}
	for (j=0; j<=i; j++)
		 ard[j] = ard[i+1];
	for (i=m;i<2*m;i++)
		ard[i] = 0;
}

}
//
// Check if the number is sorted
//
int check_sort(int* ard,int n)
{
    int i;

    for (i = n-1;i>=1;i--){
        if (ard[i]>ard[i-1])
           return 0;
    }
    return 1;
}

void swap ( int *a, int *b )
{
  int save = *a;
  *a = *b;
  *b = save;
}

void insertion_sort ( int list[], int left, int right )
{
  int i, j;

  for ( i = left + 1; i < right; i++ ) {
    int save = list[i];

    for ( j = i; j > 0 && list[j - 1] > save; j-- )
      list[j] = list[j - 1];

    list[j] = save;
  }
}

int median ( int list[], int left, int right )
{
  /* Find the median of three values in list, use it as the pivot */
  int mid = ( left + right ) / 2;

  if ( list[left] > list[mid] )
    swap ( &list[left], &list[mid] );

  if ( list[left] > list[right] )
    swap ( &list[left], &list[right] );

  if ( list[mid] > list[right] )
    swap ( &list[mid], &list[right] );

  swap ( &list[mid], &list[right - 1] );

  return list[right - 1];
}

struct pivots partition ( int list[], int left, int right )
{
  int k;
  int i = left, j = right - 1;
  int m = left, n = right - 1;
  int pivot = median ( list, left, right );
  struct pivots ret;

  /* Three way partition <,==,> */
  for ( ; ; ) {
    while ( list[++i] < pivot )
      ;
    while ( list[--j] > pivot ) {
      if ( j == left )
        break;
    }

    if ( i >= j )
      break;

    swap ( &list[i], &list[j] );

    if ( list[i] == pivot )
      swap ( &list[++m], &list[i] );

    if ( list[j] == pivot )
      swap ( &list[--n], &list[j] );
  }

  swap ( &list[i], &list[right - 1] );

  j = i - 1;
  i = i + 1;

  for ( k = left; k < m; k++, j-- )
    swap ( &list[k], &list[j] );

  for ( k = right - 1; k > n; k--, i++ )
    swap ( &list[k], &list[i] );

  ret.left = i;
  ret.right = j;

  return ret;
}

void quicksort_r ( int list[], int left, int right )
{
  /* Terminate on small subfiles */
  if ( left + CUTOFF <= right ) {
    struct pivots pivot = partition ( list, left, right );

    quicksort_r ( list, left, pivot.right );
    quicksort_r ( list, pivot.left, right );
  }
}

void quicksort ( int list[], int left, int right )
{
  quicksort_r ( list, left, right - 1 );

  /* Insertion sort on almost sorted list */
  insertion_sort ( list, left, right );
}

//
// Reverse the array(and the number)
//
void reverse (int* ard,int n){
	int *ptr1=&ard[0];
	int *ptr2=&ard[n-1];
	int temp;

	while(ptr1<ptr2){
		temp=*ptr1;
		*ptr1=*ptr2;
		*ptr2=temp;
		++ptr1;
		--ptr2;
	}
 }

//
// Substract 2 lists
//
void substract(int *ard, int *rev,int *sorted, int b, int n)
{
    int i;

	for (i = n-1; i>=0;i--){
		if (rev[i] >= sorted [i])
            ard[i] = rev[i] - sorted[i];
		else{
			ard[i] = rev[i] - sorted[i] + b;
			sorted[i-1]++;
		}
	}
}

//
// Substract by -1
//
void sub(int *ard, int b, int n)
{
    int i;
    int *sub;

    sub = (int*) malloc(sizeof(int)*n);

    for (i = 0;i<n-1;i++){
        sub[i] = 0;
    }
    sub[n-1] = 1;

    for (i = n-1; i>=0;i--){
		if (ard[i] >= sub [i])
            ard[i] = ard[i]-sub[i];
		else{
			ard[i] = ard[i]+b -sub[i];
			sub[i-1]++;
		}
	}
    free(sub);
}

static unsigned int magic(unsigned int b, unsigned int n)
{

	int i,j;
	int *digits = (int*)malloc(sizeof(int)*n);
	int *res = (int*)malloc(sizeof(int)*n);
	int *res2 = (int*)malloc(sizeof(int)*n);
	int *cpy = (int*)malloc(sizeof(int)*n);
	int *rev = (int*)malloc(sizeof(int)*n);
	int *cpy2 = (int*)malloc(sizeof(int)*n);
	
	int k = n/2;

	if ( b < 2 ){
		int *z;
		z = (int*)malloc(sizeof(int));
		z[0] = 0;
		print(z,1);
	}
	else{
		for (i = 0;i<n-1;i++){
			if (i <= n/2)
				digits[i] = b-1;
			else
				digits[i] = 0;
		}
		digits[n/2-1] = b-2;

		while(digits[0]>0){
			if(check_sort(digits,n)){
				memcpy(cpy,digits,(n * sizeof(int)));
				memcpy(rev,digits,(n * sizeof(int)));
				reverse(rev,n);
				substract(res,cpy,rev,b,n);
				memcpy(cpy,res,(n * sizeof(int)));
				quicksort(res,0,n);
				memcpy(cpy2,res,(n * sizeof(int)));
				reverse(res,n);
				substract(res2,res,cpy2,b,n);

				for(j=0;(j<n)&&(res2[j]==cpy[j]);j++){
					if ((j == n-1)&&(res2[0]>0)){
						char base_res[n];
						convert(base_res,res2,n);
						mpz_t bignum;
						mpz_init_set_str(bignum,base_res,b);
						char ten_num[100];
						mpz_get_str(ten_num,10,bignum);
						printf("%s",ten_num);
						return 0;
					}
				}
				sub(digits,b,n);
			}
			else
				next_sorted(digits,0,k,b);
		}
		int *z;
		z = (int*)malloc(sizeof(int));
		z[0] = 0;
		print(z,1);
}
return 0;
}

int main(int argc, char* argv[])
{
	unsigned int b;
	unsigned int n;

	if (argc != 3) {
	printf("Usage: magic b n\n"); return 1;
	}
	sscanf(argv[1], "%u", &b);
	sscanf(argv[2], "%u", &n);

	b = magic(b,n);
	getchar();
	return 0;
}
