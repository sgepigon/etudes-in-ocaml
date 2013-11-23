
#include <stdlib.h>
#include <stdio.h>

typedef struct LNode {
    int value;
    struct LNode *prev;
    struct LNode *next;
} LNode;

void print(LNode *head) {
    printf("%d ", head->value);

    if (head->next == NULL) {
	printf("\n", head->value);
	return;
    }
        
    print(head->next);
}

void reverse(LNode *front, LNode *back) {
    int finish = 0;

    if (front == back) { // odd
	return;
    }

    if (back == front->next) { // even
	finish = 1;
    }

    int a = front->value;
    front->value = back->value;
    back->value = a;
      
    if (finish) return;
    
    reverse(front->next, back->prev);    
}

int main(int argc, char **argv){
    int s = 7;
    if (argc > 1)
	sscanf(argv[1], "%d", &s);
    if (s == 0) {
	printf("0\n0\n");
	return 0;
    }

    LNode **hello = malloc(s * sizeof(*hello));
    for (int i = 0; i < s; i++) {
	hello[i] = malloc(sizeof(hello));
	hello[i]->value = i+4;

	hello[i]->prev = NULL;
	if (i > 0)
	    hello[i]->prev = hello[i-1];
    }
    for (int i = 0; i < s; i++) {
	hello[i]->next = hello[i+1];
	if (i == s-1)
	    hello[i]->next = NULL;	
    }

    print(hello[0]);

    reverse(hello[0], hello[s-1]);

    print(hello[0]);
}
