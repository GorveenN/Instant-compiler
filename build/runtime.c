#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void printInt(int x)
{
	printf("%d\n", x);
}

void printString(char *string)
{
	printf("%s\n", string);
}

char *__str_concat(char *s1, char *s2)
{
	char *new = (char *)malloc(strlen(s1) + strlen(s2));
	if (new == NULL)
	{
		exit(1);
	}
	strcpy(new, s1);
	strcpy(new, s2);
	return new;
}
