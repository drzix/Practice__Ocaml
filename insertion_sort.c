#include <stdio.h>

void swap(int *lhs, int *rhs)
{
    int temp = *lhs;
    *lhs = *rhs;
    *rhs = temp;
}

void insert(int inserted, int *arr, int start, int len)
{
    if (len == 0 || arr[inserted] <= arr[start])
        return;

    swap(&arr[inserted], &arr[start]);
    insert(start, arr, start + 1, len - 1);
}

void sort(int *arr, int start, int len)
{
    if (len == 0)
        return;
    sort(arr, start + 1, len - 1);
    insert(start, arr, start + 1, len - 1);
}

int main()
{
    int i;
    int arr[] = {3, 2, 5, 4, 15, 7};
    int n = 6;
    sort(arr, 0, n);

    for (i = 0; i < n; i++)
        printf("%d, ", arr[i]);

    return 0;
}