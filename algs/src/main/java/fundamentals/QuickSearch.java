package fundamentals;

import java.util.Scanner;

public class QuickSearch {
    public static int[] quicksort(int[] a) {
        sort(0, a.length - 1, a);
        return a;
    }

    public static void sort(int start, int end, int[] arr) {
        if (end - start > 0) {

            int left = start;
            int right = end;
            int piv = arr[start];

            while (left <= right) {
                while (arr[left] < piv) {
                    left++;
                }

                while (arr[right] > piv) {
                    right--;
                }

                if (left <= right) {
                    int temp = arr[left];
                    arr[left] = arr[right];
                    arr[right] = temp;
                    left++;
                    right--;
                }
            }
            sort(left, end, arr);
            sort(start, right, arr);
        }
    }

    public static void main(String[] args) {
        Scanner s = new Scanner(System.in);

        System.out.println("enter number of elements");

        int n = s.nextInt();

        int whitelist[] = new int[n];

        System.out.println("enter elements");

        for (int i = 0; i < n; i++) {//for reading array
            whitelist[i] = s.nextInt();

        }
        quicksort(whitelist);
        for (int e : whitelist) {
            System.out.print(e);
        }
    }

}
