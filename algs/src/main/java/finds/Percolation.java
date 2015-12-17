package finds;
import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.StdRandom;
import edu.princeton.cs.algs4.StdStats;
import edu.princeton.cs.algs4.WeightedQuickUnionUF;

public class Percolation {
    private  int size, vsize, top, bottom;
    private  int[] sites;
    private  WeightedQuickUnionUF uf;


    public Percolation(int N) // create N-by-N grid, with all sites blocked
    {
        size    = N + 2;
        vsize   = N * N + 2;
        top     = 0;
        bottom  = vsize + 1;

        sites = new int[vsize];
        sites[top] = 1;
        sites[bottom] = 1;

        this.uf = new WeightedQuickUnionUF(vsize);

        for(int i = 1; i <= size; i++)
        {
            uf.union(0,i);
        }

//        for (int i=bottom-N; i<=bottom-1; i++)
//        {
//            uf.union(bottom,i);
//        }
    }

    public void open(int i, int j)          // open site (row i, column j) if it is not open already
    {
        checkBounds(i,j);
        int base = i * size;
        int diff = size -j;



    }
    public static void main(String[] args){
        Percolation pr = new Percolation(2);

        for (int i = 1; i < pr.size; i ++)
            StdOut.println(pr.sites[i]);
    }
     private void checkBounds(int i, int j)
     {
         if (i > size || j > size || i < 1 || j < 1) {
             throw new IndexOutOfBoundsException(
                     String.format("Index i out of bounds {%d} %d", i,j,size)
             );
         }
     }
}
