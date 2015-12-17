package w1;

import edu.princeton.cs.algs4.WeightedQuickUnionUF;
import edu.princeton.cs.algs4.StdOut;


public class Percolation {
    private WeightedQuickUnionUF union;
    private boolean[] sites;
    private int N, top, bottom;
//    private static boolean BLOCKED = false;
//    private static boolean OPEN = true;

    public Percolation(int N)                // create N-by-N grid, with all sites blocked
    {
        if (N < 1) throw new java.lang.IllegalArgumentException();
        this.N = N;
        this.top = N * N +1;
        this.bottom = 0;
        this.sites = new boolean[N * N + 2];
        this.union = new WeightedQuickUnionUF(N * N + 2);

    }

    private int xy(int x, int y) {
        return N * (y - 1) + x;
    }

    public boolean isOpen(int i, int j)     // is site (row i, column j) open?
    {
        if (!valid(i, j)) throw new java.lang.IndexOutOfBoundsException();
        return  sites[xy(i, j)]; //!= BLOCKED;
    }

    public void open(int i, int j)          // open site (row i, column j) if it is not open already
    {
        if (!valid(i, j)) throw new java.lang.IndexOutOfBoundsException(
                String.format("x(%d) y(%d)", i, j)
        );

        if (isOpen(i, j)) return ;
        int xy = xy(i, j);

        this.sites[xy] = true;

        if (valid(i - 1, j) && isOpen(i - 1, j)) {
            int left = xy(i - 1, j);
            union.union(xy, left);

        }

        if (valid(i + 1, j) && isOpen(i + 1, j)) {
            int right = xy(i + 1, j);
            union.union(xy, right);
        }

        if (valid(i, j+1) && isOpen(i, j + 1)) {
            int up = xy(i, j + 1);
            union.union(xy, up);
        }

        if (valid(i, j -1) && isOpen(i, j - 1)) {
            int down = xy(i, j - 1);
            union.union(xy, down);
        }

        if (i == 1) union.union(xy, top);
        if (i == N) union.union(xy, bottom);
    }

    private boolean valid(int x, int y) {
        return !(x < 1 || x > N  || y < 1 || y > N);
    }
    public boolean percolates()
    {
        return union.connected(top, bottom);
    }
    public boolean isFull(int i, int j)     // is site (row i, column j) full?
    {
        if (!valid(i, j)) throw new java.lang.IndexOutOfBoundsException();
        return isOpen(i, j) && union.connected(xy(i, j), top);
    }

    public static void main(String[] args)  // test client (optional)
    {

        Percolation p = new Percolation(3);
//        p.open(1,3);
//        p.open(2,3);
//        p.open(3,3);
        p.open(3,1);
        p.open(2,1);
        p.open(1,1);
        p.open(2,3);
//        p.open(3,2);
//        p.open(2,2);
//        p.open(2,5);
//        p.open(1,5);
//        p.open(1,3);
// //       p.open(3,5);
        StdOut.println(String.format("Percolates? : %b", p.percolates()));
        for (int row = 1; row <= p.N; row++) {
            for (int col = 1; col <= p.N; col++) {
                StdOut.print(
                        String.format(
                                "(col %d, row %d)(%d) -> (%b) | ",
                                col, row, p.xy(col,row), p.isOpen(row,col)));
                if (col % p.N == 0) StdOut.println();
            }
        }
        StdOut.println(String.format("Top : %d -> %b , bottom %d -> %b, size %d",
                p.top, p.sites[p.top],
                p.bottom, p.sites[p.bottom],
                p.sites.length));

    }

}

