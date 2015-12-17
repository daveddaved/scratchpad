
import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.StdRandom;
import edu.princeton.cs.algs4.StdStats;

public class PercolationStats {
    private int count;
    private Percolation pr;
    private double[] fractions;

    public PercolationStats(int N, int T) {
        if (N <= 0 || T <= 0) {
            throw new IllegalArgumentException();
        }
        count = T;
        fractions = new double[count];
        for (int expNum = 0; expNum < count; expNum++) {
            pr = new Percolation(N);
            int openedSites = 0;
            while (!pr.percolates()) {
                int i = StdRandom.uniform(1, N + 1);
                int j = StdRandom.uniform(1, N + 1);
                if (!pr.isOpen(i, j)) {
                    pr.open(i, j);
                    openedSites++;
                }
            }
            double fraction = (double) openedSites / (N * N);
            fractions[expNum] = fraction;
        }
    }

    public double mean() {
        return StdStats.mean(fractions);
    }

    public double stddev() {
        return StdStats.stddev(fractions);
    }

    public double confidenceLo() {
        return mean() - ((1.96 * stddev()) / Math.sqrt(count));
    }

    public double confidenceHi() {
        return mean() + ((1.96 * stddev()) / Math.sqrt(count));
    }

    public static void main(String[] args) {
        int N = Integer.parseInt(args[0]);
        int T = Integer.parseInt(args[1]);
        PercolationStats ps = new PercolationStats(N, T);

        String confidence = ps.confidenceLo() + ", " + ps.confidenceHi();
        StdOut.println("mean                    = " + ps.mean());
        StdOut.println("stddev                  = " + ps.stddev());
        StdOut.println("95% confidence interval = " + confidence);
    }
}