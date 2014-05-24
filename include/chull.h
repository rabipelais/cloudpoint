#ifndef CONVEXHULL_HPP
#define CONVEXHULL_HPP

extern "C" {
    void convexhull(double *in, int numpoints, double *out, int *numhull, int *indices, int *numindices, double *area, double *volume);
}
#endif
