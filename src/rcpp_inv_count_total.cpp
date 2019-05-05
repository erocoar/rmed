#include <Rcpp.h>
#include <iostream>
#include <utility>
#include <functional>
#include <vector>
#include <algorithm>
using namespace Rcpp;
//Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

// Count inversion involvement per line in dual space.
std::pair<std::vector<int>, int> inv_count(std::vector<int> arr, int count)
{
  if (arr.size() <= 1) {
    return std::make_pair(arr, count);
  }
  else {
    std::vector<int> a, b, c;
    unsigned int ai, bi, inversions;
    std::pair<std::vector<int>, int> left, right;
    
    unsigned int middle = ((int)arr.size() + 1) / 2;
    for (int i = 0; i < middle; i++) {
      a.push_back(arr[i]);
    }
    for (int i = middle; i < (int)arr.size(); i++) {
      b.push_back(arr[i]);
    }
    
    left = inv_count(a, count);
    a = left.first;
    ai = left.second;
    
    right = inv_count(b, count);
    b = right.first;
    bi = right.second;
    inversions = ai + bi;
    
    unsigned int i = 0;
    unsigned int j = 0;
    
    while(i < a.size() && j < b.size()) {
      if (a[i] <= b[j]) {
        c.push_back(a[i]);
        i++;
      }
      else {
        c.push_back(b[j]);
        inversions += a.size() - i;
        j++;
      }
    }
    for (int u = i; u < a.size(); u++) {
      c.push_back(a[u]);
    }
    for (int u = j; u < b.size(); u++) {
      c.push_back(b[u]);
    }
    return std::make_pair(c, inversions);
  }
}

// [[Rcpp::export]]
int get_inv_count(std::vector<int> arr)
{
  int count = 0;
  return inv_count(arr, count).second;
}