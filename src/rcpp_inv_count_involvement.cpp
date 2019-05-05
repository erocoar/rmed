#include <Rcpp.h>
#include <iostream>
#include <utility>
#include <functional>
#include <vector>
#include <algorithm>
using namespace Rcpp;
//Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

// Count inversion involvement per line in dual space.
std::pair<std::vector<int>, std::vector<int> > inv_inv_count(std::vector<int> arr, 
                                                          std::vector<int> counts)
{
  if (arr.size() <= 1) {
    return std::make_pair(arr, counts);
  }
  else {
    std::vector<int> a, b, c, ai, bi, inv_count;
    std::pair<std::vector<int>, std::vector<int> > left, right;
    
    unsigned int middle = ((int)arr.size() + 1) / 2;
    for (int i = 0; i < middle; i++) {
      a.push_back(arr[i]);
    }
    for (unsigned int i = middle; i < (int)arr.size(); i++) {
      b.push_back(arr[i]);
    }
    
    left = inv_inv_count(a, counts);
    a = left.first;
    ai = left.second;
    
    right = inv_inv_count(b, counts);
    b = right.first;
    bi = right.second;
    
    unsigned int i = 0;
    unsigned int j = 0;
    
    for (int u = 0; u < ai.size(); u++) {
      inv_count.push_back(ai[u] + bi[u]);
    }
    
    while(i < a.size() && j < b.size()) {
      if (a[i] <= b[j]) {
        c.push_back(a[i]);
        i++;
      }
      else {
        c.push_back(b[j]);
        for (int u = i; u < a.size(); u++) {
          inv_count.at(a[u] - 1) += 1;
        }
        inv_count.at(b[j] - 1) += (a.size() - i);
        j++;
      }
    }
    for (int u = i; u < a.size(); u++) {
      c.push_back(a[u]);
    }
    for (int u = j; u < b.size(); u++) {
      c.push_back(b[u]);
    }
    return std::make_pair(c, inv_count);
  }
}

// [[Rcpp::export]]
std::vector<int> get_inv_involvement(std::vector<int> arr)
{
  std::vector<int> counts (arr.size(), 0);
  return inv_inv_count(arr, counts).second;
}