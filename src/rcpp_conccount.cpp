#include <Rcpp.h>
#include <iostream>
#include <utility>
#include <functional>
#include <vector>
#include <algorithm>
#include <tuple>
using namespace Rcpp;
//Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
  
std::pair<std::vector<int>, std::vector<int> > conc_count(std::vector<int> arr, 
                                                          std::vector<int> counts)
{
  if (arr.size() <= 1) {
    return std::make_pair(arr, counts);
  }
  else {
    std::vector<int> a, b, c, ai, bi, inv_count;
    std::pair<std::vector<int>, std::vector<int> > left, right;
    
    int middle = ((int)arr.size() + 1) / 2;
    for (int i = 0; i < middle; i++) {
      a.push_back(arr[i]);
    }
    for (int i = middle; i < (int)arr.size(); i++) {
      b.push_back(arr[i]);
    }
    
    left = conc_count(a, counts);
    a = left.first;
    ai = left.second;
    
    right = conc_count(b, counts);
    b = right.first;
    bi = right.second;
    
    int i = 0;
    int j = 0;
    
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

std::tuple<std::vector<int>, std::vector<int>, std::vector<int> > inversion_pairs(std::vector<int> arr, std::vector<int> pairs, std::vector<int> lines)
{
  if (arr.size() <= 1) {
    return std::make_tuple(arr, pairs, lines);
  }
  else {
    
    std::vector<int> a, b, c, left_pair, right_pair;
    std::tuple<std::vector<int>, std::vector<int>, std::vector<int> > left, right;
    
    int middle = ((int)arr.size() + 1) / 2;
    for (int i = 0; i < middle; i++) {
      a.push_back(arr[i]);
    }
    for (int i = middle; i < (int)arr.size(); i++) {
      b.push_back(arr[i]);
    }
    
    left = inversion_pairs(a, pairs, lines);
    a = std::get<0>(left);
    
    right = inversion_pairs(b, pairs, lines);
    b = std::get<0>(right);
    
    left_pair = std::get<1>(left);
    right_pair = std::get<1>(right);
    
    pairs.reserve( left_pair.size() + right_pair.size() ); // preallocate memory
    pairs.insert( pairs.end(), left_pair.begin(), left_pair.end() );
    pairs.insert( pairs.end(), right_pair.begin(), right_pair.end() );
    
    int i = 0;
    int j = 0;
    
    while(i < a.size() && j < b.size()) {
      if (a[i] <= b[j]) {
        c.push_back(a[i]);
        i++;
      }
      else {
        c.push_back(b[j]);
        // Inversion, add pair 
        if (std::find(lines.begin(), lines.end(), b[j]) != lines.end()) {
          for (int u = i; u < a.size(); u++) {
            pairs.push_back(a[u]);
            pairs.push_back(b[j]);
          }
        } else {
          for (int u = i; u < a.size(); u++) {
            if (std::find(lines.begin(), lines.end(), a[u]) != lines.end()) {
              pairs.push_back(a[u]);
              pairs.push_back(b[j]);
            }
          }
        }
        j++;
      }
    }
    for (int u = i; u < a.size(); u++) {
      c.push_back(a[u]);
    }
    for (int u = j; u < b.size(); u++) {
      c.push_back(b[u]);
    }
    return std::make_tuple(c, pairs, lines);
  }
}
// [[Rcpp::export]]
std::vector<int> get_inv_pairs(std::vector<int> v, std::vector<int> lines)
{
  std::vector<int> empty_pairs; 
  return std::get<1>(inversion_pairs(v, empty_pairs, lines));
}

// [[Rcpp::export]]
std::vector<int> get_conc_count(std::vector<int> arr)
{
  std::vector<int> counts (arr.size(), 0);
  return conc_count(arr, counts).second;
}
