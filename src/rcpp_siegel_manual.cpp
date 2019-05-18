#include <Rcpp.h>
using namespace Rcpp;
// TODO # change median_slopes to retrieve k_i_th rank instead of median

// [[Rcpp::export]]
NumericVector median_slopes(NumericVector x, NumericVector y, NumericVector idx, int mdn_idx) {
  idx = idx - 1;
  mdn_idx = mdn_idx - 1;
  NumericVector mdns (idx.length());
  for (int i = 0; i < idx.length(); i++) {
    NumericVector slopes = (y[idx[i]] - y) / (x[idx[i]] - x);
    slopes.erase(idx[i]);
    slopes[is_infinite(slopes)] = R_PosInf;
    slopes = slopes.sort();
    mdns[i] = slopes[mdn_idx];//median(slopes);
  }
  return(mdns);
}

// [[Rcpp::export]]
double median_estimator(NumericVector x, NumericVector y) {
  NumericVector mdns (x.length());
  for (int i = 0; i < x.length(); i++) {
    NumericVector slopes = (y[i] - y) / (x[i] - x);
    slopes.erase(i);
    slopes[is_infinite(slopes)] = R_PosInf;
    mdns[i] = median(slopes);
  }
  return(median(mdns));
}

//#http://dirk.eddelbuettel.com/code/rcpp/Rcpp-quickref.pdf
//#http://adv-r.had.co.nz/Rcpp.html#rcpp-sugar
//#https://teuder.github.io/rcpp4everyone_en/300_Rmath.html