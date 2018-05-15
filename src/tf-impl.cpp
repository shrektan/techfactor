#include <Rcpp.h>
#include "GCAMCTF_types.h"
#include <string>
#include <map>

namespace alpha_impl
{
extern Alpha_fun alpha001;
extern Alpha_fun alpha002;
extern Alpha_fun alpha003;
extern Alpha_fun alpha004;
extern Alpha_fun alpha005;
extern Alpha_fun alpha006;
extern Alpha_fun alpha007;
extern Alpha_fun alpha008;
extern Alpha_fun alpha009;
extern Alpha_fun alpha010;
extern Alpha_fun alpha011;
extern Alpha_fun alpha014;
extern Alpha_fun alpha053;
extern Alpha_fun alpha149;
}


/* The reason that it has to be the reference of Alpha_fun rather
 * than the copy is the function is defined externally. If we copy
 * insert into the tf_calculator, it will fail to link the external
 * definitions and result a failure `std_bad_call()` when called.
 */

std::map<std::string, Alpha_fun&> tf_caculators
{
  {"alpha001", alpha_impl::alpha001},
  {"alpha002", alpha_impl::alpha002},
  {"alpha003", alpha_impl::alpha003},
  {"alpha004", alpha_impl::alpha004},
  {"alpha005", alpha_impl::alpha005},
  {"alpha006", alpha_impl::alpha006},
  {"alpha007", alpha_impl::alpha007},
  {"alpha008", alpha_impl::alpha008},
  {"alpha009", alpha_impl::alpha009},
  {"alpha010", alpha_impl::alpha010},
  {"alpha011", alpha_impl::alpha011},
  {"alpha014", alpha_impl::alpha014},
  {"alpha053", alpha_impl::alpha053},
  {"alpha149", alpha_impl::alpha149}
};


std::map<
  std::string,
  std::function<Rcpp::DataFrame(Quote&, const Rcpp::newDateVector)>
> tf_fast_caculators
{

};