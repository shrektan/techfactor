#include <Rcpp.h>
#include "GCAMCTF_types.h"
#include <string>
#include <map>

namespace alpha_impl
{
extern Alpha_fun alpha001;
extern Alpha_fun alpha003;
extern Alpha_fun alpha005;
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
  {"alpha003", alpha_impl::alpha003},
  {"alpha005", alpha_impl::alpha005},
  {"alpha014", alpha_impl::alpha014},
  {"alpha053", alpha_impl::alpha053},
  {"alpha149", alpha_impl::alpha149}
};


std::map<std::string, Alpha_mfun&> tf_mcaculators
{

};
