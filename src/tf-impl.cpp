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
extern Alpha_fun alpha012;
extern Alpha_fun alpha013;
extern Alpha_fun alpha014;
extern Alpha_fun alpha015;
extern Alpha_fun alpha016;
extern Alpha_fun alpha017;
extern Alpha_fun alpha018;
extern Alpha_fun alpha019;
extern Alpha_fun alpha020;
extern Alpha_fun alpha021;
extern Alpha_fun alpha022;
extern Alpha_fun alpha023;
extern Alpha_fun alpha024;
extern Alpha_fun alpha025;
extern Alpha_fun alpha026;
extern Alpha_fun alpha027;
extern Alpha_fun alpha028;
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
  {"alpha012", alpha_impl::alpha012},
  {"alpha013", alpha_impl::alpha013},
  {"alpha014", alpha_impl::alpha014},
  {"alpha015", alpha_impl::alpha015},
  {"alpha016", alpha_impl::alpha016},
  {"alpha017", alpha_impl::alpha017},
  {"alpha018", alpha_impl::alpha018},
  {"alpha019", alpha_impl::alpha019},
  {"alpha020", alpha_impl::alpha020},
  {"alpha021", alpha_impl::alpha021},
  {"alpha022", alpha_impl::alpha022},
  {"alpha023", alpha_impl::alpha023},
  {"alpha024", alpha_impl::alpha024},
  {"alpha025", alpha_impl::alpha025},
  {"alpha026", alpha_impl::alpha026},
  {"alpha027", alpha_impl::alpha027},
  {"alpha028", alpha_impl::alpha028},
  {"alpha053", alpha_impl::alpha053},
  {"alpha149", alpha_impl::alpha149}
};


std::map<
  std::string,
  std::function<Rcpp::DataFrame(Quote&, const Rcpp::newDateVector)>
> tf_fast_caculators
{

};