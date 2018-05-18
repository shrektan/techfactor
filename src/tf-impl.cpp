#include <Rcpp.h>
#include "GCAMCTF_types.h"
#include <string>
#include <map>

namespace alpha_impl
{
extern Alpha_fun alpha002;
extern Alpha_fun alpha003;
extern Alpha_fun alpha004;
extern Alpha_fun alpha005;
extern Alpha_fun alpha009;
extern Alpha_fun alpha011;
extern Alpha_fun alpha013;
extern Alpha_fun alpha014;
extern Alpha_fun alpha015;
extern Alpha_fun alpha018;
extern Alpha_fun alpha019;
extern Alpha_fun alpha020;
extern Alpha_fun alpha021;
extern Alpha_fun alpha022;
extern Alpha_fun alpha023;
extern Alpha_fun alpha024;
extern Alpha_fun alpha026;
extern Alpha_fun alpha027;
extern Alpha_fun alpha028;
extern Alpha_fun alpha029;
extern Alpha_fun alpha030;
extern Alpha_fun alpha031;
extern Alpha_fun alpha034;
extern Alpha_fun alpha038;
extern Alpha_fun alpha040;
extern Alpha_fun alpha043;
extern Alpha_fun alpha044;
extern Alpha_fun alpha046;
extern Alpha_fun alpha047;
extern Alpha_fun alpha049;
extern Alpha_fun alpha053;
extern Alpha_fun alpha122;
extern Alpha_fun alpha126;
extern Alpha_fun alpha127;
extern Alpha_fun alpha128;
extern Alpha_fun alpha129;
extern Alpha_fun alpha132;
extern Alpha_fun alpha133;
extern Alpha_fun alpha134;
extern Alpha_fun alpha135;
extern Alpha_fun alpha137;
extern Alpha_fun alpha139;
extern Alpha_fun alpha143;
extern Alpha_fun alpha144;
extern Alpha_fun alpha145;
extern Alpha_fun alpha146;
extern Alpha_fun alpha147;
extern Alpha_fun alpha149;
extern Alpha_fun alpha150;
extern Alpha_fun alpha151;
extern Alpha_fun alpha152;
extern Alpha_fun alpha153;
extern Alpha_fun alpha154;
extern Alpha_fun alpha155;
extern Alpha_fun alpha158;
extern Alpha_fun alpha159;
extern Alpha_fun alpha160;
extern Alpha_fun alpha161;
extern Alpha_fun alpha162;
extern Alpha_fun alpha164;
extern Alpha_fun alpha165;
extern Alpha_fun alpha166;
extern Alpha_fun alpha167;
extern Alpha_fun alpha168;
extern Alpha_fun alpha169;
extern Alpha_fun alpha171;
extern Alpha_fun alpha172;
extern Alpha_fun alpha173;
extern Alpha_fun alpha174;
}


/* The reason that it has to be the reference of Alpha_fun rather
 * than the copy is the function is defined externally. If we copy
 * insert into the tf_calculator, it will fail to link the external
 * definitions and result a failure `std_bad_call()` when called.
 */

std::map<std::string, Alpha_fun&> tf_caculators
{
  {"alpha002", alpha_impl::alpha002},
  {"alpha003", alpha_impl::alpha003},
  {"alpha004", alpha_impl::alpha004},
  {"alpha005", alpha_impl::alpha005},
  {"alpha009", alpha_impl::alpha009},
  {"alpha011", alpha_impl::alpha011},
  {"alpha013", alpha_impl::alpha013},
  {"alpha014", alpha_impl::alpha014},
  {"alpha015", alpha_impl::alpha015},
  {"alpha018", alpha_impl::alpha018},
  {"alpha019", alpha_impl::alpha019},
  {"alpha020", alpha_impl::alpha020},
  {"alpha021", alpha_impl::alpha021},
  {"alpha022", alpha_impl::alpha022},
  {"alpha023", alpha_impl::alpha023},
  {"alpha024", alpha_impl::alpha024},
  {"alpha026", alpha_impl::alpha026},
  {"alpha027", alpha_impl::alpha027},
  {"alpha028", alpha_impl::alpha028},
  {"alpha029", alpha_impl::alpha029},
  {"alpha030", alpha_impl::alpha030},
  {"alpha031", alpha_impl::alpha031},
  {"alpha034", alpha_impl::alpha034},
  {"alpha038", alpha_impl::alpha038},
  {"alpha040", alpha_impl::alpha040},
  {"alpha043", alpha_impl::alpha043},
  {"alpha044", alpha_impl::alpha044},
  {"alpha046", alpha_impl::alpha046},
  {"alpha047", alpha_impl::alpha047},
  {"alpha049", alpha_impl::alpha049},
  {"alpha053", alpha_impl::alpha053},
  {"alpha122", alpha_impl::alpha122},
  {"alpha126", alpha_impl::alpha126},
  {"alpha127", alpha_impl::alpha127},
  {"alpha128", alpha_impl::alpha128},
  {"alpha129", alpha_impl::alpha129},
  {"alpha132", alpha_impl::alpha132},
  {"alpha133", alpha_impl::alpha133},
  {"alpha134", alpha_impl::alpha134},
  {"alpha135", alpha_impl::alpha135},
  {"alpha137", alpha_impl::alpha137},
  {"alpha139", alpha_impl::alpha139},
  {"alpha143", alpha_impl::alpha143},
  {"alpha144", alpha_impl::alpha144},
  {"alpha145", alpha_impl::alpha145},
  {"alpha146", alpha_impl::alpha146},
  {"alpha147", alpha_impl::alpha147},
  {"alpha149", alpha_impl::alpha149},
  {"alpha150", alpha_impl::alpha150},
  {"alpha151", alpha_impl::alpha151},
  {"alpha152", alpha_impl::alpha152},
  {"alpha153", alpha_impl::alpha153},
  {"alpha154", alpha_impl::alpha154},
  {"alpha155", alpha_impl::alpha155},
  {"alpha158", alpha_impl::alpha158},
  {"alpha159", alpha_impl::alpha159},
  {"alpha160", alpha_impl::alpha160},
  {"alpha161", alpha_impl::alpha161},
  {"alpha162", alpha_impl::alpha162},
  {"alpha164", alpha_impl::alpha164},
  {"alpha165", alpha_impl::alpha165},
  {"alpha166", alpha_impl::alpha166},
  {"alpha167", alpha_impl::alpha167},
  {"alpha168", alpha_impl::alpha168},
  {"alpha169", alpha_impl::alpha169},
  {"alpha171", alpha_impl::alpha171},
  {"alpha172", alpha_impl::alpha172},
  {"alpha173", alpha_impl::alpha173},
  {"alpha174", alpha_impl::alpha174}
};



namespace alpha_impl
{
extern Alpha_mfun alpha001;
extern Alpha_mfun alpha006;
extern Alpha_mfun alpha007;
extern Alpha_mfun alpha008;
extern Alpha_mfun alpha010;
extern Alpha_mfun alpha012;
extern Alpha_mfun alpha016;
extern Alpha_mfun alpha017;
extern Alpha_mfun alpha025;
extern Alpha_mfun alpha032;
extern Alpha_mfun alpha033;
extern Alpha_mfun alpha035;
extern Alpha_mfun alpha036;
extern Alpha_mfun alpha037;
extern Alpha_mfun alpha039;
extern Alpha_mfun alpha041;
extern Alpha_mfun alpha042;
extern Alpha_mfun alpha045;
extern Alpha_mfun alpha048;
extern Alpha_mfun alpha121;
extern Alpha_mfun alpha123;
extern Alpha_mfun alpha124;
extern Alpha_mfun alpha125;
extern Alpha_mfun alpha130;
extern Alpha_mfun alpha131;
extern Alpha_mfun alpha136;
extern Alpha_mfun alpha138;
extern Alpha_mfun alpha140;
extern Alpha_mfun alpha141;
extern Alpha_mfun alpha142;
extern Alpha_mfun alpha148;
extern Alpha_mfun alpha156;
extern Alpha_mfun alpha157;
extern Alpha_mfun alpha163;
extern Alpha_mfun alpha170;
}

std::map<std::string, Alpha_mfun&> tf_mcaculators
{
  {"alpha001", alpha_impl::alpha001},
  {"alpha006", alpha_impl::alpha006},
  {"alpha007", alpha_impl::alpha007},
  {"alpha008", alpha_impl::alpha008},
  {"alpha010", alpha_impl::alpha010},
  {"alpha012", alpha_impl::alpha012},
  {"alpha016", alpha_impl::alpha016},
  {"alpha017", alpha_impl::alpha017},
  {"alpha025", alpha_impl::alpha025},
  {"alpha032", alpha_impl::alpha032},
  {"alpha033", alpha_impl::alpha033},
  {"alpha035", alpha_impl::alpha035},
  {"alpha036", alpha_impl::alpha036},
  {"alpha037", alpha_impl::alpha037},
  {"alpha039", alpha_impl::alpha039},
  {"alpha041", alpha_impl::alpha041},
  {"alpha042", alpha_impl::alpha042},
  {"alpha045", alpha_impl::alpha045},
  {"alpha048", alpha_impl::alpha048},
  {"alpha121", alpha_impl::alpha121},
  {"alpha123", alpha_impl::alpha123},
  {"alpha124", alpha_impl::alpha124},
  {"alpha125", alpha_impl::alpha125},
  {"alpha130", alpha_impl::alpha130},
  {"alpha131", alpha_impl::alpha131},
  {"alpha136", alpha_impl::alpha136},
  {"alpha138", alpha_impl::alpha138},
  {"alpha140", alpha_impl::alpha140},
  {"alpha141", alpha_impl::alpha141},
  {"alpha142", alpha_impl::alpha142},
  {"alpha148", alpha_impl::alpha148},
  {"alpha156", alpha_impl::alpha156},
  {"alpha157", alpha_impl::alpha157},
  {"alpha163", alpha_impl::alpha163},
  {"alpha170", alpha_impl::alpha170}
};
