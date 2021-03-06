#include <Rcpp.h>
#include "techfactor_types.h"
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
extern Alpha_fun alpha050;
extern Alpha_fun alpha051;
extern Alpha_fun alpha052;
extern Alpha_fun alpha053;
extern Alpha_fun alpha055;
extern Alpha_fun alpha057;
extern Alpha_fun alpha058;
extern Alpha_fun alpha059;
extern Alpha_fun alpha060;
extern Alpha_fun alpha063;
extern Alpha_fun alpha065;
extern Alpha_fun alpha066;
extern Alpha_fun alpha067;
extern Alpha_fun alpha068;
extern Alpha_fun alpha069;
extern Alpha_fun alpha070;
extern Alpha_fun alpha071;
extern Alpha_fun alpha072;
extern Alpha_fun alpha075;
extern Alpha_fun alpha076;
extern Alpha_fun alpha078;
extern Alpha_fun alpha079;
extern Alpha_fun alpha080;
extern Alpha_fun alpha081;
extern Alpha_fun alpha082;
extern Alpha_fun alpha084;
extern Alpha_fun alpha085;
extern Alpha_fun alpha086;
extern Alpha_fun alpha088;
extern Alpha_fun alpha089;
extern Alpha_fun alpha093;
extern Alpha_fun alpha094;
extern Alpha_fun alpha095;
extern Alpha_fun alpha096;
extern Alpha_fun alpha097;
extern Alpha_fun alpha098;
extern Alpha_fun alpha100;
extern Alpha_fun alpha102;
extern Alpha_fun alpha103;
extern Alpha_fun alpha106;
extern Alpha_fun alpha109;
extern Alpha_fun alpha110;
extern Alpha_fun alpha111;
extern Alpha_fun alpha112;
extern Alpha_fun alpha116;
extern Alpha_fun alpha117;
extern Alpha_fun alpha118;
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
extern Alpha_fun alpha175;
extern Alpha_fun alpha177;
extern Alpha_fun alpha178;
extern Alpha_fun alpha180;
extern Alpha_fun alpha181;
extern Alpha_fun alpha182;
extern Alpha_fun alpha183;
extern Alpha_fun alpha186;
extern Alpha_fun alpha187;
extern Alpha_fun alpha188;
extern Alpha_fun alpha189;
extern Alpha_fun alpha190;
extern Alpha_fun alpha191;
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
  {"alpha050", alpha_impl::alpha050},
  {"alpha051", alpha_impl::alpha051},
  {"alpha052", alpha_impl::alpha052},
  {"alpha053", alpha_impl::alpha053},
  {"alpha055", alpha_impl::alpha055},
  {"alpha057", alpha_impl::alpha057},
  {"alpha058", alpha_impl::alpha058},
  {"alpha059", alpha_impl::alpha059},
  {"alpha060", alpha_impl::alpha060},
  {"alpha063", alpha_impl::alpha063},
  {"alpha065", alpha_impl::alpha065},
  {"alpha066", alpha_impl::alpha066},
  {"alpha067", alpha_impl::alpha067},
  {"alpha068", alpha_impl::alpha068},
  {"alpha069", alpha_impl::alpha069},
  {"alpha070", alpha_impl::alpha070},
  {"alpha071", alpha_impl::alpha071},
  {"alpha072", alpha_impl::alpha072},
  {"alpha075", alpha_impl::alpha075},
  {"alpha076", alpha_impl::alpha076},
  {"alpha078", alpha_impl::alpha078},
  {"alpha079", alpha_impl::alpha079},
  {"alpha080", alpha_impl::alpha080},
  {"alpha081", alpha_impl::alpha081},
  {"alpha082", alpha_impl::alpha082},
  {"alpha084", alpha_impl::alpha084},
  {"alpha085", alpha_impl::alpha085},
  {"alpha086", alpha_impl::alpha086},
  {"alpha088", alpha_impl::alpha088},
  {"alpha089", alpha_impl::alpha089},
  {"alpha093", alpha_impl::alpha093},
  {"alpha094", alpha_impl::alpha094},
  {"alpha095", alpha_impl::alpha095},
  {"alpha096", alpha_impl::alpha096},
  {"alpha097", alpha_impl::alpha097},
  {"alpha098", alpha_impl::alpha098},
  {"alpha100", alpha_impl::alpha100},
  {"alpha102", alpha_impl::alpha102},
  {"alpha103", alpha_impl::alpha103},
  {"alpha106", alpha_impl::alpha106},
  {"alpha109", alpha_impl::alpha109},
  {"alpha110", alpha_impl::alpha110},
  {"alpha111", alpha_impl::alpha111},
  {"alpha112", alpha_impl::alpha112},
  {"alpha116", alpha_impl::alpha116},
  {"alpha117", alpha_impl::alpha117},
  {"alpha118", alpha_impl::alpha118},
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
  {"alpha174", alpha_impl::alpha174},
  {"alpha175", alpha_impl::alpha175},
  {"alpha177", alpha_impl::alpha177},
  {"alpha178", alpha_impl::alpha178},
  {"alpha180", alpha_impl::alpha180},
  {"alpha181", alpha_impl::alpha181},
  {"alpha182", alpha_impl::alpha182},
  {"alpha183", alpha_impl::alpha183},
  {"alpha186", alpha_impl::alpha186},
  {"alpha187", alpha_impl::alpha187},
  {"alpha188", alpha_impl::alpha188},
  {"alpha189", alpha_impl::alpha189},
  {"alpha190", alpha_impl::alpha190},
  {"alpha191", alpha_impl::alpha191}
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
extern Alpha_mfun alpha054;
extern Alpha_mfun alpha056;
extern Alpha_mfun alpha061;
extern Alpha_mfun alpha062;
extern Alpha_mfun alpha064;
extern Alpha_mfun alpha073;
extern Alpha_mfun alpha074;
extern Alpha_mfun alpha077;
extern Alpha_mfun alpha083;
extern Alpha_mfun alpha087;
extern Alpha_mfun alpha090;
extern Alpha_mfun alpha091;
extern Alpha_mfun alpha092;
extern Alpha_mfun alpha099;
extern Alpha_mfun alpha101;
extern Alpha_mfun alpha104;
extern Alpha_mfun alpha105;
extern Alpha_mfun alpha107;
extern Alpha_mfun alpha108;
extern Alpha_mfun alpha113;
extern Alpha_mfun alpha114;
extern Alpha_mfun alpha115;
extern Alpha_mfun alpha119;
extern Alpha_mfun alpha120;
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
extern Alpha_mfun alpha176;
extern Alpha_mfun alpha179;
extern Alpha_mfun alpha184;
extern Alpha_mfun alpha185;
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
  {"alpha054", alpha_impl::alpha054},
  {"alpha056", alpha_impl::alpha056},
  {"alpha061", alpha_impl::alpha061},
  {"alpha062", alpha_impl::alpha062},
  {"alpha064", alpha_impl::alpha064},
  {"alpha073", alpha_impl::alpha073},
  {"alpha074", alpha_impl::alpha074},
  {"alpha077", alpha_impl::alpha077},
  {"alpha083", alpha_impl::alpha083},
  {"alpha087", alpha_impl::alpha087},
  {"alpha090", alpha_impl::alpha090},
  {"alpha091", alpha_impl::alpha091},
  {"alpha092", alpha_impl::alpha092},
  {"alpha099", alpha_impl::alpha099},
  {"alpha101", alpha_impl::alpha101},
  {"alpha104", alpha_impl::alpha104},
  {"alpha105", alpha_impl::alpha105},
  {"alpha107", alpha_impl::alpha107},
  {"alpha108", alpha_impl::alpha108},
  {"alpha113", alpha_impl::alpha113},
  {"alpha114", alpha_impl::alpha114},
  {"alpha115", alpha_impl::alpha115},
  {"alpha119", alpha_impl::alpha119},
  {"alpha120", alpha_impl::alpha120},
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
  {"alpha170", alpha_impl::alpha170},
  {"alpha176", alpha_impl::alpha176},
  {"alpha179", alpha_impl::alpha179},
  {"alpha184", alpha_impl::alpha184},
  {"alpha185", alpha_impl::alpha185}
};
