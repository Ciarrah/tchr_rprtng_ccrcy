* selecting dataset
use "M:\projects\ieu2\p1\015\working\data\CB\KS3_CB_2.dta", clear

* removing twins
drop if birth_ord=="B"

* generating ID's required for GCTA
egen id=concat(preg_iden birth_ord)
egen id2=concat(preg_iden birth_ord)

* inverse rank normalization of variables - GCTA requires normal data. This example for the variable "ks4_avptsent"
replace Bias_S=. if Bias_S==0
gen e=rnormal(0,0.00001)
gen new_bias=Bias_S+e
egen rank=rank(new_bias)
su new_bias, meanonly
gen inv_new_bias=invnormal((rank-0.5) / r(N))
*qnorm inv_new_bias
drop rank e new_bias

* select variables to keep
keep id id2 inv_new_bias

* replacing missing vcalues with "-9" - requirement of GCTA
replace inv_new_bias=-9 if inv_new_bias==.

* ensuring that variables are ordered correctly
order id id2 inv_new_bias 

* exporting dataset to GCTA required format
export delimited using M:\projects\ieu2\p1\015\working\data\Bias_S_KS3.pheno, delimiter(" ") novarnames replace

