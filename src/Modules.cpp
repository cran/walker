#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4rw1_model_mod) {


    class_<rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> >("model_rw1_model")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_rw1_model_namespace::model_rw1_model, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4rw1_model_naive_mod) {


    class_<rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> >("model_rw1_model_naive")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_rw1_model_naive_namespace::model_rw1_model_naive, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4rw_glm_model_mod) {


    class_<rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> >("model_rw_glm_model")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_rw_glm_model_namespace::model_rw_glm_model, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4walker_glm_mod) {


    class_<rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> >("model_walker_glm")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_walker_glm_namespace::model_walker_glm, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
#include <Rcpp.h>
using namespace Rcpp ;
#include "include/models.hpp"

RCPP_MODULE(stan_fit4walker_lm_mod) {


    class_<rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> >("model_walker_lm")

    .constructor<SEXP,SEXP,SEXP>()


    .method("call_sampler", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::call_sampler)
    .method("param_names", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::param_names)
    .method("param_names_oi", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::param_names_oi)
    .method("param_fnames_oi", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::param_fnames_oi)
    .method("param_dims", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::param_dims)
    .method("param_dims_oi", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::param_dims_oi)
    .method("update_param_oi", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::update_param_oi)
    .method("param_oi_tidx", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::param_oi_tidx)
    .method("grad_log_prob", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::grad_log_prob)
    .method("log_prob", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::log_prob)
    .method("unconstrain_pars", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::unconstrain_pars)
    .method("constrain_pars", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::constrain_pars)
    .method("num_pars_unconstrained", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::num_pars_unconstrained)
    .method("unconstrained_param_names", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::unconstrained_param_names)
    .method("constrained_param_names", &rstan::stan_fit<model_walker_lm_namespace::model_walker_lm, boost::random::ecuyer1988> ::constrained_param_names)
    ;
}
