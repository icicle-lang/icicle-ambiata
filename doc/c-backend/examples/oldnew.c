#include <stdbool.h>
#include <stdint.h>

typedef int64_t     date_t;
typedef const char *error_t;

// gen$date = DATE
struct icicle_state {
    date_t      gen_date;
    bool        acc_conv_6_set;
    int64_t     acc_conv_6_val;
    bool        acc_conv_12_set;
    int64_t     acc_conv_12_val;

    int64_t     new_count;
    int64_t    *new_fact;
    date_t     *new_date;

    int64_t     out_repl_fst_val;
    error_t     out_repl_fst_err;
    int64_t     out_repl_snd_val;
    error_t     out_repl_snd_err;
};

void compute(struct icicle_state *s)
{
    //init [Mutable] [Option Int] acc$conv$6 = None : Option Int;
    bool    acc_conv_6_set = false;
    int64_t acc_conv_6_val = 0;

    //init [Mutable] [Option Int] acc$conv$12 = None : Option Int;
    bool    acc_conv_12_set = false;
    int64_t acc_conv_12_val = 0;

    //load_resumable [Option Int] acc$conv$6;
    acc_conv_6_set = s->acc_conv_6_set;
    acc_conv_6_val = s->acc_conv_6_val;

    //load_resumable [Option Int] acc$conv$12;
    acc_conv_12_set = s->acc_conv_12_set;
    acc_conv_12_val = s->acc_conv_12_val;

    // for_facts (elem$gen$fact : Int, elem$gen$date : Date) in new {
    const int64_t        new_count = s->new_count;
    const int64_t *const new_fact  = s->new_fact;
    const date_t  *const new_date  = s->new_date;

    for (int64_t i = 0; i < new_count; i++) {
        int64_t elem_gen_fact = new_fact[i];
        date_t  elem_gen_date = new_date[i];

        // read [Mutable] [Option Int] acc$conv$6 = acc$conv$6;
        acc_conv_6_set = acc_conv_6_set;
        acc_conv_6_val = acc_conv_6_val;

        // let anf$9 = some# [Int] elem$gen$fact;
        bool    anf_9_set = true;
        int64_t anf_9_val = elem_gen_fact;

        // if (Option_isSome# [Int] acc$conv$6) {
        if (acc_conv_6_set) {
            // let flat$0 = unsafe_Option_get# [Int] acc$conv$6;
            int64_t flat_0 = acc_conv_6_val;

            // write acc$conv$6 = some# [Int] flat$0;
            acc_conv_6_set = true;
            acc_conv_6_val = flat_0;
        } else {
            //write acc$conv$6 = anf$9;
            acc_conv_6_set = anf_9_set;
            acc_conv_6_val = anf_9_val;
        }

        // read [Mutable] [Option Int] acc$conv$12 = acc$conv$12;
        acc_conv_12_set = acc_conv_12_set;
        acc_conv_12_val = acc_conv_12_val;

        // if (Option_isSome# [Int] acc$conv$12) {
        if (acc_conv_12_set) {
            // write acc$conv$12 = some# [Int] elem$gen$fact;
            acc_conv_6_set = true;
            acc_conv_6_val = elem_gen_fact;
        } else {
            //write acc$conv$6 = anf$9;
            acc_conv_6_set = anf_9_set;
            acc_conv_6_val = anf_9_val;
        }
    }

    // save_resumable [Option Int] acc$conv$6;
    s->acc_conv_6_set = acc_conv_6_set;
    s->acc_conv_6_val = acc_conv_6_val;

    // save_resumable [Option Int] acc$conv$12;
    s->acc_conv_12_set = acc_conv_12_set;
    s->acc_conv_12_val = acc_conv_12_val;

    // read [Mutable] [Option Int] conv$6 = acc$conv$6;
    bool    conv_6_set = acc_conv_6_set;
    int64_t conv_6_val = acc_conv_6_val;

    // read [Mutable] [Option Int] conv$12 = acc$conv$12;
    bool    conv_12_set = acc_conv_12_set;
    int64_t conv_12_val = acc_conv_12_val;

    // if (Option_isSome# [Int] conv$6) {
    if (conv_6_set) {
        // let flat$2 = unsafe_Option_get# [Int] conv$6;
        int64_t flat_2 = conv_6_val;

        // if (Option_isSome# [Int] conv$12) {
        if (conv_12_set) {
            // let flat$3 = unsafe_Option_get# [Int] conv$12;
            int64_t flat_3 = conv_12_val;

            // let conv$13 = pair# [Int] [Int] flat$2 flat$3;
            error_t conv_13_fst_err = 0;
            int64_t conv_13_fst_val = flat_2;
            error_t conv_13_snd_err = 0;
            int64_t conv_13_snd_val = flat_3;

            // output repl conv$13;
            s->out_repl_fst_err = conv_13_fst_err;
            s->out_repl_fst_val = conv_13_fst_val;
            s->out_repl_snd_err = conv_13_snd_err;
            s->out_repl_snd_val = conv_13_snd_val;
        } else {
            // let conv$13 = pair# [Int] [Int] flat$2 (error# "Fold1, but there is no value" : Int);
            error_t conv_13_fst_err = 0;
            int64_t conv_13_fst_val = flat_2;
            error_t conv_13_snd_err = "Fold1, but there is no value";
            int64_t conv_13_snd_val = 0;

            // output repl conv$13;
            s->out_repl_fst_err = conv_13_fst_err;
            s->out_repl_fst_val = conv_13_fst_val;
            s->out_repl_snd_err = conv_13_snd_err;
            s->out_repl_snd_val = conv_13_snd_val;
        }
    } else {
        // if (Option_isSome# [Int] conv$12) {
        if (conv_12_set) {
            // let flat$4 = unsafe_Option_get# [Int] conv$12;
            int64_t flat_4 = conv_12_val;

            // let conv$13 = pair# [Int] [Int] (error# "Fold1, but there is no value" : Int) flat$4;
            error_t conv_13_fst_err = "Fold1, but there is no value";
            int64_t conv_13_fst_val = 0;
            error_t conv_13_snd_err = 0;
            int64_t conv_13_snd_val = flat_4;

            // output repl conv$13;
            s->out_repl_fst_err = conv_13_fst_err;
            s->out_repl_fst_val = conv_13_fst_val;
            s->out_repl_snd_err = conv_13_snd_err;
            s->out_repl_snd_val = conv_13_snd_val;
        } else {
            // let conv$13 = pair# [Int] [Int] (error# "Fold1, but there is no value" : Int)
            //                                 (error# "Fold1, but there is no value" : Int);
            error_t conv_13_fst_err = "Fold1, but there is no value";
            int64_t conv_13_fst_val = 0;
            error_t conv_13_snd_err = "Fold1, but there is no value";
            int64_t conv_13_snd_val = 0;

            // output repl conv$13;
            s->out_repl_fst_err = conv_13_fst_err;
            s->out_repl_fst_val = conv_13_fst_val;
            s->out_repl_snd_err = conv_13_snd_err;
            s->out_repl_snd_val = conv_13_snd_val;
        }
    }
}
