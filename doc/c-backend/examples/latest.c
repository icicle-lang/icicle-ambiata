#include <stdbool.h>
#include <stdint.h>

typedef int64_t date_t;

// gen$date = DATE
struct icicle_state {
    date_t      gen_date;
    bool        acc_conv_6_set;
    int64_t     acc_conv_6_val;

    int64_t     new_count;
    int64_t    *new_fact;
    date_t     *new_date;

    int64_t     output_repl;

    const char *error;
};

void compute(struct icicle_state *s)
{
    // init [Mutable] [Option Int] acc$conv$6 = None : Option Int;
    bool    acc_conv_6_set = false;
    int64_t acc_conv_6_val = 0;

    // load_resumable [Option Int] acc$conv$6;
    acc_conv_6_set = s->acc_conv_6_set;
    acc_conv_6_val = s->acc_conv_6_val;

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

        // let anf$7 = some# [Int] elem$gen$fact;
        bool    anf_7_set = true;
        int64_t anf_7_val = elem_gen_fact;

        //if (Option_isSome# [Int] acc$conv$6) {
        if (acc_conv_6_set) {
            // let flat$0 = unsafe_Option_get# [Int] acc$conv$6;
            int64_t flat_0 = acc_conv_6_val;

            // write acc$conv$6 = some# [Int] flat$0;
            acc_conv_6_set = true;
            acc_conv_6_val = flat_0;
        } else {
            // write acc$conv$6 = anf$7;
            acc_conv_6_set = anf_7_set;
            acc_conv_6_val = anf_7_val;
        }
    }

    // save_resumable [Option Int] acc$conv$6;
    s->acc_conv_6_set = acc_conv_6_set;
    s->acc_conv_6_val = acc_conv_6_val;

    // read [Mutable] [Option Int] conv$6 = acc$conv$6;
    bool    conv_6_set = acc_conv_6_set;
    int64_t conv_6_val = acc_conv_6_val;

    // if (Option_isSome# [Int] conv$6) {
    if (conv_6_set) {
        // let flat$1 = unsafe_Option_get# [Int] conv$6;
        int64_t flat_1 = conv_6_val;

        // output repl flat$1;
        s->output_repl = flat_1;
    } else {
        // output repl (error# "Fold1, but there is no value" : Int);
        s->error = "repl: Fold1, but there is no value";
    }
}
