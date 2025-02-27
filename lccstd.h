#ifndef __LCC_STD__
#define __LCC_STD__
#define __lcc_is_same_type(a, b)  __builtin_types_compatible_p(typeof(a), typeof(b))
#define __lcc_is_pointer_or_array(p)  (__builtin_classify_type(p) == 5)
#define __lcc_decay(p)  (&*__builtin_choose_expr(__lcc_is_pointer_or_array(p), p, NULL))
#define __lcc_is_pointer(p)  __lcc_is_same_type(p, __lcc_decay(p))
#define __lcc_pointout(v) (__builtin_choose_expr(__lcc_is_pointer(v), v, &v))
#define __lcc_receiver(s, m) typeof(*__lcc_pointout(s))##_info __lcc_ m
#endif // __LCC_STD__
