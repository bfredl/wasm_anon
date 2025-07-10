;; i32 operation (SINGULAR)

(module
  (func (export "div_s") (param $x i32) (param $y i32) (result i32) (i32.div_s (local.get $x) (local.get $y)))
)

(assert_trap (invoke "div_s" (i32.const 1) (i32.const 0)) "integer divide by zero")
