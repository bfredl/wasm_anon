;; basic tests

(module
  (func (export "add") (param $x i32) (param $y i32) (result i32) (i32.add (local.get $x) (local.get $y)))
)

(assert_return (invoke "add" (i32.const 1) (i32.const 1)) (i32.const 2))
