;; basic tests

(module
  (func (export "add") (param $x i32) (param $y i32) (result i32) (i32.add (local.get $x) (local.get $y)))

  (func (export "elser") (param $x i32) (param $y i32) (result i32)
    (local i32)
    local.get 0
    if
      local.get 1
      local.set 2
    else
      i32.const 7
      local.set 2
    end
    local.get 2
  )
)

(assert_return (invoke "add" (i32.const 1) (i32.const 1)) (i32.const 2))
(assert_return (invoke "elser" (i32.const 1) (i32.const 10)) (i32.const 10))
(assert_return (invoke "elser" (i32.const 0) (i32.const 10)) (i32.const 7))
